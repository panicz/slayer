#!../src/slayer -e3d
exit
!#

(set! %load-path (append '("." "./guile-modules" ".." "../guile-modules"
			   "./scum")
			 %load-path))

(use-modules (extra common)
	     (extra math)
	     (extra slayer)
	     (extra ref)
	     (extra 3d)
	     (oop goops)
	     (widgets base)
	     (widgets physics)
	     (widgets 3d)
	     (editor relations)
	     (editor modes)
	     (editor poses)
	     (extra scmutils)
	     (scum physics))

(define-syntax-rule (with-context-for-joint/body-relation action . *)
  (specify ((joint-property-getter joint-property)
	    (body-rig-getter body-rig)
	    (rig-joints-getter rig-joints))
    action . *))

(set-window-title! "POSED: The POSE Editor")

(define the-simulation (primitive-make-simulation))

(set-simulation-property! the-simulation 'gravity #f32(0 0 0))

(define physical-objects (make <physics-stage> #:simulation the-simulation))

(define-rig-for the-simulation 
  'legs (with-input-from-file "art/rigs/legs.rig" read))

(define view (make <3d-editor>
	       #:x  0 #:y  0 
	       #:w 640 #:h 480
	       #:stage physical-objects))

(add-child! view #;to *stage*)

;;(load "posed-trash.scm")

(define the-number-of-joints 4)

(keydn "["
  (lambda () 
    (if the-number-of-joints
	(set! the-number-of-joints 
	  (let ((n (- the-number-of-joints 1)))
	    (if (negative? n)
		#f
		n))))
    (<< the-number-of-joints)))

(keydn "]" 
  (lambda () 
    (set! the-number-of-joints
      (if the-number-of-joints
	  (+ the-number-of-joints 1)
	  0))
    (<< the-number-of-joints)))

(define the-legs (make-rig the-simulation 'legs #:position #f32(0 0 0)
			  #:orientation '(0.707 . #f32(-0.707 0 0))))

(define rig-angles
  (let ((rig-angles (make-hash-table))
	(('pose . parameters) (null-pose #;of the-legs)))
    (for (name . angle) in parameters
      (hash-set! rig-angles name angle))
    rig-angles))

(define (current-pose)
  "the desired pose currently set up"
  `(pose ,@(hash-map->list cons rig-angles)))

(define (current-configuration)
  `(pose ,@(map (lambda (joint)
		  `(,(joint-name joint) . ,(joint-property joint 'angle)))
		(rig-joints the-legs))))

(define ((rig-joints-name&properties-getter . properties) rig)
  "the properties are obtained directly from ODE simulator"
  (map (lambda (joint) 
	 `(,(joint-name joint)
	   ,@(map (lambda (property) 
		    (joint-property joint property))
		  properties)))
       (rig-joints rig)))

(define joint-angles (rig-joints-name&properties-getter 'angle))

(define joint-axes (rig-joints-name&properties-getter 'axis))

(define joint-anchors (rig-joints-name&properties-getter 'anchor))

(define (rotate-body! body #;by angle #;around axis #;at pivot)
  (let ((position (body-property body 'position))
	(orientation (body-property body 'quaternion)))
    (set-body-property! body 'position
			(rotate position #;by angle #;around axis
				#;at pivot))
    (set-body-property! body 'quaternion
			(* (rotation-quaternion #;around axis #;by angle)
			   orientation))))

(define new-pose #f)

(define* (set-pose! #;of rig #;to pose #:key (keeping #f))
  (assert (and (pose? pose)
	       (if keeping (body? keeping))))
  (with-context-for-joint/body-relation
   (set! new-pose #t)
   (let ((('pose . parameters) pose))
     (for (joint-name . angle) in parameters
       (when angle
	 (let* ((joint (joint-named joint-name #;from rig))
		(mobile-bodies immobile-bodies direction
			       (let ((left right (split-bodies-at joint)))
				 (cond ((and keeping (in? keeping left))
					(values right left -1))
				       ((or (not keeping) (in? keeping right))
					(values left right +1))
				       (else
					(error)))))
		(angle* (- angle (joint-property joint 'angle)))
		(mobile-joints (unique (fold union '()
					     (map joints-attached-to
						  mobile-bodies))))
		(axis pivot (values (* direction (joint-property joint 'axis))
				    (joint-property joint 'anchor))))
	   (assert (eq? (joint-type joint) 'hinge))
	   (set! #[rig-angles joint-name] angle)
	   (for body in mobile-bodies
	     (rotate-body! body #;by angle* #;around axis #;at pivot))))))))

(set-pose! the-legs (current-configuration))

(include "temporary-poses.scm")

(set! #[view 'left-click]
      (lambda (x y)
	(let ((object (object-at-position x y view)))
	  (when object
	    (select-object! object #;from view)))))

(keydn 'esc (lambda () (unselect-all! view)))

(define-method (select-body! body #;from (view <3d-view>))
  (let ((object (find (lambda(x)
			(and (is-a? x <physical-object>) 
			     (equal? #[x 'body] body)))
		      #[view : 'stage : 'objects])))
    (if object
	(select-object! object #;from view)
	(format #t "no body found in ~s\n" #[view : 'stage : 'objects]))))

(define* (apply-inverse-kinematics! #;of selected-body #;to position 
					#:optional #;anchored-at (fixed #f))
  (let* ((joint-sequence+ fixed
			  (if fixed 
			      (values (shortest-joint-sequence
				       #;from fixed #;to selected-body)
				      fixed)
			      (shortest-joint-sequence-from+furthest-end
			       #;to selected-body)))
	 (joint-sequence (if the-number-of-joints
			     (take-right joint-sequence+ the-number-of-joints)
			     joint-sequence+))
	 (global-position (body-property selected-body 'position))

	 (((anchors+ axes+ angles+) ...)
	  `(,@(hinge-joint-sequence-anchors+axes+angles joint-sequence)
	    (,global-position #f32(0 0 0) 0.0)))

	 ((kinematic-chain ... (local-position . _)) (kinematic-chain 
						      anchors+ axes+ angles+))
	 (system-equation (position<-angles kinematic-chain local-position))

	 ((angles ... _) angles+)

	 ((new-angles- ... _) (desired-configuration global-position position 
						     angles system-equation))
	 ((axes- ... last-axis _) (map normalized axes+))

	 (new-angles `(,@new-angles- 
		       ,(apply - 0.0 (map (lambda (angle axis)
					    (* angle (* axis last-axis)))
					  new-angles- axes-))))
	 (new-pose `(pose ,@(map (lambda (joint new-angle)
				   `(,(joint-name joint) . ,new-angle))
				 joint-sequence new-angles))))

    (assert (and (apply eq? 'hinge (map joint-type joint-sequence))
		 (= global-body-position (apply system-equation angles))))
    (if (every finite? angles)
	(set-pose! #;of the-legs #;to new-pose #:keeping fixed)
	#;else
	(pretty-print `(inverse-kinematics-singularity: ,angles)))))

(define ((ik-mode view))
  (with-context-for-joint/body-relation
   (unless (null? #[view 'selected])
     (let* ((old-bindings (current-key-bindings))
	    (final-object (first #[view 'selected]))
	    (final-body #[final-object 'body])
	    (initial-position #[final-object 'position])
	    (_ fixed (shortest-joint-sequence-from+furthest-end 
		      #;to final-body))
	    (pose (current-pose))
	    ((xs ys zs) (3d->screen view initial-position)))
       (set-mouse-position! xs ys)
       (set-key-bindings!
	(key-bindings
	 (keydn 'esc
	   (lambda ()
	     (set-pose! #;of the-legs #;to pose #:keeping fixed)
	     (set-key-bindings! old-bindings)))
	 (keydn 'mouse-left
	   (lambda (x y)
	     (set-key-bindings! old-bindings)))
	 (mousemove
	  (lambda (x y xrel yrel)
	    (with-context-for-joint/body-relation
	     (apply-inverse-kinematics!
	      #;of final-body #;to (screen->3d view x y zs)))))))))))

(keydn 'k (ik-mode view))

(keydn 'g (grab-mode view))

(define pause #t)

(keydn 'p 
  (lambda () 
    (set! pause (not pause))
    (format #t "pause ~a\n" (if pause 'on 'off))))

(define ((rotate-around-joint-mode view))
  (with-context-for-joint/body-relation
   (match (map #[_ 'body] #[view 'selected])
     ((first second)
      (let ((joint (joint-connecting-bodies first second))
	    (previously-selected #[view 'selected]))
	(let* ((direction (if (eq? first (head (two-bodies-attached-by joint)))
			      +1
			      -1))
	       (left right (split-bodies-at joint))
	       (move still (cond ((in? first left)
				  (values left right))
				 ((in? first right)
				  (values right left))
				 (else
				  (error)))))
	  (unselect-all! #;from view)
	  (for body in move
	    (select-body! body #;from view))
	  (let ((old-pause pause)
		(axis (joint-property joint 'axis))
		(center (joint-property joint 'anchor)))
	    (set! pause #t)
	    ((rotate-mode 
	      view
	      #:axis (* direction axis)
	      #:center (lambda _ center)
	      #:always-rotate-around-center #t
	      #:rotation-direction (- direction)
	      #:on-exit (lambda (angle)
			  (increase! #[rig-angles (joint-name joint)] angle)
			  (unselect-all! #;from view)
			  (for object in (reverse previously-selected)
			    (select-object! object #;from view))
			  (set! pause old-pause))))
	    ))))
     (else
      (display "exactly two objects need to be selected\n")))))

(define-syntax-rule (gimme shit)
  (lambda () (pretty-print shit #:width 160)))

(keydn 'h (rotate-around-joint-mode view))

(keydn 'c (gimme (current-pose)))

(keydn 'b (gimme (joint-angles the-legs)))

(keydn 'x (gimme (joint-axes the-legs)))

(keydn 'z (gimme (joint-anchors the-legs)))


(keydn 'v (lambda ()
	    (set-pose! #;of the-legs #;to 
			    (if (modifier-pressed? 'shift)
				(current-configuration)
				(current-pose)))))


(add-timer! 
 25 
 (lambda()
   (unless pause
     (make-simulation-step! the-simulation)
   )))

(load "config.scm")
