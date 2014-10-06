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
	     (oop goops)
	     (widgets base)
	     (widgets physics)
	     (widgets 3d)
	     (editor relations)
	     (editor poses)
	     (scum physics))

(set-window-title! "POSED: The POSE Editor")

(define the-simulation (primitive-make-simulation))

(set-simulation-property! the-simulation 'gravity #f32(0 0 0))

(define physical-objects (make <physics-stage> #:simulation the-simulation))

(define-rig-for the-simulation 
  'legs (with-input-from-file "art/rigs/legs.rig" read))

;; achtung! obsługujemy tylko hinge (a niebawem będziemy musieli wprowadzić
;; również hinge-2, ale inne -- dopiero później)

(define view (make <3d-editor>
	       #:x  10 #:y  10 
	       #:w 620 #:h 460
	       #:stage physical-objects))

(add-child! view #;to *stage*)

(define-syntax-rule (with-context-for-joint/body-relation action . *)
  (specify ((joint-property-getter joint-property)
	    (body-rig-getter body-rig)
	    (rig-joints-getter rig-joints))
    action . *))

(define the-legs (make-rig the-simulation 'legs #:position #f32(0 0 0)
			  #:orientation '(0.707 . #f32(-0.707 0 0))))

(define rig-angles
  (let ((rig-angles (make-hash-table)))
    (match-let ((('pose . parameters) (null-pose #;of the-legs)))
      (for (name . angle) in parameters
	(hash-set! rig-angles name angle)))
    rig-angles))

(define (current-pose)
  `(pose ,@(hash-map->list cons rig-angles)))

(define (joint-angles rig)
  `(pose ,@(map (lambda(joint)
		  `(,(joint-name joint) . ,(joint-property joint 'angle)))
		(rig-joints rig))))

(define (rotate-body! body #;by angle #;around axis #;at pivot)
  (let ((position (body-property body 'position))
	(orientation (body-property body 'quaternion)))
    (set-body-property! body 'position
			(rotate position #;by angle #;around axis
				#;at pivot))
    (set-body-property! body 'quaternion
			(* (rotation-quaternion #;around axis
							 #;by angle)
			   orientation))))

(define* (set-pose! #;of rig #;to pose 
			 #:key (keeping (match pose (('pose (first . _) . _)
						     first))))
  (assert (pose? pose))
  (with-context-for-joint/body-relation
   (match pose
     (('pose . parameters)
      (for (joint-name . angle) in parameters
	(when angle
	  (let*-values (((joint) (joint-named joint-name #;from rig))
			((mobile-bodies immobile-bodies)
			 (split-bodies-at joint))
			((angle*) (- angle #[rig-angles joint-name]))
			((mobile-joints) 
			 (unique (fold union '()
				       (map joints-attached-to
					    mobile-bodies))))
			((pivot axis) 
			 (begin
			   (assert (eq? (joint-type joint) 'hinge))
			   (values (joint-property joint 'anchor)
				   (joint-property joint 'axis)))))
	    (set! #[rig-angles joint-name] angle)
	    (for body in mobile-bodies
	      (rotate-body! body #;by angle* #;around axis #;at pivot)))))))))

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

(keydn 'g (grab-mode view))

(define pause #f)
(keydn 'p (lambda () (set! pause (not pause))))

(define ((rotate-around-joint/mode view))
  (with-context-for-joint/body-relation
   (match (map #[_ 'body] #[view 'selected])
     ((first second)
      (let ((joint (joint-connecting-bodies first second))
	    (previously-selected #[view 'selected]))
	(let*-values (((left right) (split-bodies-at joint))
		      ((move still) (cond ((in? first left)
					   (values left right))
					  ((in? first right)
					   (values right left))
					  (else
					   (error)))))
	  (unselect-all! #;from view)
	  (for body in move
	    (select-body! body #;from view))
	  (let ((old-pause pause))
	    (set! pause #t)
	    ((rotate-mode 
	      view
	      #:axis (joint-property joint 'axis)
	      #:center (lambda _ (joint-property joint 'anchor))
	      #:always-rotate-around-center #t
	      #:on-exit (lambda (angle)
			  (increase! #[rig-angles (joint-name joint)] angle)
			  (unselect-all! #;from view)
			  (for object in (reverse previously-selected)
			    (select-object! object #;from view))
			  (set! pause old-pause))))
	    ))))
     (else
      (display "exactly two objects need to be selected\n")))))

(keydn 'h (rotate-around-joint/mode view))

(keydn 'c (lambda ()
	    (pretty-print (current-pose))))

(keydn 'b (lambda ()
	    (pretty-print (joint-angles the-legs))))

(keydn 'v (lambda ()
	    (set-pose! #;of the-legs #;to (current-pose))))

(add-timer! 
 25 
 (lambda()
   (unless pause
     (make-simulation-step! the-simulation)
   )))

(load "config.scm")
