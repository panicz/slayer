(define-module (editor modes)
  #:use-module (slayer)
  #:use-module (extra slayer)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (extra 3d)
  #:use-module (scum physics)
  #:use-module (editor relations)
  #:use-module (editor poses)
  #:use-module (widgets 3d)
  #:use-module (oop goops)
  #:use-module (widgets physics)
  #:export (
	     position<-angles
	     desired-configuration
	     global-positions
	     tip-position
	     kinematic-chain
	     set-pose!
	     pose

	     rotate-around-joint-mode
	     ik-mode

	    )
  #:export-syntax (
		   with-context-for-joint/body-relation
		   ))


(define-syntax-rule (with-context-for-joint/body-relation action . *)
  (specify ((joint-property-getter joint-property)
	    (body-rig-getter body-rig)
	    (rig-joints-getter rig-joints))
    action . *))

(define (rotate-body! body #;by angle #;around axis #;at pivot)
  (let ((position (body-property body 'position))
	(orientation (body-property body 'quaternion)))
    (set-body-property! body 'position
			(rotate position #;by angle #;around axis
				#;at pivot))
    (set-body-property! body 'quaternion
			(* (rotation-quaternion #;around axis #;by angle)
			   orientation))))

(define (pose #;of rig)
  `(pose ,@(map (lambda (joint)
		  `(,(joint-name joint) . ,(joint-property joint 'angle)))
		(rig-joints rig))))

(define* (set-pose! #;of rig #;to pose #:key (keeping #f))
  (assert (and (pose? pose)
	       (if keeping (body? keeping))))
  (with-context-for-joint/body-relation
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
	   (for body in mobile-bodies
	     (rotate-body! body #;by angle* #;around axis #;at pivot))))))))

(publish
 (define (kinematic-chain<-anchors+axes+angles anchors axes angles)
   (kinematic-chain (zip anchors axes angles)))
 where
 (define (kinematic-chain global-sequence)
   (match global-sequence
     (()
      '())
     (((anchor axis angle) . rest)
      `((,anchor ,axis ,angle)
	,@(kinematic-chain
	   (map (lambda ((local-anchor local-axis local-angle))
		  `(,(rotate (- local-anchor anchor)
			     #;by (- angle) #;around axis)
		    ,(rotate local-axis #;by (- angle) #;around axis)
		    ,local-angle))
		rest)))))))

(publish
 (define (global-positions kinematic-chain angles)
   (map (lambda ((anchor rotation)) 
	  anchor)
	(anchors+orientations<-kinematic-chain 
	 (map (lambda ((anchor axis) angle)
		`(,anchor
		  ,(rotation-quaternion
		    #;around axis
			     #;by angle)))
	      kinematic-chain angles))))
 where
 (define (anchors+orientations<-kinematic-chain chain)
   (global<-local chain #f32(0 0 0) '(1.0 . #f32(0 0 0))))

 (define (global<-local kinematic-chain translation rotation)
   (match kinematic-chain
     (()
      '())
     (((local-anchor local-rotation) . rest)
      (let ((new-anchor (+ translation (rotate local-anchor #;by rotation)))
	    (new-rotation (* rotation local-rotation)
			  ))
      `((,new-anchor ,new-rotation) . ,(global<-local rest new-anchor
						      new-rotation)))))))

;; co musimy przetestować? jakie ten nasz kod czyni założenia?
;; - mamy daną funkcję, która pobiera kąty i zwraca położenie
;; elementu końcowego. W jakim układzie współrzędnych wyrażone
;; jest to położenie?
;; - skąd bierze się taka jednorodność jakobianu? czy to aby
;; nie podejrzane?

(define (kinematic-chain anchors axes angles)
  (map (lambda ((anchor axis angle))
	 `(,anchor ,axis))
       (kinematic-chain<-anchors+axes+angles anchors axes angles)))

(define (tip-position kinematic-chain angles)
  (last (global-positions kinematic-chain angles)))

(define (position<-angles kinematic-chain local-position)
  (let ((N (length kinematic-chain)))
    (impose-arity
     `(,N 0 #f)
     (lambda angles
       (tip-position 
	`(,@kinematic-chain
	  (,local-position #f32(0 0 0)))
	`(,@angles 0.0))))))

(define (desired-configuration initial-position desired-position
			       initial-configuration system-equation)
  ;; inverse kinematics routine.
  ;; initial and desired positions are expressed in global coordinate system.
  ;; 
  (assert (and (uniform-vector? initial-position)
	       (uniform-vector? desired-position)
	       (list? initial-configuration)
	       (list? (desired-configuration 
		       initial-position desired-position 
		       initial-configuration system-equation))))
  (let* ((position-increment (- desired-position initial-position))
	 (jacobian (apply ((isotropic-jacobian-approximation 
			    #;of (compose uniform-vector->list 
					  system-equation))
			   #;by 0.00001)
			  #;at initial-configuration))
	 (jacobian+ (pseudoinverse #;of jacobian))
	 (angle-increment (uniform-vector->list 
			   (* jacobian+ position-increment)))
	 (result (map normalized-radians 
		      (map + initial-configuration angle-increment))))
    (assert (and (list? jacobian) (every list? jacobian)
		 (array? jacobian+) (in? (array-type jacobian+) '(f32 f64))))
    result))

(define the-number-of-joints 6)

(define* (apply-inverse-kinematics! #;of selected-body #;to rig #;into position
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
	(set-pose! #;of rig #;to new-pose #:keeping fixed)
	#;else
	(display `(inverse-kinematics-singularity: ,angles)))))

(define ((ik-mode view rig))
  (with-context-for-joint/body-relation
   (unless (null? #[view 'selected])
     (let* ((old-bindings (current-key-bindings))
	    (final-object (first #[view 'selected]))
	    (final-body #[final-object 'body])
	    (initial-position #[final-object 'position])
	    (_ fixed (shortest-joint-sequence-from+furthest-end 
		      #;to final-body))
	    (pose (pose #;of rig))
	    ((xs ys zs) (3d->screen view initial-position)))
       (set-mouse-position! xs ys)
       (set-key-bindings!
	(key-bindings
	 (keydn 'esc
	   (lambda ()
	     (set-pose! #;of rig #;to pose #:keeping fixed)
	     (set-key-bindings! old-bindings)))
	 (keydn 'mouse-left
	   (lambda (x y)
	     (set-key-bindings! old-bindings)))
	 (mousemove
	  (lambda (x y xrel yrel)
	    (with-context-for-joint/body-relation
	     (apply-inverse-kinematics!
	      #;of final-body #;to rig 
		   #;into (screen->3d view x y zs)))))))))))

(define-method (select-body! body #;from (view <3d-view>))
  (let ((object (find (lambda (x)
			(and (is-a? x <physical-object>) 
			     (equal? #[x 'body] body)))
		      #[view : 'stage : 'objects])))
    (if object
	(select-object! object #;from view)
	(format #t "no body found in ~s\n" #[view : 'stage : 'objects]))))

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
			  #;(increase! #[rig-angles (joint-name joint)] angle)
			  (unselect-all! #;from view)
			  (for object in (reverse previously-selected)
			    (select-object! object #;from view))
			  (set! pause old-pause))))
	    ))))
     (else
      (display "exactly two objects need to be selected\n")))))
