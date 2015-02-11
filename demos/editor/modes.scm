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
  #:use-module (editor limbs)
  #:use-module (editor control)
  #:use-module (widgets 3d)
  #:use-module (oop goops)
  #:use-module (widgets physics)
  #:export (
	     position<-angles
	     desired-configuration
	     global-positions
	     kinematic-chain
	     set-pose!
	     apply-stops!

	     apply-inverse-kinematics!
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

(define* (set-pose! #;of rig #;to pose #:key (keeping #f))
  (setup-pose! #;of rig #;to pose #:keeping keeping)
  (specify-pose! #;of rig #;to pose))

(define* (setup-pose! #;of rig #;to pose #:key (keeping #f))
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

(define* (apply-stops! #;to rig #:key (keeping #f))
  (let ((pose (clamped-pose #;of rig)))
    (set-pose! #;of rig #;to pose #:keeping keeping)))

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
   (let loop ((input chain)
	      (result '())
	      (translation #f32(0 0 0))
	      (rotation '(1.0 . #f32(0 0 0))))
     (match input
       (()
	(reverse result))
       (((local-anchor local-rotation) . rest)
	(let ((new-anchor (+ translation (rotate local-anchor #;by rotation)))
	      (new-rotation (* rotation local-rotation)))
	  (loop rest (cons (list new-anchor new-rotation) result)
		new-anchor new-rotation)))))))

(define (tip-position chain angles local-position)
  (let loop ((angles angles)
	     (anchors+axes chain)
	     (translation #f32(0 0 0))
	     (rotation '(1.0 . #f32(0 0 0))))
    (match anchors+axes
      (()
       (+ translation (rotate local-position #;by rotation)))
      (((local-anchor local-axis) . remaining-anchors+axes)
       (let* (((alpha . remaining-angles) angles)
	      (q (rotation-quaternion #;around local-axis #;by alpha)))
	 (loop remaining-angles 
	       remaining-anchors+axes
	       (+ translation (rotate local-anchor #;by rotation))
	       (* rotation q)))))))

(define (kinematic-chain anchors axes angles)
  (map (lambda ((anchor axis angle))
	 `(,anchor ,axis))
       (kinematic-chain<-anchors+axes+angles anchors axes angles)))

(define (position<-angles kinematic-chain local-position)
  (let ((N (length kinematic-chain)))
    (impose-arity
     `(,N 0 #f)
     (lambda angles
       (tip-position kinematic-chain angles local-position)))))

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

(publish
 (define* (apply-inverse-kinematics! #;of body #;to desired-position 
					  #:optional #;at (limb? hub?)
					  #;with (max-step 0.02))
   (and-let* ((joint-sequence pivot (joint-sequence-to+nearest-member
				     limb? #;from body))
	      ((> (length joint-sequence) 1))
	      (global-position (body-property body 'position))
	      (((anchors+ axes+ angles+) ...)
	       `(,@(hinge-joint-sequence-anchors+axes+angles joint-sequence)
		 (,global-position #f32(0 0 0) 0.0)))
	      ((kinematic-chain ... (local-position . _)) 
	       (kinematic-chain anchors+ axes+ angles+))
	      (range (apply + (map (lambda ((local-anchor _)) (norm local-anchor))
				   kinematic-chain)))
	      (reachable-position (fit desired-position #;to range
				       #;at (body-property pivot 'position)))
	      (displacement (- reachable-position global-position))
	      (distance (norm displacement))
	      (direction (/ displacement distance))
	      ((axes- ... last-axis _) (map normalized axes+))
	      (system-equation (position<-angles kinematic-chain 
						 local-position)))
     (let improve ((improvement 0)
		   (remaining distance)
		   (angles (drop-right angles+ 1))
		   (current-position global-position))
       (if (positive? remaining)
	   (let* ((improved-position (if (< remaining max-step)
					 reachable-position
					 (+ current-position 
					    (* direction max-step))))
		  ((new-angles- ... _) (desired-configuration current-position
							      improved-position
							      angles 
							      system-equation))
		  (new-angles `(,@new-angles- 
				,(apply - 0.0 (map (lambda (angle axis)
						     (* angle 
							(* axis last-axis)))
						   new-angles- axes-))))
		  (new-pose `(pose ,@(map (lambda (joint new-angle)
					    `(,(joint-name joint) . ,new-angle))
					  joint-sequence new-angles))))
	     (unless (or (> improvement 5) (exists delta in new-angles
					     (> (abs delta) 1.5)))
	       (set-pose! #;of (body-rig body) #;to new-pose #:keeping pivot)
	       (improve (+ improvement 1) (- remaining max-step)
			(map (lambda (joint) (joint-property joint 'angle))
			     joint-sequence)
			(body-property body 'position))))))))
 where
 (define (fit position #;to range #;at pivot)
   (let* ((local (- position pivot))
	  (distance (norm local))
	  (direction (/ local distance)))
     (and (>= range distance)
	  (+ pivot (* (min range distance) direction))))))

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
	       #;of final-body #;to (screen->3d view x y zs)
		    #;at hub?))))))))))

(define-method (select-body! body #;from (view <3d-view>))
  (let ((object (find (lambda (x)
			(and (is-a? x <physical-object>) 
			     (equal? #[x 'body] body)))
		      #[view : 'stage : 'objects])))
    (if object
	(select-object! object #;from view)
	(format #t "no body found in ~s\n" #[view : 'stage : 'objects]))))

(define (possibly-add-neighbours-for-chest/abdomen/pelvis! #;in view)
  (define-syntax-rule (add-neighbours! object (source targets ...) ...)
    (let* ((body #[object 'body])
	   (rig (body-rig body)))
      (match* (body-name body)
	('source
	 (let ((targets (body-object (body-named 'targets #;from rig)
				     #;from view))
	       ...)
	   (unselect-all! #;from view)
	   (select-object! targets #;from view)
	   ...
	   (select-object! object #;from view)))
	...)))
  (match* #[view 'selected]
    ((object)
     (add-neighbours! object 
		      (chest abdomen)
		      (abdomen pelvis)
		      (pelvis left-hip right-hip)))))

(define ((rotate-around-joint-mode view))
  (define ((restore! previously-selected) . _)
    (unselect-all! #;from view)
    (for object in (reverse previously-selected)
      (select-object! object #;from view)))
  (with-context-for-joint/body-relation
   (possibly-add-neighbours-for-chest/abdomen/pelvis! #;in view)
   (match (map #[_ 'body] #[view 'selected])
     ((and (first second . rest) (? (lambda ((body . bodies))
				      (for-every x in bodies
					(bodies-are-connected? x body)))))
      (let ((joint (joint-connecting-bodies first second))
	    (held (map (lambda (body)
			 (let* ((joint (joint-connecting-bodies first body))
				(left right (split-bodies-at joint)))
			   (if (in? first left)
			       right
			       left)))
		       rest))
	    (previously-selected #[view 'selected]))
	(let* ((left right (split-bodies-at joint))
	       (move+ still- (if (in? first left)
				 (values left right)
				 (values right left)))
	       (move still (values (apply difference move+ held)
				   (apply union still- held))))
	  (unselect-all! #;from view)
	  (for body in move
	    (select-body! body #;from view))
	  (let ((axis (joint-property joint 'axis))
		(center (joint-property joint 'anchor)))
	    ((rotate-mode 
	      view
	      #:axis axis
	      #:center (lambda _ center)
	      #:always-rotate-around-center #t
	      #:on-exit (restore! previously-selected)))))))
     ((and (first . rest) bodies
	   (? (lambda ((first . rest))
		(and (let ((rig (body-rig first)))
		       (for-every x in rest
			 (eq? (body-rig x) rig)))
		     (exists x in rest
		       (not (bodies-are-connected? first x)))))))
      (let* ((previously-selected #[view 'selected])
	     (rig (body-rig first))
	     (bodies (rig-bodies rig))
	     (center (rig-mass-center rig)))
	(unselect-all! #;from view)
	(for body in bodies
	  (select-body! body #;in view))
	((rotate-mode 
	  view 
	  #:center (lambda _ center)
	  #:always-rotate-around-center #t
	  #:on-exit (restore! previously-selected)))))
     (_
      (display "at least two connected bodies need to be selected\n")))))
