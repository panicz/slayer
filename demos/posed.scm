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
	     (editor poses)
	     (extra scmutils)
	     (scum physics))

(define-syntax-rule (with-context-for-joint/body-relation action . *)
  (specify ((joint-property-getter joint-property)
	    (body-rig-getter body-rig)
	    (rig-joints-getter rig-joints))
    action . *))


(set-window-title! "POSED: The POSE Editor")

(set-point-size! 10.0)
(set-line-width! 5.0)

(define the-simulation (primitive-make-simulation))

(set-simulation-property! the-simulation 'gravity #f32(0 0 0))

(define physical-objects (make <physics-stage> #:simulation the-simulation))

(define-rig-for the-simulation 
  'legs (with-input-from-file "art/rigs/legs.rig" read))

;; achtung! obsługujemy tylko hinge (a niebawem będziemy musieli wprowadzić
;; również hinge-2, ale inne -- dopiero później)

(define view (make <3d-editor>
	       #:x  0 #:y  0 
	       #:w 320 #:h 480
	       #:stage physical-objects))

(add-child! view #;to *stage*)

(define ((joint... . properties) joint)
  (match properties
    ((property)
     (joint-property joint property))
    (else
     (map (lambda (property) (joint-property joint property)) properties))))

(define ((body... . properties) body)
  (match properties
    ((property)
     (body-property joint property))
    (else
     (map (lambda (property) (body-property body property)) properties))))

;; red green blue cyan magenta yellow white

(define *color-sequence*
  #2f32((0 0 1) ; blue
	(0 1 0) ; green
	(0 1 1) ; cyan
	(1 0 0) ; red
	(1 0 1) ; magenta
	(1 1 0) ; yellow
	(1 1 1) ; white
	(1 0 0) ; red
	(1 1 1) ; white
	(1 0 0) ; red
	(1 1 1) ; white
	(1 0 0) (1 0 0) (1 0 0) (1 0 0) (1 0 0) (1 0 0) (1 0 0) (1 0 0)	(1 0 0) 
	(1 0 0) (1 0 0) (1 0 0) (1 0 0) (1 0 0) (1 0 0) (1 0 0)	(1 0 0) (1 0 0)
	(1 0 0) (1 0 0) (1 0 0) (1 0 0) (1 0 0) (1 0 0)	(1 0 0) (1 0 0) (1 0 0)))

(define the-current-joint 0)

(define the-angle-increment 0.001)

(keydn '-
  (lambda ()
    (set! the-angle-increment (if (modifier-pressed? 'shift)
				  (- the-angle-increment 
				     (* 0.1 the-angle-increment))
				  (/ the-angle-increment 2.0)))
    (<< the-angle-increment)))

(keydn '=
  (lambda ()
    (set! the-angle-increment (if (modifier-pressed? 'shift)
				  (+ the-angle-increment
				     (* 0.1 the-angle-increment))
				  (* the-angle-increment 2.0)))
    (<< the-angle-increment)))

(keydn 'backspace
  (lambda ()
    (set! the-angle-increment (- the-angle-increment))))

(for i in (1 .. 8)
  (keydn (string-append "f" (number->string i))
    (lambda ()
      (set! the-current-joint (- i 1)))))

(keydn 'f10 (lambda () (set! the-current-joint #f)))

(keydn 'f11 (lambda () (set! the-current-joint #t)))

(define v #2f32((0 0 0)))

(keydn 'i (lambda () (set! v (+ v #2f32((0.01 0 0))))))

(keydn 'k (lambda () (set! v (- v #2f32((0.01 0 0))))))

(keydn 'j (lambda () (set! v (+ v #2f32((0 0.01 0))))))

(keydn 'l (lambda () (set! v (- v #2f32((0 0.01 0))))))

(keydn "[" (lambda () (set! v (+ v #2f32((0 0 0.01))))))

(keydn "]" (lambda () (set! v (- v #2f32((0 0 0.01))))))

#| Przypomnienie z OpenGLa:

 glTranslate(...); 
 glRotate(...); 
 draw_object(); // TRv -- przesuń obrócone
 // w innym przypadku: RTv -- obróć przesunięte

(define (deriv f)
  (assert (type f (real? -> real?) -> (real? -> real?)))
  (lim (x -> +0.0) 

|#

(define (mesh<-kinematic-chain chain)
  (define (kinematic-chain->mesh chain)
    (match chain
      (()
       '())
      (((translation rotation) . rest)
       `((with-transforms ((translate-view! ,translation)
			   (rotate-view! ,rotation))
			  ,@(match rest
			      (()
			       `((vertices ,v)
				 (color #f32(1 1 1))
				 (faces
				  (points #u8(0)))))
			      (((t r) . _)
			       `((vertices ,(list->uniform-array
					     `((0.0 0.0 0.0)
					       ,(uniform-vector->list t))))
				 (faces
				  (points #u8(0))
				  (lines #u8(0 1))
				  ))))
			  ,@(kinematic-chain->mesh rest))))))
  `(mesh (color #f32(1 0 0)) ,@(kinematic-chain->mesh chain)))

(publish
 (define ((custom-mesh context) 3d-chain-proxy)
   (with-context-for-joint/body-relation
    (and-let* ((target-view #[3d-chain-proxy 'target])
	       (selected #[target-view 'selected]))
      (when (changed? `(,selected ,the-current-joint ,the-angle-increment ,v))
	(match selected
	  (()
	   (set! kinematic-chain-mesh '(mesh))
	   (set! global-skeleton-mesh '(mesh)))
	  ((first . rest)
	   (let* ((body #[first 'body])
		  (joints end directions
			  (shortest-joint-sequence-from+furthest-end #;to body))
		  (N (length joints))
		  (((anchors axes angles) ...)
		   (map (joint... 'anchor 'axis 'angle) joints))
		  (angles (map * angles directions))
		  (kinematic-chain* (kinematic-chain<-anchors+axes+angles
				    anchors axes angles))
		  (angles (if the-current-joint
			      (if (number? the-current-joint)
				  (if (< the-current-joint N)
				      (alter the-current-joint #;in angles
					     #;with (+ #[angles the-current-joint]
						       the-angle-increment))
				      angles)
				  (make-list N the-angle-increment))
			      (make-list N 0.0)))

		  (kinematic-chain (map (lambda ((anchor axis _) angle)
					  `(,anchor ,(rotation-quaternion
						      #;around axis
							       #;by angle)))
					kinematic-chain* angles))
		  (translation rotation (kinematic-chain-substitute 
					 kinematic-chain))
		  (local-position #f32(0 0 0)
				  #;(rotate (- (body-property body 'position)
					     translation)
					  #;by (~ rotation))
				  )
		  (((anchors orientations) ...)
		   (anchors+orientations<-kinematic-chain kinematic-chain))
		  (system-equation (position<-angles (zip anchors axes)
						     local-position))
		  (global-position (apply system-equation angles))
		  (vertices (list->uniform-array 
			     (map uniform-vector->list
				  `(,@anchors
				    ,@(map + anchors (map * angles axes))
				    #;,global-position))))
		  (anchor-indices (list->uniform-array `(,@(iota N) #;,(* 2 N))))
		  (axis-indices (list->uniform-array (zip (iota N) (iota N N))))
		  )
	     (set! kinematic-chain-mesh (mesh<-kinematic-chain kinematic-chain))
	     (set! global-skeleton-mesh
	       `(mesh
		 (vertices ,vertices)
		 (colors ,*color-sequence*)
		 (faces
		  (points ,anchor-indices)
		  (line-strip ,anchor-indices)
		  (lines ,axis-indices))))))))
      (case context 
	((kinematic-chain)
	 kinematic-chain-mesh)
	((global-skeleton)
	 global-skeleton-mesh)
	(else 
	 (error "unknown context" context))))))
 where
 (define kinematic-chain-mesh '(mesh))
 (define global-skeleton-mesh '(mesh)))

(define-class <3d-chain-proxy> (<3d-object>)
  (target #:init-value #f #:init-keyword #:target)
  (mesh-getter #:init-value (lambda (self) '(mesh)) #:init-keyword #:mesh-getter)
  (mesh #:allocation #:virtual 
	#:slot-ref (lambda (self)
		     (#[self 'mesh-getter] self))
	#:slot-set! noop))

(define view-1 (make <3d-view>
		 #:x 320 #:y 0
		 #:w 320 #:h 240
		 #:stage (make <3d-stage>)
		 #:camera (make <3d-cam-clone>
			    #:original #[view 'camera])))

(add-object! (make <3d-chain-proxy> #:target view
		   #:mesh-getter (custom-mesh 'kinematic-chain))
	     #;to #[view-1 'stage])

(add-child! view-1 #;to *stage*)

(define view-2 (make <3d-view>
		 #:x 320 #:y 240
		 #:w 320 #:h 240
		 #:stage (make <3d-stage>)
		 #:camera (make <3d-cam-clone>
			    #:original #[view 'camera])))

(add-object! (make <3d-chain-proxy> #:target view
		   #:mesh-getter (custom-mesh 'global-skeleton))
	     #;to #[view-2 'stage])

(add-child! view-2 #;to *stage*)

(define the-legs (make-rig the-simulation 'legs #:position #f32(0 0 0)
			  #:orientation '(0.707 . #f32(-0.707 0 0))))

(define rig-angles
  (let ((rig-angles (make-hash-table)))
    (match-let ((('pose . parameters) (null-pose #;of the-legs)))
      (for (name . angle) in parameters
	(hash-set! rig-angles name angle)))
    rig-angles))

(define (current-pose)
  "the desired pose currently set up"
  `(pose ,@(hash-map->list cons rig-angles)))

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

(define* (set-pose! #;of rig #;to pose #:key (keeping #f))
  (assert (and (pose? pose)
	       (if keeping (body? keeping))))
  (with-context-for-joint/body-relation
   (let ((('pose . parameters) pose))
     (for (joint-name . angle) in parameters
       (when angle
	 (let* ((joint (joint-named joint-name #;from rig))
		(mobile-bodies immobile-bodies 
			       (let ((left right (split-bodies-at joint)))
				 (if (and keeping (in? keeping left))
				     (values right left)
				     (values left right))))
		(angle* (- angle (joint-property joint 'angle)))
		(mobile-joints (unique (fold union '()
					     (map joints-attached-to
						  mobile-bodies))))
		(pivot axis (begin
			      (assert (eq? (joint-type joint) 'hinge))
			      (values (joint-property joint 'anchor)
				      (joint-property joint 'axis)))))
	   (set! #[rig-angles joint-name] angle)
	   (for body in mobile-bodies
	     (rotate-body! body #;by angle* #;around axis #;at pivot))))))))

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

#;(keydn 'i
  (lambda ()
    (with-context-for-joint/body-relation    
     (unless (null? #[view 'selected])
       (let* ((body #[(element #[view 'selected]) 'body])
	      (joint-sequence
	       (shortest-joint-sequence-from+furthest-end #;to body)))
	 (pretty-print (map joint-name joint-sequence)))))))

#;(keydn 'l
  (lambda ()
    (with-context-for-joint/body-relation
     (unless (null? #[view 'selected])
       (let* ((body #[(element #[view 'selected]) 'body])
	      (joint-sequence
	       fixed
	       (shortest-joint-sequence-from+furthest-end #;to body))
	      (configuration
	       (map (lambda (joint) 
		      `(,(joint-name joint) . ,(joint-property joint 'angle)))
		    joint-sequence))
	      )
	 (set-pose! 
	  #;of the-legs 
	       #;to `(pose ,@(append-map 
			      (lambda ((name . angle))
				(if (eq? name 'middle)
				    '()
				    `((,name 
				       . ,(+ angle 
					     (if (symbol-match "^left" name)
						 0.1
						 -0.1))))))
			      configuration))
		    #:keeping fixed)
	 )))))

(define* (apply-inverse-kinematics! #;of selected-body #;to position 
					#:optional #;anchored-at (fixed #f))
  (let* ((joint-sequence fixed directions
			 (if fixed
			     (let ((sequence directions
					     (shortest-joint-sequence 
					      #;from fixed #;to selected-body)))
			       (values sequence fixed directions))
			     (shortest-joint-sequence-from+furthest-end
			      #;to selected-body)))
	 (((anchors angles axes) ...)
	  (map (lambda (joint) (map (lambda (property)
				 (joint-property joint property))
			       '(anchor angle axis)))
	       joint-sequence))
	 (anchors+orientations (map (lambda(anchor angle axis)
				      `(,anchor ,(rotation-quaternion
						  #;around axis
							   #;by angle)))
				    anchors angles axes))
	 ((kinematic-chain ...)
	  (kinematic-chain<-anchors+orientations anchors+orientations))
	 (translation rotation (kinematic-chain-substitute kinematic-chain))
	 (global-body-position (body-property selected-body 'position))
	 (local-body-position (rotate (- global-body-position translation)
				      #;by (~ rotation)))
	 (system-equation (position<-angles (map list anchors axes)
					    local-body-position))
	 (angles (desired-configuration global-body-position
					position angles
					system-equation))
	 (pose `(pose . ,(map (lambda(joint angle)
				`(,(joint-name joint) . ,angle))
			      joint-sequence angles)))
	 ((x y z) (3d->screen view (apply system-equation angles)))
	 )
    (pretty-print (!# angles))
    #|
    (pretty-print 
     (apply ((isotropic-jacobian-approximation
	      #;of (compose uniform-vector->list system-equation))
	     #;by 0.00001) #;at angles))
    |#
    (assert (and (apply eq? 'hinge (map joint-type joint-sequence))
		 (= global-body-position (apply system-equation angles))))
    #;(pretty-print `(#:real ,global-body-position
			   #:system ,(apply system-equation angles)
			   #:solution ,angles))
    (if (every finite? angles)
	(begin 
	  (set-pose! #;of the-legs #;to pose #:keeping fixed)
	  ;(set-mouse-position! x y)
	  )
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
	 (mousemove
	  (lambda (x y xrel yrel)
	    (with-context-for-joint/body-relation
	     (apply-inverse-kinematics!
	      #;of final-body #;to (screen->3d view x y zs)))))))))))

;(keydn 'k (ik-mode view))

(keydn 'g (grab-mode view))

(define pause #t)

(keydn 'p 
  (lambda () 
    (set! pause (not pause))
    (format #t "pause ~a\n" (if pause 'on 'off))))

(define ((rotate-around-joint/mode view))
  (with-context-for-joint/body-relation
   (match (map #[_ 'body] #[view 'selected])
     ((first second)
      (let ((joint (joint-connecting-bodies first second))
	    (previously-selected #[view 'selected]))
	(let* ((left right (split-bodies-at joint))
	       (move still (cond ((in? first left)
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

(define-syntax-rule (gimme shit)
  (lambda () (pretty-print shit #:width 160)))

(keydn 'h (rotate-around-joint/mode view))

(keydn 'c (gimme (current-pose)))

(keydn 'b (gimme (joint-angles the-legs)))

(keydn 'x (gimme (joint-axes the-legs)))

(keydn 'z (gimme (joint-anchors the-legs)))

(keydn 'v (lambda ()
	    (set-pose! #;of the-legs #;to (current-pose))))

(add-timer! 
 25 
 (lambda()
   (unless pause
     (make-simulation-step! the-simulation)
   )))

(load "config.scm")
