
(define v #2f32((0 0 0)))

(keydn 'i (lambda () (set! v (+ v #2f32((0.01 0 0))))))

(keydn 'k (lambda () (set! v (- v #2f32((0.01 0 0))))))

(keydn 'j (lambda () (set! v (+ v #2f32((0 0.01 0))))))

(keydn 'l (lambda () (set! v (- v #2f32((0 0.01 0))))))

(define the-current-joint 0)

(define the-angle-increment 0.001)

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

(keydn 'backspace
  (lambda ()
    (set! the-angle-increment (- the-angle-increment))))

(for i in (1 .. 8)
  (keydn (string-append "f" (number->string i))
    (lambda ()
      (set! the-current-joint (- i 1)))))

(keydn 'f10 (lambda () (set! the-current-joint #f)))

(keydn 'f11 (lambda () (set! the-current-joint #t)))


(keydn '-
  (lambda ()
    (set! the-angle-increment 
      (if (modifier-pressed? 'shift)
	  (- the-angle-increment 
	     (* 0.1 the-angle-increment))
	  (/ the-angle-increment 2.0)))
    (<< the-angle-increment)))

(keydn '=
  (lambda ()
    (set! the-angle-increment 
      (if (modifier-pressed? 'shift)
	  (+ the-angle-increment
	     (* 0.1 the-angle-increment))
	  (* the-angle-increment 2.0)))
    (<< the-angle-increment)))


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
      (when (changed? `(,selected ,the-current-joint ,the-angle-increment ,v
				  ,new-pose))
	(set! new-pose #f)
	(match selected
	  (()
	   (set! kinematic-chain-mesh '(mesh))
	   (set! global-skeleton-mesh '(mesh)))
	  ((first . rest)
	   (let* ((body #[first 'body])
		  (joints end
			  (shortest-joint-sequence-from+furthest-end #;to body))
		  (N (length joints))
		  (((anchors axes angles directions) ...)
		   (hinge-joint-sequence-anchors+axes+angles+directions joints))
		  (orientations (map rotation-quaternion #;around axes 
				     #;by angles))
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
		  (local-position (rotate (- (body-property body 'position)
					     translation)
					  (~ rotation))
				  )
		  (((anchors orientations) ...)
		   (anchors+orientations<-kinematic-chain kinematic-chain))
		  
#|
		  (((anchors orientations) ...)
		   (kinematic-chain<-anchors+orientations kinematic-chain))

		  (anchors (global-positions<-kinematic-chain kinematic-chain))
|#
		  (system-equation (position<-angles (zip anchors axes)
						     local-position))
		  (global-position (apply system-equation angles))
		  (vertices (list->uniform-array
			     (map uniform-vector->list
				  `(,@anchors
				    #;,@(map + anchors (map * angles axes))
				    #;,global-position))))
		  (anchor-indices (list->uniform-array `(,@(iota N) #;,(* 2 N))))
		  #;(axis-indices (list->uniform-array (zip (iota N) (iota N N))))
		  )

#|
	     (<< (!# (last anchors)
		     (position-at-tip-of-kinematic-chain kinematic-chain
							 #f32(0 0 0))
		     (rotate (+ #f32(0 0 0) translation) #;by rotation)))

	     (<< (!# global-position))
	     (<< (!# (position-at-tip-of-kinematic-chain kinematic-chain
							 local-position)))
|#
	     (set! kinematic-chain-mesh (mesh<-kinematic-chain kinematic-chain))
	     (set! global-skeleton-mesh
	       `(mesh
		 (vertices ,vertices)
		 (colors ,*color-sequence*)
		 (faces
		  (points ,anchor-indices)
		  (line-strip ,anchor-indices)
		  #;(lines ,axis-indices)
		  ))))))))
      (case context 
	((kinematic-chain)
	 kinematic-chain-mesh)
	((global-skeleton)
	 global-skeleton-mesh)
	(else 
	 (error "unknown context" context)))))
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


(keydn 'i
  (lambda ()
    (with-context-for-joint/body-relation    
     (unless (null? #[view 'selected])
       (let* ((body #[(element #[view 'selected]) 'body])
	      (joint-sequence
	       (shortest-joint-sequence-from+furthest-end #;to body)))
	 (pretty-print (map joint-name joint-sequence)))))))

(keydn 'l
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
