(define-module (editor modes)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (editor relations)
  #:export (
	     anchors+orientations<-kinematic-chain
	     kinematic-chain<-anchors+orientations
	     kinematic-chain<-anchors+axes+angles
	     build-kinematic-chain
	     kinematic-chain-substitute
	     position<-angles
	     desired-configuration
	     position-at-tip-of-kinematic-chain
	     global-positions
	     global-positions<-kinematic-chain
	    ))

(publish
 (define (kinematic-chain<-anchors+axes+angles anchors axes angles)
   (kinematic-chain (zip anchors axes angles)))

 (define (build-kinematic-chain #;from anchors+axes #;and angles)
   (map (lambda ((anchor axis angle))
	  `(,anchor ,(rotation-quaternion #;around axis #;by angle)))
	(kinematic-chain (map (lambda ((anchor axis) angle)
				`(,anchor ,axis ,angle))
			      anchors+axes angles))))
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
 (define (kinematic-chain<-anchors+orientations anchors+rotations)
   (local<-global anchors+rotations #f32(0 0 0) '(1.0 . #f32(0 0 0))))
 where 
 (define (local<-global global-anchor+rotation-chain translation rotation)
   (match global-anchor+rotation-chain
     (()
      '())
     (((global-anchor global-rotation) . rest)
      (let ((new-rotation (* rotation global-rotation))
	    (new-anchor (rotate (- translation global-anchor)
				#;by rotation)))
	`((,new-anchor ,new-rotation) 
	  . ,(local<-global rest global-anchor 
			    (* (~ new-rotation) rotation))))))))

(define (global-positions<-kinematic-chain kinematic-chain)
  (match kinematic-chain
    (()
     '())
    (((position rotation) . rest)
     `(,position ,@(global-positions<-kinematic-chain
		    (map (lambda ((dependent-position dependent-rotation))
			   `(,(rotate (+ dependent-position  position)
					 #;by rotation)
			     (* rotation dependent-rotation #;rotation)))
			 rest))))
    (else
     kinematic-chain)))

(publish
 (define (global-positions kinematic-chain)
   (anchors+orientations<-kinematic-chain 
				   (map (lambda ((anchor axis angle))
					  `(,anchor
					    ,(rotation-quaternion
					      #;around axis
						       #;by angle)))
					kinematic-chain)))

 (define (anchors+orientations<-kinematic-chain chain)
   (global<-local chain #f32(0 0 0) '(1.0 . #f32(0 0 0))))
 where
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

(assert (and (identity? (compose anchors+orientations<-kinematic-chain
				 kinematic-chain<-anchors+orientations))
	     (identity? (compose kinematic-chain<-anchors+orientations
				 anchors+orientations<-kinematic-chain))))

(publish
 (define (kinematic-chain-substitute kinematic-chain)
   "a single translation and rotation that stand for the application of\
 the whole kinematic chain"
   (translation+rotation kinematic-chain #f32(0 0 0) '(1.0 . #f32(0 0 0))))
 where
 (define (translation+rotation local-chain translation rotation)
   (match local-chain
     (()
      (values translation rotation))
     (((local-anchor local-rotation) . rest)
      (let ((new-rotation (rotate rotation (~ local-rotation))))
	(translation+rotation rest (+ translation (rotate local-anchor
							  #;by (~ rotation)))
			      new-rotation))))))

;; co musimy przetestować? jakie ten nasz kod czyni założenia?
;; - mamy daną funkcję, która pobiera kąty i zwraca położenie
;; elementu końcowego. W jakim układzie współrzędnych wyrażone
;; jest to położenie?
;; - skąd bierze się taka jednorodność jakobianu? czy to aby
;; nie podejrzane?

(define (position-at-tip-of-kinematic-chain kinematic-chain local-position)
  (match kinematic-chain
    (()
     local-position)
    ((transforms ... (local-translation local-rotation))
     (position-at-tip-of-kinematic-chain transforms 
					 (+ local-translation 
					    (rotate local-position 
						    #;by local-rotation))))))

(define (position<-angles global-anchors+axes local-position)
  (let ((N (length global-anchors+axes)))
    (impose-arity
     `(,N 0 #f)
     (lambda angles
       (let ((kinematic-chain (build-kinematic-chain global-anchors+axes
						     angles)))
	 (position-at-tip-of-kinematic-chain kinematic-chain 
					     local-position))))))

(define (desired-configuration initial-position desired-position
			       initial-configuration system-equation
			       direction-correction)
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
	 ;;(increment-magnitude (norm position-increment))
	 (jacobian (apply ((isotropic-jacobian-approximation 
			    #;of (compose uniform-vector->list 
					  system-equation))
			   #;by 0.00001)
			  #;at initial-configuration))
	 (jacobian+ (pseudoinverse #;of jacobian))
	 (angle-increment (uniform-vector->list 
			   (* jacobian+ position-increment)))
	 (result (map * #;direction-correction
		      (map normalized-radians 
			   (map + initial-configuration angle-increment))))
	 (obtained-position (apply system-equation #;to result)))
    ;;(<<< (* (list->uniform-array jacobian) jacobian+))
    (<<< direction-correction desired-position obtained-position result)
    #;(<< 
     "initial: " initial-position
	"desired: " desired-position
	"obtained: " obtained-position
	"desired increment: " position-increment
	"difference: " (norm (- desired-position obtained-position))
	"angle increment: " angle-increment
	)
    result))

(define (inverse-translation+rotation translation rotation)
  (let ((inverse (~ rotation)))
    (values (rotate (- translation) #;by inverse)
	    inverse)))
