(define-module (editor modes)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (editor relations)
  #:export (
	     position<-angles
	     desired-configuration
	     global-positions
	     tip-position
	     kinematic-chain
	    ))

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
