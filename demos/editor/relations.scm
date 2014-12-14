(define-module (editor relations)
  #:use-module (extra common)
  #:use-module (extra math)
  #:export (
	     bodies-linked-to
	     split-bodies-at
	     bodies-are-connected?
	     joint-connecting-bodies
	     body-attached-by
	     joints-attached-to
	     body-sequences
	     body-island-leaves
	     shortest-joint-sequence-from+furthest-end
	     shortest-joint-sequence

	     anchors+orientations<-kinematic-chain
	     kinematic-chain<-anchors+orientations
	     kinematic-chain-substitute
	     position<-angles
	     desired-configuration
	     ))

;; Throughout this module, we understand that two bodies are ATTACHED
;; to each other if there exists a joint that connects those bodies
;; directly.
;; The bodies A, B are LINKED to each other if there exists a sequence
;; of bodies body-1, ..., body-n, such that body-1 = A, body-2 = B and
;; for 1 <= i < n, body-i is attached to body-i+1. Each such sequence
;; is called KINEMATIC CHAIN.

(without-default (joint-property-getter)
  (define (two-bodies-attached-by joint)
    `(,((specific joint-property-getter) joint 'body-1)
      ,((specific joint-property-getter) joint 'body-2))))

(define (attaches? joint body1 #;with body2)
  (let ((attached-bodies (two-bodies-attached-by joint)))
    (and (in? body1 attached-bodies)
	 (in? body2 attached-bodies))))

(without-default (body-rig-getter rig-joints-getter)
  (define (joints-attached-to body)
    (let* ((rig ((specific body-rig-getter) body))
	   (joints ((specific rig-joints-getter) rig)))
      (filter (lambda (joint)
		(in? body (two-bodies-attached-by joint)))
	      joints))))

(define (body-attached-by joint #;to body)
  (element #;of (delete body #;from (two-bodies-attached-by joint))))

(define (bodies-attached-to body)
  (map (lambda (joint)
	 (body-attached-by joint #;to body))
       (joints-attached-to body)))

(define (bodies-linked-to body #:except '())
  (let ((attached-bodies (difference (bodies-attached-to body) except)))
    (fold union attached-bodies 
	  (map (lambda (attached-body)
		 (bodies-linked-to attached-body #:except `(,body ,@except)))
	       attached-bodies))))

(define (split-bodies-at joint)
  (let (((left-body right-body) (two-bodies-attached-by joint)))
    (values
     (union (bodies-linked-to left-body #:except `(,right-body))
	    `(,left-body))
     (union (bodies-linked-to right-body #:except `(,left-body))
	    `(,right-body)))))

(define (joints-connecting-bodies body-1 body-2)
  ;; it is most likely going to be a singleton, but that's not guaranteed
  (intersection #;of (joints-attached-to body-1) 
		     #;and (joints-attached-to body-2)))

(define (joint-connecting-bodies body-1 body-2)
  (element #;of (joints-connecting-bodies body-1 body-2)))

(define (bodies-are-connected? body-1 body-2)
  (not (null? (intersection (joints-attached-to body-1)
			    (joints-attached-to body-2)))))

(define (body-island? l)
  (match l
    ((first . rest)
     (let ((island (bodies-linked-to first)))
       (for-every body in rest
	 (in? body island))))))

(define (body-island-leaves body-island)
  (filter (lambda (body)
	    (let ((joints (joints-attached-to body)))
	      (or (= (length joints) 1)
		  (apply equal? (map (lambda (joint)
				       (body-attached-by joint #;to body))
				     joints)))))
   body-island))

(define (body-sequences #:from start #:to end #:excluding (bodies '()))
  (let ((neighbours (difference (bodies-attached-to start) `(,start) bodies)))
    (cond ((or (null? neighbours) (in? start bodies))
	   #f)
	  ((in? end neighbours)
	   `((,start ,end)))
	  (else
	   (let ((subchains (concatenate
			     (filter-map
			      (lambda (x)
				(body-sequences 
				 #:from x #:to end
				 #:excluding `(,start ,@bodies)))
			      neighbours))))
	     (and (not (null? subchains))
		  (map (lambda (subchain)
			 `(,start ,@subchain))
		       subchains)))))))

(define (shortest-joint-sequence #;from start #;to end)
  (let ((bodies (apply argmin length (body-sequences #;from start #;to end))))
    (map joint-connecting-bodies (drop-right bodies 1) (drop bodies 1))))

(define (shortest-joint-sequence-from+furthest-end #;to body)
  (let* ((island (bodies-linked-to body))
	 (ends (delete body #;from (body-island-leaves island)))
	 (sequence-sets (map (lambda (end)
			       (body-sequences #:from body #:to end))
			     ends))
	 (set-of-sequences-from-body-to-furthest-end 
	  (apply argmax (lambda (sequences) (apply min (map length sequences)))
		 sequence-sets))
	 (shortest-body-sequence-from-furthest-end-to-body
	  (reverse (apply argmin length 
			  set-of-sequences-from-body-to-furthest-end)))
	 (corresponding-joint-sequence
	  (map joint-connecting-bodies
	       (drop-right shortest-body-sequence-from-furthest-end-to-body 1)
	       (drop shortest-body-sequence-from-furthest-end-to-body 1))))
    (values corresponding-joint-sequence
	    (first shortest-body-sequence-from-furthest-end-to-body))))

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
	    (new-anchor (rotate (- global-anchor translation)
				#;by rotation)))
	`((,new-anchor ,new-rotation) 
	  . ,(local<-global rest global-anchor 
			    (* (~ new-rotation) rotation))))))))

(publish
 (define (anchors+orientations<-kinematic-chain chain)
   (global<-local chain #f32(0 0 0) '(1.0 . #f32(0 0 0))))
 where
 (define (global<-local kinematic-chain translation rotation)
   (match kinematic-chain
     (()
      '())
     (((local-anchor local-rotation) . rest)
      (let ((new-anchor (+ translation (rotate local-anchor #;by rotation)))
	    (new-rotation (* rotation local-rotation)))
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
      (translation+rotation rest (+ translation (rotate local-anchor
							#;by rotation))
			    (* rotation local-rotation))))))

;; co nam powinna dawać funkcja kinematyki odwrotnej?
;; co powinna pobierać?
;; powinno to być coś takiego, co -- dla danych punktów początkowego i
;; końcowego oraz początkowej konfiguracji kątów zwraca nową konfigurację
;; kątów

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
  (let ((position-increment (- desired-position initial-position))
	(jacobian (apply ((isotropic-jacobian-approximation 
			   #;of (compose uniform-vector->list system-equation))
			  #;by 0.0001)
			 #;at initial-configuration)))
    (map + initial-configuration 
	 (uniform-vector->list
	  (* (pseudoinverse #;of jacobian) position-increment)))))

(define (position<-angles global-anchors+axes local-position)
  (impose-arity
   `(,(length global-anchors+axes) 0 #f)
   (lambda angles
     (let* ((anchors+orientations (map (lambda ((anchor axis) angle)
					 `(,anchor ,(rotation-quaternion 
						     #;around axis 
							      #;by angle)))
				       global-anchors+axes angles))
	    (kinematic-chain (kinematic-chain<-anchors+orientations
			      anchors+orientations))
	    (translation rotation 
			 (kinematic-chain-substitute kinematic-chain)))
       (+ translation (rotate local-position #;by rotation))))))

(define (inverse-translation+rotation translation rotation)
  (let ((inverse (~ rotation)))
    (values (rotate (- translation) #;by inverse)
	    inverse)))
