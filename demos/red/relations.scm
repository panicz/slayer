(define-module (red relations)
  #:use-module (oop goops)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (red joint)
  #:use-module (red body)
  #:export (
	     bodies-linked-to
	     split-bodies-at
	     bodies-are-connected?
	     joint-connecting-bodies
	     body-attached-by
	     joints-attached-to
	    ))

;; these procedures are introduced for compatibility with (scum physics)
;; module, so that -- if needed -- the code below can be copied verbatim
;; to the simulator
(define (body-rig body)
  #[body 'rig])

(define (joint-property joint property)
  (assert (in? property '(body-1 body-2)))
  #[joint property])

(define (rig-joints rig)
  (filter (lambda(x)(is-a? x <physical-joint>)) #[rig 'objects]))

(define (two-bodies-attached-by joint)
  `(,(joint-property joint 'body-1) ,(joint-property joint 'body-2)))

(define (attaches? joint body1 #;with body2)
  (let ((attached-bodies (two-bodies-attached-by joint)))
    (and (in? body1 attached-bodies)
	 (in? body2 attached-bodies))))

(define (joints-attached-to body)
  (let* ((rig (body-rig body))
	 (joints (rig-joints rig)))
    (filter (lambda (joint)
	      (in? body (two-bodies-attached-by joint)))
	    joints)))

(define (body-attached-by joint #;to body)
  (first (delete body #;from (two-bodies-attached-by joint))))

(define (bodies-attached-to body)
  (map (lambda (joint)
	 (body-attached-by joint #;to body))
       (joints-attached-to body)))

(define (bodies-linked-to body #;except (bodies ...))
  (let ((attached-bodies (difference (bodies-attached-to body) bodies)))
    (fold union attached-bodies 
	  (map (lambda (attached-body)
		 (bodies-linked-to attached-body #;except `(,body ,@bodies)))
	       attached-bodies))))

(define (split-bodies-at joint)
  (match-let (((left-body right-body) (two-bodies-attached-by joint)))
    (values
     (union (bodies-linked-to left-body #;excluding `(,right-body))
	    `(,left-body))
     (union (bodies-linked-to right-body #;excluding `(,left-body))
	    `(,right-body)))))

(define (joint-connecting-bodies body-1 body-2)
  (first (intersection (joints-attached-to body-1)
		       (joints-attached-to body-2))))

(define (bodies-are-connected? body-1 body-2)
  (not (null? (intersection (joints-attached-to body-1)
			    (joints-attached-to body-2)))))

#|
(let*-values (((left right) (split-bodies-at joint))
	      ((move still) (cond ((in? body left)
				   (values left right))
				  ((in? body right)
				   (values right left))
				  (else
				   (error "bad argument")))))
		((anchor axis) (values (joint-property joint 'anchor)
				       (joint-property joint 'axis))))
    (for body in move
      (set! #[body 'position] (rotate #[body 'position]
				      #;by (deg->rad some-degrees)
					   #;around axis))
      (set! #[body 'quaternion] 
	    (* (rotation-quaternion #;around axis #;by (deg->rad some-degrees))
	       #[body 'quaternion])))))
								 
		((positions orientations)
		 (values 
		  (map (lambda(b)(body-property b 'position)) rotated)
		  (map (lambda(b)(body-property b 'quaternion)) rotated)))
		((rotated-positions rotated-orientations)
		 (values
		  (map (lambda (position) ...) positions)
		  (map (lambda (orientation) ...) orientations))))
|#