(define-module (editor relations)
  #:use-module (extra common)
  #:export (
	     bodies-linked-to
	     split-bodies-at
	     bodies-are-connected?
	     joint-connecting-bodies
	     body-attached-by
	     joints-attached-to
	     kinematic-chains
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
  (match-let (((left-body right-body) (two-bodies-attached-by joint)))
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

(define (body-island-leaves #;of body-island)
  (filter (lambda (body)
	    (= (length (joints-attached-to body)) 1))
	  body-island))

(define (kinematic-chains #:from start #:to end #:excluding (bodies '()))
  (let ((neighbours (difference (bodies-attached-to start) `(,start) bodies)))
    (cond ((or (null? neighbours) (in? start bodies))
	   #f)
	  ((in? end neighbours)
	   `((,start ,end)))
	  (else
	   (let ((subchains (concatenate
			     (filter-map
			      (lambda (x)
				(kinematic-chains 
				 #:from x #:to end
				 #:excluding `(,start ,@bodies)))
			      neighbours))))
	     (and (not (null? subchains))
		  (map (lambda (subchain)
			 `(,start ,@subchain))
		       subchains)))))))
