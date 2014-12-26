(define-module (editor relations)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra ref)
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
  (let* ((joint (element #;of (joints-connecting-bodies body-1 body-2)))
	 (direction (if (eq? body-1 (first (two-bodies-attached-by joint)))
			-1
			+1)))
    (values joint direction)))

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
    (map/values joint-connecting-bodies 
		(drop-right bodies 1) (drop bodies 1))))

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
	  directions
	  (map/values 
	   joint-connecting-bodies
	   (drop-right shortest-body-sequence-from-furthest-end-to-body 1)
	   (drop shortest-body-sequence-from-furthest-end-to-body 1))))
    (values corresponding-joint-sequence
	    (first shortest-body-sequence-from-furthest-end-to-body)
	    directions)))
