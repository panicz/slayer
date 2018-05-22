(define-module (extra array)
  #:use-module (grand scheme)
  #:export (random-array
	    array-size
	    array-map/typed
	    array-map
	    array-append
	    array-copy
	    copy
	    list->uniform-array
	    list->uniform-vector))

(define* (random-array #:key (range 1.0)(type #t)(mean 0) #:rest dims)
  (let ((dims (remove-keyword-args dims)))
    (array-map (lambda (mean) (+ mean (- (random (* 2 range)) range)))
		(apply make-typed-array type mean dims))))

(define (array-size a)
  (fold * 1.0 (map (lambda (`(,lower ,upper))
		     (1+ (abs (- upper lower))))
		   (array-shape a))))

(define (array-map/typed type proc arg1 . args)
  (let ((result (apply make-typed-array type (if #f #f) 
		       (array-dimensions arg1))))
    (apply array-map! result proc arg1 args)
    result))

(define (array-map proc arg1 . args)
  (apply array-map/typed (array-type arg1) proc arg1 args))

(define (array-append first . rest)
  (list->typed-array 
   (array-type first)
   (length (array-dimensions first))
   (apply append (map array->list (cons first rest)))))

(define (array-copy array)
  (let* ((shape (array-shape array))
	 (copy (apply make-typed-array (array-type array) (if #f #f) shape)))
    (array-copy! array copy)
    copy))

(e.g.
 (let* ((array #f32(1 2 3))
	(copy (array-copy array)))
   (and (array-equal? array copy)
	(not (eq? array copy)))))

(define (copy object)
  (cond ((list? object)
	 (map copy object))
	((array? object)
	 (array-copy object))
	((string? object)
	 (string-copy object))
	;;((instance? object)
	;; (deep-clone object))
	(else object)))

(define (array-pointer array . address)
  (let ((shape (array-shape array)))
    (assert (every (lambda (index (lower upper))
		     (<= lower index upper))
		   address shape))
    (cond ((= (length address) (length shape))
	   (make-procedure-with-setter 
	    (lambda () (apply array-ref array address))
	    (lambda (value) (apply array-set! array value address))))
	  ((< (length address) (length shape))
	   (let ((view (apply make-shared-array array 
			      (lambda args (apply list `(,@address ,@args)))
			      (drop shape (length address)))))
	     (make-procedure-with-setter
	      (lambda () view)
	      (lambda (value) (array-copy! value view)))))
	  (else
	   (error "Invalid array address")))))

(e.g.
 (let* ((A #2f32((0 1 2)
		 (3 4 5)))
	(*A/1/1 (array-pointer A 1 1))
	(initial-A/1/1 (*A/1/1)))
   (set! (*A/1/1) 8)
   (values initial-A/1/1 (*A/1/1) A))
 ===> 4.0 8.0 #2f32((0 1 2)
		    (3 8 5)))

(e.g.
 (let* ((A #2f32((0 1 2)
		 (3 4 5)))
	(*A/1 (array-pointer A 1))
	(initial-A/1 (array-copy (*A/1))))
   (set! (*A/1) #(2 1 0))
   (values initial-A/1 (*A/1) A))
 ===> #f32(3 4 5) #f32(2 1 0) #2f32((0 1 2)
				    (2 1 0)))

(define (list->uniform-vector type list)
  (list->typed-array type 1 list))

(define (list->uniform-array numbers)
  (let ((shape (depth numbers))
	(type 
	 (let ((flat-list (flatten numbers)))
	   (cond ((or (null? flat-list)
		      (not (every number? flat-list)))
		  #t)
		 ((not (every real? flat-list))
		  'c32)
		 ((let ((upper (apply max flat-list))
			(lower (apply min flat-list)))
		    (cond ((not (every exact? flat-list))
			   'f32)
			  ((< lower 0)
			   (cond ((<= (- 1 (expt 2 7)) lower upper (expt 2 7))
				  's8)
				 ((<= (- 1 (expt 2 15)) lower upper (expt 2 15))
				  's16)
				 (else
				  's32)))
			  (else
			   (cond ((<= upper (expt 2 8))
				  'u8)
				 ((<= upper (expt 2 16))
				  'u16)
				 (else
				  'u32))))))))))
	(list->typed-array type shape numbers)))
