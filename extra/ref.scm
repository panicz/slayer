(define-module (extra ref)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module (extra common)
  #:export (ref aref))
 
(define (getter obj key)
  (cond ((vector? obj)
	 (vector-ref obj key))
	((uniform-vector? obj)
	 (uniform-vector-ref obj key))
	((hash-table? obj)
	 (hash-ref obj key))
	((string? obj)
	 (string-ref obj key))
	((instance? obj)
	 (slot-ref obj key))
	((array? obj)
	 (array-ref obj key))))

(define (setter obj key value)
  (cond ((vector? obj)
 	 (vector-set! obj key value))
	((uniform-vector? obj)
	 (uniform-vector-set! obj key value))
	((hash-table? obj)
	 (hash-set! obj key value))
	((string? obj)
	 (string-set! obj key value))
	((instance? obj)
	 (slot-set! obj key value))
	((boolean? obj)
	 (if #f #f))
	((array? obj)
	 (array-set! obj value key))))

(define ref (make-procedure-with-setter getter setter))

(define-generic ref)

(define vref (make-procedure-with-setter vector-ref vector-set!))

(define uvref (make-procedure-with-setter uniform-vector-ref uniform-vector-set!))

(define href (make-procedure-with-setter hash-ref hash-set!))

(define sref (make-procedure-with-setter slot-ref slot-set!))

(define stref (make-procedure-with-setter string-ref string-set!))

(define asref (make-procedure-with-setter assoc-ref assoc-set!))

(define aref (make-procedure-with-setter 
	      (lambda(array indices)(apply array-ref array indices))
	      (lambda(array indices value)(apply array-set! array value indices))))

(define-method (ref (fluid <fluid>) . rest)
  (apply ref (fluid-ref fluid) rest))

(define-method (ref (vector <vector>) (key <integer>))
  (vref vector key))

(define-method (ref (vector <uvec>) (key <integer>))
  (uvref vector key))

(define-method (ref (hash <hashtable>) key)
  (href hash key))

(define-method (ref (string <string>) key)
  (stref string key))

(define-method (ref (list <list>) key)
  (asref list key))

(define-method (ref (bool <boolean>) key)
  #f)

(define-method (ref (object <top>) (key <symbol>))
  (sref object key))

(read-hash-extend #\[
		  (letrec ((process-:-sequence (lambda(sequence initial)
						 (match sequence
						   (()
						    initial)
						   ((': key . rest)
						    (process-:-sequence rest `(ref ,initial ,key)))))))
		    (let ((process (match-lambda
				    (('_ key)
				     `(lambda(_)(ref _ ,key)))
				    ((object key)
				     `(ref ,object ,key))
				    (('_ ': key . rest)
				     `(lambda(_),(process-:-sequence rest `(ref _ ,key))))
				    ((object ': key . rest)
				     (process-:-sequence rest `(ref ,object ,key)))
				    ((seq start '.. end)
				     `(substring ,seq ,start ,end))
				    ((seq start ': count)
				     `(substring ,seq ,start ,(min (string-length seq) (+ start count))))
				    ((array . indices)
				     `(aref ,array (list ,@indices)))
				    (default
				      default))))
		      (lambda (char port) 
			(let loop ((exp '()))
			  (let ((char (read-char port)))
			    (cond ((char-whitespace? char)
				   (loop exp))
				  ((eq? char #\])
				   (process (reverse exp)))
				  (#t
				   (unread-char char port)
				   (loop (cons (read port) exp))))))))))

(if (string<? (version) "2.0.0")
    (read-hash-extend #\;
     (lambda (char port)
       (let ((comment (read port)))
	 (if #f #t)))))

