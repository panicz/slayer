(define-module (extra ref)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:export (ref))
 
(define (getter obj key)
  (cond ((vector? obj)
	 (vector-ref obj key))
	((hash-table? obj)
	 (hash-ref obj key))
	((instance? obj)
	 (slot-ref obj key))
	((array? obj)
	 (array-ref obj key))))

(define (setter obj key value)
  (cond ((vector? obj)
 	 (vector-set! obj key value))
	((hash-table? obj)
	 (hash-set! obj key value))
	((instance? obj)
	 (slot-set! obj key value))
	((array? obj)
	 (array-set! obj value key))))

(define ref (make-procedure-with-setter getter setter))

(define-generic ref)

(define vref (make-procedure-with-setter vector-ref vector-set!))

(define href (make-procedure-with-setter hash-ref hash-set!))

(define sref (make-procedure-with-setter slot-ref slot-set!))

(define aref (make-procedure-with-setter 
	      (lambda(array indices)(apply array-ref array indices))
	      (lambda(array indices value)(apply array-set! array value indices))))

(define-method (ref (vector <vector>) (key <integer>))
  (vref vector key))

(define-method (ref (hash <hashtable>) key)
  (href hash key))

(define-method (ref (object <top>) (key <symbol>))
  (sref object key))

(read-hash-extend #\[
		  (let ((process (match-lambda 
				  ((object key)
				   `(ref ,object ,key))
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
				 (loop (cons (read port) exp)))))))))

(if (string<? (version) "2.0.0")
    (read-hash-extend #\;
     (lambda (char port)
       (let ((comment (read port)))
	 (if #f #t)))))

