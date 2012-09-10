(define-module (extra ref)
  #:use-module (oop goops)
  #:export (ref))
 
(define (getter obj key)
  (cond ((vector? obj)
	 (vector-ref obj key))
	((hash-table? obj)
	 (hash-ref obj key))
	((instance? obj)
	 (slot-ref obj key))))

(define (setter obj key value)
  (cond ((vector? obj)
 	 (vector-set! obj key value))
	((hash-table? obj)
	 (hash-set! obj key value))
	((instance? obj)
	 (slot-set! obj key value))))

(define ref (make-procedure-with-setter getter setter))

(define-generic ref)

(define vref (make-procedure-with-setter vector-ref vector-set!))
(define href (make-procedure-with-setter hash-ref hash-set!))
(define sref (make-procedure-with-setter slot-ref slot-set!))

(define-method (ref (vector <vector>) (key <integer>))
  (vref vector key))

(define-method (ref (hash <hashtable>) key)
  (href hash key))

(define-method (ref (object <top>) (key <symbol>))
  (sref object key))

;(with-input-from-string "#[2 3\n]" read) (lambda()(catch #t read (lambda args args))))

(read-hash-extend #\[
		  (lambda (char port)
		    (let* ((exp `(ref ,(read port) ,(read port))))
		      ;(read-char port)
		      (catch 'read-error (lambda()(read port)) (lambda(key . args) key))
		      exp)))

