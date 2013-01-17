(define-module (extra ref)
  #:use-module ((oop goops) #:hide (slot-ref slot-set!))
  #:use-module (ice-9 match)
  #:use-module (extra common)
  #:export (ref aref fref random-element slot-set! slot-ref))

(define-generic slot-set!)

(define-generic slot-ref)

(define-method (slot-ref object slot-name)
  ((@(oop goops)slot-ref) object slot-name))

(define-method (slot-set! object slot-name value)
  ((@(oop goops)slot-set!) object slot-name value))


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

(define-generic random-element)

(define-method (random-element (l <list>))
  (list-ref l (random (length l))))

(define-method (random-element (v <vector>))
  (vector-ref v (random (vector-length v))))

(define vref (make-procedure-with-setter vector-ref vector-set!))

(define uvref (make-procedure-with-setter uniform-vector-ref 
					  uniform-vector-set!))

(define href (make-procedure-with-setter hash-ref hash-set!))

(define sref (make-procedure-with-setter slot-ref slot-set!))

(define stref (make-procedure-with-setter string-ref string-set!))

(define asref (make-procedure-with-setter assoc-ref assoc-set!))

(define lsref (make-procedure-with-setter list-ref list-set!))

(define fref (make-procedure-with-setter fluid-ref fluid-set!))

(define aref (make-procedure-with-setter 
	      (lambda(array indices)(apply array-ref array indices))
	      (lambda(array indices value)(apply 
					   array-set! 
					   array value indices))))

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

(define-method (ref (list <list>) (key <integer>))
  (lsref list key))

(define-method (ref (list <list>) key)
  (asref list key))

(define-method (ref (bool <boolean>) key)
  #f)

(define-method (ref (object <top>) (key <symbol>))
  (sref object key))

(read-hash-extend 
 #\[
 (letrec ((process-:-sequence (lambda(sequence initial)
				(match sequence
				  (()
				   initial)
				  ((': key . rest)
				   (process-:-sequence 
				    rest 
				    `(ref ,initial ,key)))))))
   (let ((process (match-lambda
		      (('_ key)
		       `(lambda(_)(ref _ ,key)))
		    ((object '_)
		     `(lambda(_)(ref ,object _)))
		    ((object key)
		     `(ref ,object ,key))
		    (('_ ': key . rest)
		     `(lambda(_)
			,(process-:-sequence rest `(ref _ ,key))))
		    ((object ': key . rest)
		     (process-:-sequence rest `(ref ,object ,key)))
		    ((seq start '.. end)
		     `(substring ,seq ,start ,end))
		    ((seq start ': count)
		     `(substring ,seq ,start 
				 ,(min (string-length seq) 
				       (+ start count))))
		    (((? integer? size))
		     `(make-hash-table ,size))
		    ((fluid)
		     `(fref ,fluid))
		    ((array . indices)
		     `(aref ,array (list ,@indices)))
		    (()
		     '(make-hash-table))
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

(define (read-hash-n char port)
  (let loop ((n (list char)) (char (read-char port)))
    (if (char-numeric? char)
	(loop (cons char n) (read-char port))
	(begin
	  (unread-char char port)
	  (let loop ((n (string->number (list->string (reverse n))))
		     (result (list (read port))))
	    (if (= n 0)
		(reverse result)
		(loop (1- n) (cons (read port) result))))))))

(for-each (lambda(c)
	    (read-hash-extend c read-hash-n))
	  (map (lambda(i)(integer->char (+ i (char->integer #\0)))) (iota 10)))
