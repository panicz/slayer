(define-module (extra ref)
  #:use-module (oop goops)
  #:use-module (ice-9 nice-9)
  #:use-module (extra common)
  #:export (ref aref fref random-element set-values! <ref-interface> slots
		no-setter)
  #:export-syntax (accessor)
  )

(define-method (slots (object <object>))
  (map (lambda (x)
	 (match x
	   ((head . _) #;=> head)
	   (_          #;=> x)))
       (class-slots (class-of object))))

(define-class <ref-interface> ()
  (getter ;;#:allocation #:each-subclass
	  #:init-value 
	  (lambda(object key)
	    (format #t "getting ~s from ~s\n" key object))
	  #:init-keyword #:getter)
  (setter ;;#:allocation #:each-subclass
	  #:init-value 
	  (lambda(object key value)
	    (format #t "setting ~s in ~s to ~s value\n" key object value))
	  #:init-keyword #:setter))

(define-syntax-rule (accessor x ref)
  (make-procedure-with-setter
   (lambda (x) ref)
   (lambda (x value) (set! ref value))))

(define (getter obj key)
  (cond ((vector? obj)
	 (vector-ref obj key))
	((uniform-vector? obj)
	 (uniform-vector-ref obj key))
	((hash-table? obj)
	 (hash-ref obj key))
	((string? obj)
	 (string-ref obj key))
	((is-a? obj <ref-interface>)
	 ((slot-ref obj 'getter) obj key))
	((instance? obj)
	 (slot-ref obj key))
	((and (list? obj) (integer? key) (positive? key))
	 (list-ref obj key))
	((list? obj)
	 (assoc-ref obj key))
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
	((is-a? obj <ref-interface>)
	 ((slot-ref obj 'setter) obj key value))
	((instance? obj)
	 (slot-set! obj key value))
	((and (list? obj) (integer? key) (positive? key))
	 (list-set! obj key value))
	((list? obj)
	 (assoc-set! obj key value))
	((boolean? obj)
	 (if #f #f))
	((array? obj)
	 (array-set! obj value key))))

(define ((no-setter slot-name) self value)
  (<< "no valid setter for slot"slot-name"in object"self"(value"value")"))

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

(define iref (make-procedure-with-setter
	      (lambda(object key)
		((slot-ref object 'getter) object key))
	      (lambda(object key value)
		((slot-ref object 'setter) object key value))))

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

(define-method (ref (object <ref-interface>) key)
  (iref object key))

(define-method (ref (object <top>) (key <symbol>))
  (sref object key))

(define-generic set-values!)

(define-method (set-values! (dest <array>) (source <list>))
  (TODO this procedure should consider more complex cases,
	like assigning list of arrays of lists and so on)
  (let* ((dimensions (array-dimensions dest))
	 (depth (1- (length dimensions))))
    (let set-level! ((higher-indices '())
		     (source-layer source))
      (cond ((list? source-layer)
	     (for (i v) in (indexed source-layer)
		  (if (= (length higher-indices) depth)
		      (apply array-set! dest v (reverse `(,i ,@higher-indices)))
		      (set-level! `(,i ,@higher-indices) v))))
	    ((generalized-vector? source-layer)
	     (for i in 0 .. (1- (generalized-vector-length source-layer))
		  (apply array-set! dest (generalized-vector-ref source-layer i)
			 (reverse `(,i ,@higher-indices)))))))))

(e.g.
 (let ((a '#2f32((1 2 3)
		 (4 5 6))))
   (set-values! a '((10 20 30)
		    (40 50 60)))
   a)
 ===> #2f32((10 20 30)
	    (40 50 60)))

(e.g.
 (let* ((a '#2f32((1 2)
		  (3 4)))
	(b a))
   (set-values! a '((5 6)
		    (7 8)))
   (eq? a b)))

(e.g.
 (let ((a '#2((1 2)
	      (3 4))))
   (set-values! a '(#(5 6) #(7 8)))
   a)
 ===> #2((5 6)
	 (7 8)))

(define-method (set-values! (dest <vector>) (source <list>))
  (for (i v) in (indexed source)
       (vector-set! dest i v)))

(define-method (set-values! (dest <uvec>) (source <list>))
  (for (i v) in (indexed source)
       (uniform-vector-set! dest i v)))

(e.g.
 (let ((a '#f32(1 2 3)))
   (set-values! a '(4 5 6))
   a)
 ===> #f32(4 5 6))

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

;; (define (read-hash-n char port)
;;   (let loop ((n (list char)) (char (read-char port)))
;;     (if (char-numeric? char)
;; 	(loop (cons char n) (read-char port))
;; 	(begin
;; 	  (unread-char char port)
;; 	  (let loop ((n (string->number (list->string (reverse n))))
;; 		     (result (list (read port))))
;; 	    (if (= n 0)
;; 		(reverse result)
;; 		(loop (1- n) (cons (read port) result))))))))

;; (for-each (lambda(c)
;; 	    (read-hash-extend c read-hash-n))
;; 	  (map (lambda(i)(integer->char (+ i (char->integer #\0)))) (iota 10)))
