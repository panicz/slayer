; routines to support scmutils in guile

; some changes that are necessary:

; mit scheme comments   #| ... |#
; ->
; semicolon comments

; comment out in source files:
; (declare usual-integrations)

; mit scheme make-eq-hash-table		<- depends on comparator 
;	     hash-table/get
;	     hash-table/put!
; ->
; guile	     make-hash-table
;	     hashq-ref			<- depends on comparator 
;	     hashq-set!			<- depends on comparator 

;;;;;
; optional arguments
;
; mit scheme (define (proc #!optional arg)) ..
;            (lambda (proc #!optional arg)) ..
; ->
; guile      (define* (proc (#:optional arg)) ...
;            (lambda* (proc (#:optional arg)) ...
;;;;;
(cond-expand (guile-2
	      (use-modules (ice-9 curried-definitions)))
	     (else 
	      (use-modules (ice-9 syncase))))	; provides define-syntax
	     
(use-modules (ice-9 optargs))
(define (default-object? x)
  (eq? #f x))

(use-modules ((srfi srfi-1)
              :select (any every)))     ; provides any and every
; not importing everything because second = cadr in srfi-1 conflicts with SI unit

; ice9 streams work very differently from mit scheme streams.
; srfi-40 streams are much more similar to mit scheme.
; though srfi-40 uses stream-cons while mit scheme uses cons-stream.
(load "srfi-40.scm")
(define head stream-car)
(define tail stream-cdr)
(define empty-stream? stream-null?)
(define stream-ref
   (lambda (x n)
      (if (zero? n)
          (stream-car x)
          (stream-ref (stream-cdr x) (- n 1)))))
(define (stream-head x n)
  (if (zero? n)
      '()
      (cons (stream-car x)
	    (stream-head (stream-cdr x) (- n 1)))))

; to get string-contains
(use-modules (srfi srfi-13))
(define (string-head string index)
  (substring string 0 index))
(define (string-tail string index)
  (substring string index))

; to get (prime? 7)
;(use-modules (ice-9 slib)) 
;(require 'primes) 
(load "primes.scm")
;(use-modules ((math primes)))
;(require 'fluid-let)	; deprecated

; ignore integrable
(cond-expand (guile-2
	      (define-syntax define-integrable
		(syntax-rules ()
		  ((_ form body ...) (define form body ...)))))
	     (else
	       (define define-integrable define)))



;;;;;;;; environments
; mit scheme environments -> guile modules
; use set-current-module to change between environments
; after loading scmutils in guile:
; (set-current-module generic-environment)    <- env defined in kernel/genenv.scm
;   to get generic + * etc operators that take functions, vectors, etc as operands
;
; (set-current-module scmutils-base-environment)
;   to get standard scheme + * etc operators back
;
; after making a new environment with this routine
; call (define-module! env symbol value) to add to new environment
(define (extend-top-level-environment top-env)
  (let ((new-environment (make-module)))
    (module-use! new-environment top-env)
    new-environment))



(define (symbol<? x y)
  (string<? (symbol->string x)
	    (symbol->string x)))

(define (exact-integer? x)
  (and (integer? x)
       (exact? x)))
(define (exact-rational? x)
  (and (rational? x)
       (exact? x)))
(define (exact-positive-integer? x)
  (and (integer? x)
       (positive? x)
       (exact? x)))
(define (exact-nonnegative-integer? x)
  (and (integer? x)
       (not (negative? x))
       (exact? x)))

(define (floor->exact x)
  (inexact->exact (floor x)))

; guile 1.6 does not have numerator/denominator
; but guile 1.8 does
(cond-expand (guile-2
	      )
	     (else
	      (if (not (defined? 'numerator))
		  (define (numerator x) x))
	      (if (not (defined? 'denominator))
		  (define (denominator x) 1))
	      (if (not (defined? 'rationalize))
		  (define (rationalize x err) x))))


(define fix:fixnum? number?)

(define fix:= =)
(define fix:> >)
(define fix:< <)
(define fix:+ +)
(define fix:- -)
(define fix:* *)
(define fix:quotient quotient)
(define fix:negative? negative?)
(define fix:zero? zero?)
(define (fix:-1+ x) (+ -1 x))
(define (fix:1+ x) (+ 1 x))
(define (flo:atan2 x y) (atan x y))
(define (flo:log x) (log x))

(define int:= =)
(define int:- -)
(define int:+ +)
(define int:* *)
(define (int:negate x) (- x))
(define int:quotient quotient)
(define int:zero? zero?)

(define flo:= =)
(define flo:- -)
(define flo:+ +)
(define flo:* *)
(define flo:/ /)

(define (integer-divide x y)
  (cons (quotient x y)
	(remainder x y)))
(define integer-divide-quotient car)
(define integer-divide-remainder cdr)

(define true #t)
(define false #f)

(define* (generate-uninterned-symbol #:optional s)
  (if s
      (if (symbol? s) 
	  (gensym (symbol->string s))
	  (gensym s))
      (gensym)))
(define string->uninterned-symbol gensym)

(use-modules (ice-9 pretty-print))
(define* (pp object #:optional (output-port (current-output-port)) (as-code #f))
  (pretty-print object output-port))

(define (guarantee-exact-positive-integer x msg)
  (if (not (exact-positive-integer? x))
      (error msg x "not an exact positive integer")))

(define (guarantee-symbol x msg)
  (if (not (symbol? x))
      (error msg x "not a symbol")))

(define guile-procedure-arity
  (if (defined? 'procedure-minimum-arity)
      procedure-minimum-arity
      (lambda (proc)
	(assoc-ref (procedure-properties proc) 'arity))))

(define (procedure-arity proc)
  ; if apply hook, use arity of underlying procedure
  (let ((arity (or (procedure-property proc 'apply-hook-arity)
		   (guile-procedure-arity proc))))
    ; convert from guile arity: (min opt more-possible)
    ; to mit arity: (min . max) max is #f if more-possible
    (cons (car arity)
	  (and (not (caddr arity))
	       (+ (car arity) (cadr arity))))))

(define (guarantee-procedure-of-arity proc arity msg)
  (if (not (and (<= (car (procedure-arity proc))
		    arity)
		(or (not (cdr (procedure-arity proc)))
		    (>= (car (procedure-arity proc))
			arity))))
      (error msg proc "not of arity" arity)))



;;;;;; apply hook

; implementation of mit scheme apply-hook feature
; as a procedure that will return the apply-hook-extra
; if passed a special keyword argument.
; the arity of the real procedure is stored in the apply-hook-arity field
; of the procedure-properties.  the arity of the apply-hook is variable
; because of the special keyword arguments.
(define (make-apply-hook proc extra)
  (define aph
    (lambda* ( #:key get-apply-extra set-apply-extra set-apply-proc . args )
	     (cond ((eq? get-apply-extra #t)
		    extra)
		   (set-apply-extra
		    (set! extra (caddr args)))
		   (set-apply-proc
		    (set! proc (caddr args)))
		   (else (apply proc args)))))
  (set-procedure-property! aph 'apply-hook? #t)
  (if proc
      (set-procedure-property! aph 'apply-hook-arity 
			       (guile-procedure-arity proc)))
  aph)
  
; implementation of mit scheme apply-hook feature
; retrieve info in extra field after call to make-apply-hook
(define (apply-hook-extra proc)
  (proc #:get-apply-extra #t))
(define (set-apply-hook-extra! proc extra)
  (proc #:set-apply-extra #t extra))
(define (set-apply-hook-procedure! aph proc)
  (aph #:set-apply-proc #t proc)
  (if proc
      (set-procedure-property! aph 'apply-hook-arity 
			       (guile-procedure-arity proc))))


(define (apply-hook? proc)
  (and (procedure? proc) (procedure-property proc 'apply-hook?)))


;;;;; some list routines that are part of mit scheme

(define* (make-initialized-list n proc #:optional (start 0) )
  (if (eq? n 0)
      '()
      (cons (proc start)
	    (make-initialized-list (- n 1) proc (+ 1 start) ) ) ) )

(define (make-initialized-vector n proc)
  (list->vector (make-initialized-list n proc)))

(cond-expand (guile-2
	      )
	     (else
	      (if (not (defined? 'vector-copy))
		  (define (vector-copy v) (list->vector (vector->list v))))))

(define (find-matching-item l pred)
  (if (null? l)
      #f
      (if (pred (car l))
	  (car l)
	  (find-matching-item (cdr l) pred))))

(define (for-all? p l) (every l p))

;;;;; make-initialized-list uses a lot of stack space
(debug-set! stack 0)
