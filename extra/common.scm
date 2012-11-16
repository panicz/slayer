(define-module (extra common)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 syncase)
  #:use-module (system base compile)
  #:export (
	    expand 
	    ?not ?and ?or in? 
	    map-n
	    array-map
	    contains-duplicates?
	    module->hash-map
	    module->list
	    module-symbols
	    hash-map->alist 
	    alist->hash-map
	    last-sexp-starting-position
	    properize
	    flatten
	    cart
	    take-at-most drop-at-most
	    remove-keyword-args
	    random-array
	    with-output-to-utf8
	    )
  #:export-syntax (\ for if*
		   safely symbol-list
		   transform! increase! decrease! multiply!
		   define-symmetric-method))

;(use-modules (srfi srfi-1) (srfi srfi-2) (srfi srfi-11) (ice-9 match) (ice-9 regex) (ice-9 syncase))

(define-syntax-rule (define-symmetric-method (name arg1 arg2) body ...)
  (begin
    (define-method (name arg1 arg2) body ...)
    (define-method (name arg2 arg1) body ...)))

(define-syntax safely 
  (syntax-rules ()
    ((_ sexp) 
     (catch #t (lambda () (values sexp #t))
       (lambda (key . args)
	 (with-output-to-port (current-output-port)
	   (lambda()
	     (backtrace)
	     (display `(error calling sexp : ,key ,args))
	     (values (if #f #f) #f))))))))

(define-syntax-rule (symbol-list symbol ...)
  `((symbol ,symbol) ...))

(define-syntax transform!
  (syntax-rules ()
    ((_ fx x args ...)
     (set! x (fx x args ...)))))
  
(define-macro (increase! x . args)
  `(transform! + ,x ,@args))

(define-macro (decrease! x . args)
  `(transform! - ,x ,@args))

(define-macro (multiply! x . args)
  `(transform! * ,x ,@args))

(define-syntax for
  (syntax-rules (in)
    ((_ x in list body ...)
     (for-each (match-lambda(x body ...)) list))))

(define-syntax if*
  (syntax-rules (in)
    ((_ condition then else)
     (let ((value condition))
       (if (not (unspecified? value))
	   (if value
	       then
	       else))))
    ((_ condition then)
     (let ((value condition))
       (if (not (unspecified? value))
	   (if value then))))))

(define* (in? obj list #:key (compare equal?))
  (any (lambda(x)(compare x obj)) list))

(define (properize src . dst)
  (cond ((pair? src)
	 (apply properize (cdr src) (cons (car src) dst)))
	((null? src)
	 (reverse dst))
	(else 
	 (reverse (cons src dst)))))

(define (array-map proc first . rest)
  (let ((dest (apply make-typed-array (array-type first) *unspecified* 
		     (array-dimensions first))))
    (apply array-map! dest proc first rest)
    dest))

(define* (module->hash-map #:optional (module (current-module)))
  (module-obarray module))

(define* (module->list #:optional (module (current-module)))
  (hash-map->list (lambda(key variable)
		    (cons key (variable-ref variable)))
		  (module->hash-map module)))

(define* (module-symbols #:optional (module (current-module)))
  (map car (module->list module)))

(define (hash-map->alist hash)
  (hash-map->list cons hash))

(define (alist->hash-map alist)
  (let ((h (make-hash-table)))
    (for-each (lambda(kv)(hash-set! h (car kv) (cdr kv))) alist)
    h))


;; (define (contains-duplicates? l)
;;   (call/cc (lambda(break)
;; 	     (let ((keys (make-hash-table)))
;; 	       (for-each (lambda(key)
;; 			   (if (hash-ref keys key)
;; 			       (break #t)
;; 			       (hash-set! keys key #t)))
;; 			 l))
;; 	     #f)))

(define (contains-duplicates? l)
  (let ((keys (make-hash-table)))
    (letrec ((next (lambda (l)
		     (cond ((null? l) #f)
			   ((hash-ref keys (car l)) #t)
			   (#t (hash-set! keys (car l) #t)
			       (next (cdr l)))))))
      (next l))))

(define-macro (\ f . args)
  (let* ((prefix "_")
	 (placeholder '_)
	 (ellipsis '...)
	 (rest (if (equal? (last args) ellipsis) ellipsis '()))
	 (args (if (symbol? rest) (drop-right args 1) args))
	 (max-arg 0)
	 (next-arg 
	  (lambda ()
	    (set! max-arg (+ max-arg 1))
	    (string->symbol (string-append 
			     prefix (number->string max-arg))))))
    (let ((args 
	   (map (lambda(arg)
		  (cond ((eq? arg placeholder)
			 (next-arg))
			((and-let* 
			     (((symbol? arg))
			      (arg-string (symbol->string arg))
			      (match-struct (string-match 
					     (string-append 
					      "^" 
					      (regexp-quote prefix) 
					      "([0-9]+)$")
					     arg-string))
			      (number (string->number 
				       (match:substring 
					match-struct 1))))
				(if (> number max-arg) 
				    (set! max-arg number))
				#t)
			 arg)
			(else arg))) 
		args)))
      `(lambda ,(append
		 (map (lambda (n) 
			(string->symbol 
			 (string-append prefix (number->string n)))) 
		      (iota max-arg 1))
		 rest)
	 ,(if (symbol? rest)
	      `(apply ,f ,@args ,rest)
	      `(,f ,@args))))))

(define* (expand e #:key (opts '()))
  (let-values (((exp env) (decompile 
			   (compile e #:from 'scheme #:to 'tree-il #:env (current-module))
			   #:from 'tree-il #:to 'scheme #:opts opts)))
    exp))

(define (unix-environment)
  (let ((env (make-hash-table)))
	(for-each (lambda(s)
		    (match-let (((name . values)
				 (string-split s #\=)))
			       (hash-set! env name (string-join values "="))))
		  (environ))
	env))

(define (cart . lists)
  (match lists
    (() '())
    ((only) (map list only))
    ((first . rest)
     (append-map (lambda(x)
		   (map (\ cons _ x)
			first))
		 (apply cart rest)))))

(define (?not pred)(lambda(x)(not (pred x))))

;; Note that (?and p? q? ...) is equivalent to (lambda(x)(and (p? x) (q? x)) ...)
;; (the same applies /mutatis mutandis/ to disjunction, i.e. ?or)
(define (?and . predicates)
  (lambda(x) (every (lambda(p)(p x)) predicates)))

(define (?or . predicates)
  (lambda(x) (any (lambda(p)(p x)) predicates)))

(define (map-n n fn lst . out)                                                            
   (if (< (length lst) n)
     out
     (apply map-n n fn (drop lst n) (append out (list (apply fn (take lst n)))))))

(define (last-sexp-starting-position str)
  (define opening-braces '(#\( #\[))
  (define closing-braces '(#\) #\]))
  (define braces (append opening-braces closing-braces))
  (define* (rewind #:key string while starting-from)
    (let loop ((pos starting-from))
      (if (and (>= pos 0) (while (string-ref string pos)))
	  (loop (1- pos))
	  pos)))
  (define (last-symbol-starting-position str init-pos)
    (rewind #:string str #:starting-from init-pos 
	    #:while (lambda(c)(and (not (char-whitespace? c)) (not (in? c braces))))))
  (define (last-whitespaces-starting-position str init-pos)
    (rewind #:string str #:starting-from init-pos #:while char-whitespace?))
  (define (last-string-starting-position str init-pos)
    (let loop ((pos init-pos))
      (if (and (eq? (string-ref str pos) #\")
	       (or (= pos 0) (not (eq? (string-ref str (- pos 1)) #\\))))
	  pos
	  (loop (1- pos)))))
  (let eat ((pos (- (string-length str) 1))
	    (level 0))
    (cond ((char-whitespace? (string-ref str pos))
	   (eat (last-whitespaces-starting-position str pos) level))
	  ((eq? (string-ref str pos) #\")
	   (if (= level 0)
	       (last-string-starting-position str (- pos 1))
	       (eat (- (last-string-starting-position str (- pos 1)) 1) level)))
	  ((not (in? (string-ref str pos) braces))
	   (if (= level 0)
	       (+ (last-symbol-starting-position str pos) 1)
	       (eat (last-symbol-starting-position str pos) level)))
	  ((in? (string-ref str pos) closing-braces)
	   (eat (- pos 1) (+ level 1)))
	  ((in? (string-ref str pos) opening-braces)
	   (cond ((= level 1)
		  (if (and (> pos 0)
			   (not (char-whitespace? (string-ref str (- pos 1))))
			   (not (in? (string-ref str (- pos 1)) braces)))
		      (let ((pos* (+ (last-symbol-starting-position str (- pos 1)) 1)))
			(if (and (in? (string-ref str pos*) '(#\# #\' #\`))
				 (not (eq? (string-ref str (+ pos* 1)) #\:)))
			    pos*
			    pos))
		      pos))
		 ((> level 1)
		  (eat (- pos 1) (- level 1)))
		 (#t
		  (error "mismatch braces")))))))

(define (cart . args)
  (let ((n (length args)))
    (cond ((= n 0) '())
	  ((= n 1) (map list (car args)))
	  (#t (append-map (lambda(x)
			    (map (lambda(y)
				   (cons y x))
				 (car args)))
			  (apply cart (cdr args)))))))

(define (flatten l)
  (if (list? l)
    (append-map flatten l)
    (list l)))

(define (take-at-most lst i)
  (take lst (min i (length lst))))

(define (drop-at-most lst i)
  (drop lst (min i (length lst))))

(define remove-keyword-args
  (letrec ((self (lambda(list)
		   (match list
		     (((? keyword?) (? (?not keyword?)) . rest)
		      (self rest))
		     (((? (?not keyword?) x) . rest)
		      (cons x (self rest)))
		     (((? keyword?) . rest)
		      (self rest))
		     (() '())))))
    self))

(define* (random-array #:key (range 1.0)(type #t)(mean 0) #:rest dims)
  (let ((dims (remove-keyword-args dims)))
    (array-map (lambda (mean) (+ mean (- (random (* 2 range)) range)))
		(apply make-typed-array type mean dims))))

(define (with-output-to-utf8 thunk)
  (string->utf8 (with-output-to-string thunk)))

;; do dalszej rozkminki
;; (define (collatz n)
;;   (cond 
;;    ((= n 1) 1)
;;    ((= (modulo n 2) 0) (collatz (/ n 2)))
;;    (else (collatz (+ (* 3 n) 1)))))

;; (every (lambda(x)(= x 1))(map collatz (iota 1000 1)))
