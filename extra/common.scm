(define-module (extra common)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 syncase)
  #:use-module (system base compile)
  #:export (in? compose expand unix-environment ?not ?and ?or map-n
		contains-duplicates)
  #:export-syntax (\ ))

;(use-modules (srfi srfi-1) (srfi srfi-2) (srfi srfi-11) (ice-9 match) (ice-9 regex) (ice-9 syncase))

(define* (in? obj list #:key (compare equal?))
  (any (lambda(x)(compare x obj)) list))

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

(define (compose f . rest)
  (if (null? rest)
      f
      (let ((g (apply compose rest)))
        (lambda args
          (call-with-values (lambda () (apply g args)) f)))))

(define-macro (\ f . args)
  (let* ((prefix "\\")
	 (placeholder '\.)
	 (max-arg 0)
	 (next-arg (lambda ()
		     (set! max-arg (+ max-arg 1))
		     (string->symbol (string-append 
				      prefix (number->string max-arg))))))
    (let ((args (map (lambda(arg)
		       (cond ((eq? arg placeholder)
			      (next-arg))
			     ((and-let* (((symbol? arg))
					 (arg-string (symbol->string arg))
					 (match-struct (string-match 
							(string-append "^" (regexp-quote prefix) "([0-9]+)$")
							arg-string))
					 (number (string->number (match:substring match-struct 1))))
				(if (> number max-arg) (set! max-arg number))
				#t)
			      arg)
			     (else arg))) args)))
      `(lambda ,(map (lambda (n) (string->symbol (string-append prefix (number->string n)))) (iota max-arg 1))
	 (,f ,@args)))))

(define (expand e)
  (let-values (((exp env) (decompile 
			   (compile e #:from 'scheme #:to 'tree-il #:env (current-module))
			   #:from 'tree-il #:to 'scheme)))
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
		   (map (\ cons \1 x)
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

