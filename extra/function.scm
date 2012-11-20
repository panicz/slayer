(define-module (extra function)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-11)
  #:use-module (extra common)
  #:export (free-variables procedure-args)
  #:export-syntax (proc))

(define (used-variables form)
  (define (diff . args) (apply lset-difference equal? args))
  (define (join . args) (apply lset-union equal? args))
  (define (bound-variables bindings)
    (let-values (((names forms) (unzip2 bindings)))
      (values names (append-map used-variables forms))))
  (define (argument-name arg)
    (cond ((symbol? arg) arg)
	  ((pair? arg) (car arg))
	  (else #f)))
  (match form 
    (((or 'let 'letrec 'letrec* 'with-fluids) (bindings ...) body ...)
     (let-values (((shadowed used) (bound-variables bindings)))
       (join used (diff (append-map used-variables body) (diff shadowed used)))))
    #;(('let (? symbol? name) (bindings ...) body ...)
     (let-values (((shadowed used) (bound-variables bindings)))
       (join used (diff (append-map used-variables body) (diff shadowed used) (list name)))))
    (('begin body ...)
     (append-map used-variables body))
    (((or 'lambda 'lambda*) arg body ...)
     (cond
      ((or (pair? arg) (list? arg))
       (diff (append-map used-variables body) 
	     (filter-map argument-name (properize arg))))
      ((symbol? arg)
       (diff (append-map used-variables body) (list arg)))))
    #;(((or 'define 'define*) (name ...) body ...)
     (diff (append-map used-variables body) name))
    (('define name value)
     (used-variables value))
    (((or 'case-lambda 'case-lambda*) def ...)
     (apply join 
	    (map (match-lambda ((arg body)
				(cond
				 ((symbol? arg)
				  (diff (append-map used-variables 
						    body) (list arg)))
				 ((or (pair? arg) (list? arg))
				  (diff (append-map used-variables 
						    body) 
					(filter-map 
					 argument-name 
					 (properize arg)))))))
		 def)))
    (('if expr ...)
     (append-map used-variables expr))
    (('quote data)
     '())
    #;(('quasiquote data)
     (letrec ((quasiquote-variables (match-lambda
				     (('unquote data) (used-variables data))
				     ((data ...) (append-map quasiquote-variables data))
				     (else '()))))
       (quasiquote-variables data)))
    #;(('@@ name ...)
     '())
    ((procedure ...)
     (append-map used-variables procedure))
    ((? symbol? variable)
       (list variable))
    (else
     '())))

(define (free-variables form)
  (used-variables 
   (expand form #:opts '(#:use-derived-syntax? #f #:avoid-lambda #f))))

(define-syntax proc
  (syntax-rules ()
    ((_ args body ...)
     (let ((p (lambda args body ...)))
       (set-procedure-property! p 'source '(lambda args body ...))
       p))))

(define (procedure-args proc)
  (match (procedure-source proc)
    (('lambda args body ...)
     args)
    (else
     (match (procedure-minimum-arity proc)
       ((required optional rest)
	(append (make-list required '_)
		(if (> optional 0) (cons #:optional 
					 (make-list optional '_)) '())
		(if rest '_ '())))
       (else
	'(..?))))))
