(define-module (extra common)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 syncase)
  #:export (in? compose)
  #:export-syntax (\ ))

;(use-modules (srfi srfi-1) (srfi srfi-2) (ice-9 match) (ice-9 regex) (ice-9 syncase))

(define (in? obj list)
  (any (lambda(x)(equal? x obj)) list))

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

