(define-module (ice-9 nice-9)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:re-export (match)
  #:export ((and-let*/match . and-let*)
	    primitive-lambda)
  #:replace ((cdefine . define)
	     (mlambda . lambda)
	     (named-match-let-values . let)
	     (match-let*-values . let*)
	     (letrec-syntax/rules . letrec-syntax)
	     (let-syntax/rules . let-syntax)
	     (define-syntax/rules . define-syntax)))

;; This module extends the syntax of a few core forms so that their
;; "abuses" behave meaningfully -- in particular, it allows to
;; destructure bindings in "lambda" and "let" forms and use curried definitions.
;; It also provides pattern-matching version of "and-let*".

(define-syntax define-syntax/rules
  (syntax-rules ()
    ((_ (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
	 ((name . pattern) template))))

    ((_ name transformer)
     (define-syntax name transformer))

    ((_ name keywords (pattern template) ...)
     (define-syntax name
       (syntax-rules keywords
	 (pattern
	  template)
	 ...)))
    ))

(define-syntax let*-syntax/rules
  (syntax-rules ()
    ((_ let*-syntax () processed-bindings body . *)
     (let*-syntax processed-bindings body . *))
    
    ((_ let*-syntax (((name pattern ...) template) bindings ...) 
	(processed ...) body . *)
     (let*-syntax/rules
      let*-syntax
      (bindings ...) 
      (processed ... (name (syntax-rules () 
			     ((_ pattern ...) 
			      template))))
      body . *))

    ((_ let*-syntax ((name value) bindings ...) (processed ...) body . *)
     (let*-syntax/rules
      let*-syntax
      (bindings ...) 
      (processed ... (name value))
      body . *))

    ((_ let*-syntax ((name keywords (pattern template) ...) bindings ...)
	(processed ...)
	body . *)
     (let*-syntax/rules
      let*-syntax
      (bindings ...)
      (processed ... (name (syntax-rules keywords 
			     (pattern 
			      template) 
			     ...)))
      body . *))
    ))

(define-syntax let-syntax/rules
  (syntax-rules ()
    ((_ (bindings ...) body . *)
     (let*-syntax/rules let-syntax (bindings ...) () body . *))))

(define-syntax letrec-syntax/rules
  (syntax-rules ()
    ((_ (bindings ...) body . *)
     (let*-syntax/rules letrec-syntax (bindings ...) () body . *))))

(define-syntax mlambda
  (lambda (stx)
    (syntax-case stx ()

      ((_ (first-arg ... last-arg . rest-args) . body)
       (and (every identifier? #'(first-arg ... last-arg))
	    (or (identifier? #'rest-args) (null? #'rest-args)))
       #'(lambda (first-arg ... last-arg . rest-args) . body))

      ((_ arg body ...)
       (or (identifier? #'arg) (null? #'arg))
       #'(lambda arg body ...))

      ((_ args body ...)
       #'(match-lambda* (args body ...)))
      )))

(define-syntax primitive-lambda
  (syntax-rules ()
    ((_ . whatever)
     (lambda . whatever))))

(define-syntax cdefine
  (syntax-rules ()
    ((_ ((head . tail) . args) body ...)
     (cdefine (head . tail)
       (mlambda args body ...)))
    ((_ (name . args) body ...)
     (define name (mlambda args body ...)))
    ((_ . rest)
     (define . rest))
    ))

(define-syntax list<-values
  (syntax-rules ()
    ((_ call)
     (call-with-values (lambda () call) list))))

(define-syntax named-match-let-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((identifier expression) ...) ;; optimization: plain "let" form
	  body + ...)
       (every identifier? #'(identifier ...))
       #'(let ((identifier expression) ...)
	   body + ...))

      ((_ name ((identifier expression) ...) ;; optimization: regular named-let
	  body + ...)
       (and (identifier? #'name) (every identifier? #'(identifier ...)))
       #'(let name ((identifier expression) ...)
	   body + ...))

      ((_ name ((structure expression) ...)
	  body + ...)
       (identifier? #'name)
       #'(letrec ((name (mlambda (structure ...) body + ...)))
	   (name expression ...)))

      ((_ ((structure expression) ...)
	  body + ...)
       #'(match-let ((structure expression) ...) 
	   body + ...))

      ;; it should generally be discouraged to use the plain let
      ;; with multiple values, because there's no natural way to implement
      ;; that when there's more than one (multiple-value) binding,
      ;; but it's added for completeness
      ((_ ((structures ... expression) ...)
	  body + ...)
       #'(match-let (((structures ...) (list<-values expression)) ...)
	   body + ...))
      
      ((_ name ((structures ... expression) ...)
	  body + ...)
       (identifier? #'name) 
       #'(letrec ((loop 
		   (mlambda ((structures ...) ...)
			    (let-syntax ((name (syntax-rules ()
						 ((_ args (... ...))
						  (loop (list<-values args)
							(... ...))))))
			      body + ...))))
	   (loop (list<-values expression) ...)))
      )))

(define-syntax match-let*-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((identifier expression) ...) ;; optimization: regular let*
	  body + ...)
       (every identifier? #'(identifier ...))
       #'(let* ((identifier expression) ...)
	   body + ...))
      
      ((_ ((structure expression) ...)
	  body + ...)
       #'(match-let* ((structure expression) ...)
	   body + ...))

      ((_ ((identifier expression) remaining-bindings ...)
	  body + ...)
       (identifier? #'identifier)
       #'(let ((identifier expression))
	   (match-let*-values (remaining-bindings ...) body + ...)))

      ((_ ((structure expression) remaining-bindings ...)
	  body + ...)
       #'(match-let ((structure expression))
	   (match-let*-values (remaining-bindings ...) body + ...)))
      
      ((_ ((structure structures ... expression) remaining-bindings ...)
	  body + ...)
       #'(call-with-values (lambda () expression) 
	   (mlambda (structure structures ...)
		    (match-let*-values (remaining-bindings ...) body + ...))))
      )))

(define-syntax and-let*/match
  (lambda (stx)
    (syntax-case stx ()

      ((_)
       #'#t)

      ((_ ())
       #'#t)

      ((_ () body ...)
       #'(let () body ...))

      ((_ ((value binding) rest ...) body ...)
       (identifier? #'value)
       #'(let ((value binding))
	   (and value
		(and-let*/match (rest ...)
				body ...))))

      ((_ ((value binding) rest ...) body ...)
       #'(match binding
	   (value
	    (and-let*/match (rest ...)
	      body ...))
	   (_ #f)))

      ((_ ((condition) rest ...)
	  body ...)
       #'(and condition
	      (and-let*/match (rest ...)
		body ...)))

      )))
