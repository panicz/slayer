(define-module (ice-9 nice-9)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:re-export (match)
  #:export ((and-let*/match . and-let*)
	    publish
	    primitive-lambda)
  #:replace ((cdefine . define)
	     (mlambda . lambda)
	     (named-match-let-values . let)
	     (match-let*-values . let*)
	     (let-syntax-rules . let-syntax)
	     (define-syntax/rule . define-syntax)))

(define-syntax define-syntax/rule
  (syntax-rules ()
    ((_ (name . pattern) . transformation)
     (define-syntax-rule (name . pattern) . transformation))

    ((_ name transformer)
     (define-syntax name transformer))

    ((_ name keywords (pattern template) ...)
     (define-syntax name
       (syntax-rules keywords
	 (pattern
	  template)
	 ...)))
    ))

(define-syntax let-syntax-rules/helper
  (syntax-rules ()
    ((_ () processed-bindings body . *)
     (let-syntax processed-bindings body . *))
    
    ((_ (((name pattern ...) template) bindings ...) (processed ...) body . *)
     (let-syntax-rules/helper
      (bindings ...) 
      (processed ... (name (syntax-rules () 
			     ((_ pattern ...) 
			      template))))
      body . *))

    ((_ ((name value) bindings ...) (processed ...) body . *)
     (let-syntax-rules/helper 
      (bindings ...) 
      (processed ... (name value))
      body . *))

    ((_ ((name keywords (pattern template) ...) bindings ...) (processed ...)
	body . *)
     (let-syntax-rules/helper
      (bindings ...)
      (processed ... (name (syntax-rules keywords 
			     (pattern 
			      template) 
			     ...)))
      body . *))
    ))

(define-syntax let-syntax-rules
  (syntax-rules ()
    ((_ (bindings ...) body . *)
     (let-syntax-rules/helper (bindings ...) () body . *))))

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
       #''(call-with-values (lambda () expression) 
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

;; The `publish' macro is used to provide means to separate public
;; definitions from private ones (such that are visible only from within
;; the public procedures and from within themselves).
;; For example, the form
;; 
;; (publish
;;   (define (f x) (+ a x))
;;   (define (g y) (* a y))
;;  where
;;   (define a 5))
;;
;; is equivalent to
;;
;; (begin
;;   (define f (and (defined? 'f) f))
;;   (define g (and (defined? 'g) g))
;;   (let ()
;;     (define a 5)
;;     (set! f (let () (define (f x) (+ a x)) f))
;;     (set! g (let () (define (g x) (* a y)) g))))

(define-syntax-rule (publish definitions ...)
  (publisher (definitions ...) ()))

(define-syntax publisher 
  (syntax-rules (where)
    ((_ (where private ...) (public ...))
     (private+public (private ...) (public ...)))
    ((_ (new defs ...) (approved ...))
     (publisher (defs ...) 
		(approved ... new)))))

(define-syntax private+public
  (lambda (stx)
    (define (sorted-private/interfaces+names+bodies private specs)
      ;; both sorting and name extraction takes place in the
      ;; same function called from with-syntax, because that
      ;; way we can tell the macro processor that the bindings in
      ;; the code belong to the same scope
      (define (interface-name interface)
	(match interface
	  ((head . tail)
	   (interface-name head))
	  ((? symbol? name)
	   name)))
      `(,(datum->syntax ;; this reordering is done, so that the (e.g. ...)
	  stx ;; forms can be freely mixed with definitions
	  (let-values (((definitions non-definitions)
			(partition (match-lambda 
				     (((? symbol? x) . _)
				      (string-contains (symbol->string x)
						       "def"))
				     (_ #f))
				   (syntax->datum private))))
	    `(,@definitions ,@non-definitions)))
	,(map (lambda (spec)
		(syntax-case spec ()
		  ((interface . body)
		   (datum->syntax stx `(,(syntax->datum #'interface)
					,(interface-name 
					  (syntax->datum #'interface))
					,(syntax->datum #'body))))))
	      specs)))
    (syntax-case stx ()
      ((_ (private ...) ((define-variant . spec) ...))
       (with-syntax ((((private ...) ((interface name body) ...))
		      (sorted-private/interfaces+names+bodies 
		       #'(private ...) #'(spec ...))))
	 #'(begin
	     (define name (and (defined? 'name) name))
	     ...
	     (let ()
	       private ...
	       (set! name
		     (let ()
		       (define-variant interface . body)
		       name))
	       ...)))))))
