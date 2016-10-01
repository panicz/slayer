(define-module (extra registry)
  #:use-module (extra attributes)
  #:use-module (extra common)
  #:use-module (ice-9 nice-9)
  #:export (either both maybe registry-category registry-entry? default))

(define-syntax (define-procedure-property-accessor name)
  (define name
    (case-lambda
      ((type)
       (procedure-property type 'name))
      ((type value)
       (when (procedure-property type 'name)
	 (warn `(redefinition of name for ,type)))
       (set-procedure-property! type 'name value)
       type))))

(define-procedure-property-accessor default)

(default list? '())

(default number? 0)

(default integer? 0)

(default natural? 0)

(default complex? 0)

(define (either . preds)
  (define (verify x)
    (any (lambda (pred)
	   (pred x))
	 preds))
  (default verify (and-let* (((first . _) preds))
		    (default first))))

(define (both . preds)
  (define (verify x)
    (every (lambda (pred)
	     (pred x))
	   preds))
  (default verify (and-let* (((first . _) preds))
		    (default first))))

(define (maybe pred)
  (define (verify x)
    (or (not x)
	(pred x)))
  (default verify #false))

(define-procedure-property-accessor registry-checker)

(define (registry-category . definition)
  ;; (assert (attributes? definition))
  (define (constructor . initial-values)
    (let ((object (default constructor)))
      (merge-attributes object initial-values)))
  (let ((attributes+types (chunks definition 2)))
    (default constructor
      (append-map (lambda ((attribute type))
		    `(,attribute ,(default type)))
		  attributes+types))
    (registry-checker
     constructor
     (lambda (object)
       (and (attributes? object)
	    (every (lambda ((attribute type))
		     (type ((from object) attribute)))
		   attributes+types))))))

(define (registry-entry? #;of category)
  (define (belongs-to-category? object)
    (and-let* ((check (registry-checker category))
	       ((procedure? check)))
      (check object)))
  (default belongs-to-category?
    (default category)))
