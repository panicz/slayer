(define-module (extra registry)
  #:use-module (extra attributes)
  #:use-module (extra common)
  #:use-module (ice-9 nice-9)
  #:export (either
	    both
	    maybe
	    registry-category
	    registry-category?
	    registry-entry?
	    default))

;; Categories are a little bit like structures/records except that
;; they are based on attribute lists, and each attribute has its type,
;; where by "type" we understand a predicate that is supposed to be
;; satisfied for an object of a given type.

;; These predicates are equipped with the "default" value, which
;; is obtained using the `default' procedure.


(define-syntax (define-procedure-property-accessor name)
  (define name
    (case-lambda
      ((type)
       (procedure-property type 'name))
      ((type value)
       (let ((property (procedure-property type 'name)))
	 
	 (cond  ((and property
		      (not (equal? property value)))
		 (let* ((properties (replace-alist-bindings
				     (filter (lambda ((key . value))
					       (not (member key '(arity))))
					     (procedure-properties type))
				     `((name . ,value))))
			(type* (lambda args (apply type args))))
		   (warn `(shadowing name for ,type))
		   (set-procedure-properties! type* properties)
		   type*))

		((equal? property value)
		 type)
		
		(else
		 (set-procedure-property! type 'name value)
		 type)))))))

(define-procedure-property-accessor default)

(default list? '())

(default number? 0)

(default integer? 0)

(default natural? 0)

(default complex? 0)

(default string? "")

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
    
    (registry-checker
     (default constructor
       (append-map (lambda ((attribute type))
		     `(,attribute ,(default type)))
		   attributes+types))
     (lambda (object)
       (and (attributes? object)
	    (every (lambda ((attribute type))
		     (type ((from object) attribute)))
		   attributes+types))))))

(define (registry-category? x)
  ;; this is not a good definition, because it only exploits
  ;; some implementation details of categories
  (and-let* (((procedure? x))
	     (((properties . _) ...) (procedure-properties x))
	     ((member 'default properties))
	     ((member 'registry-checker properties)))))

(define (registry-entry? #;of category)
  (define (belongs-to-category? object)
    (and-let* ((check (registry-checker category))
	       ((procedure? check)))
      (check object)))
  (default belongs-to-category?
    (default category)))

(define (category-member? record category)
  ((registry-entry? category) record))

(e.g.
 (let* ((example-category (registry-category
			   #:s (default string? "s")
			   #:string string?
			   #:number number?))
	(example-record (example-category #:number 5)))
   (and (registry-category? example-category)
	(instance? example-record example-category)
	(not (category-member? 5 example-category))
	(equal? example-record '(#:s "s" #:string "" #:number 5)))))

