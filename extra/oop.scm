(define-module (extra oop)
  #:use-module (oop goops)
  #:use-module (ice-9 match)
  #:use-module (extra common)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:export (
	    class-tree-append-map
	    class-names+classes
	    superclass-layers
	    class-slot-names
	    class-specific-slot-names
	    make*
	    ))

;; this module is barely used

(define (class-tree-append-map f root)
  "class-tree-append-map returns a flat list containing the result of application of f to the class object in the root, appended with the list of all its descendants, appended with the list of all their descenants' descentants and so on. f should always return a list."
  (apply append (f root) 
	 (map (lambda(class)(class-tree-append-map f class)) 
	      (class-direct-subclasses root))))

(define (class-slot-names class)
  (map first (class-slots class)))

(define (class-specific-slot-names class)
  (difference (class-slot-names class)
	      (append-map class-slot-names (class-direct-supers class))))


(define (superclass-layers class)
  (define (superclasses-layers classes result)
    (let ((supers (apply union (map class-direct-supers classes))))
      (if (null? supers)
	  result
	  (superclasses-layers supers (cons supers result)))))
  (reverse (superclasses-layers (list class) '())))

#;(define (superclass-layers class)
  (let loop ((supers (class-direct-supers class))
	     (result '()))
    (if (null? supers)
	result
	(loop (apply union (map class-direct-supers supers))
	      (append result (list supers))))))

(define (class-names+classes root)
  (class-tree-append-map
   (lambda(class)
     (if (slot-bound? class 'name)
	 `((,(class-name class) ,class))
	 '()))
   root))

(define-macro (define*-class <class-name> supers . slots)
  (define keywords* '(#:init-keyword))
  (define slot-options (list #:init-thunk #:init-keyword  #:init-value 
			     #:init-form #:allocation #:getter #:setter
			     #:accessor #:slot-ref #:slot-set!))
  (define (%slot-properties . property-list)
    (concatenate (filter (match-lambda ((keyword value)
					(in? keyword slot-options)))
			 (map-n 2 list property-list))))
  (define (direct-slot? slot-def)
    (match slot-def
      ((slot-name . property-list)
       (let ((properties (kw-list->hash-map property-list)))
	 (not (or (hash-ref properties #:on-write) 
		  (hash-ref properties #:on-read)
		  (hash-ref properties #:allow-override)
		  (equal? (hash-ref properties #:allocation) #:virtual)))))
      ((? symbol? slot-name)
       #t)))
  (define (transform-keywords slot-name property-list)
    (concatenate
     (map-n 2 (match-lambda* 
		  ((#:init-keyword #t)
		   (list #:init-keyword (symbol->keyword slot-name)))
		((keyword value) 
		 (list keyword value)))
	    property-list)))
  (define (resolve slot-def)
    (match slot-def
      ((slot-name . property-list)
       (if (direct-slot? slot-def)
	   `((,slot-name ,@(transform-keywords slot-name property-list)))
	   ;;else we create two slots
	   (let ((%slot-name (symbol-append '% slot-name))
		 (properties (kw-list->hash-map property-list)))
	     `((,%slot-name ,@(transform-keywords slot-name
						  (%slot-properties 
						   property-list)))
	       (,slot-name #:allocation #:virtual
			   #:slot-ref
			   (lambda(this)
			     ,@(or (and-let* ((on-read (hash-ref properties 
								 #:on-read)))
				     `((,on-read this ',slot-name)))
				   '())
			     (slot-ref this ,%slot-name))
			   #:slot-set!
			   (lambda (this value)
			     ,@(or (and-let* ((on-write (hash-ref 
							 properties #:on-write)))
				     `((,on-write this ',slot-name value)))
				   '())
			     (slot-set! this ,%slot-name)))))))
	  ((? symbol? slot-name)
	   `(,slot-name))))
  `(define-class* ,<class-name> ,supers
     ,@(append-map resolve slots)))

(define (make* class . args)
  (let ((object (make class)))
    (apply for-each (lambda(key value)
		      (slot-set! object (keyword->symbol key) value))
	   (apply map list (map-n list args 2)))
    object))
