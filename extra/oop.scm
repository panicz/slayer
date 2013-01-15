(define-module (extra oop)

  #:use-module ((oop goops) #:hide (slot-ref slot-set!))
  #:use-module (extra ref)
  #:use-module (ice-9 match)
  #:use-module (extra common)

  #:use-module (extra hset)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)

  #:export (
	    class-tree-append-map-down
	    class-tree-append-map-up
	    class-ancestors
	    class-names+classes
	    superclass-layers
	    class-slot-names
	    class-specific-slot-names
	    make*
	    <write-and-display-signature>
	    signature

	    <call-before-slot-set!>
	    <unique-id>

	    *object-registry*
	    <registered-object>
	    <register-write-access>
	    remove!

	    modified?
	    reset-write-registry!
	    ))

;; this module is barely used

(define (class-tree-append-map-down f root)
  "class-tree-append-map-down returns a flat list containing the result of application of f to the class object in the root, appended with the list of all its descendants, appended with the list of all their descenants' descentants and so on. f should always return a list."
  (apply append (f root) 
	 (map (lambda(class)(class-tree-append-map-down f class)) 
	      (class-direct-subclasses root))))

(define (class-tree-append-map-up f root)
  (apply append (f root)
	 (map (lambda(class)(class-tree-append-map-up f class))
	      (class-direct-supers root))))

(define (class-ancestors class)
  (class-tree-append-map-up list class))

(define (class-slot-names class)
  (map first (class-slots class)))

(define (class-specific-slot-names class)
  (difference (class-slot-names class)
	      (append-map class-slot-names (class-direct-supers class))))

(define* (superclass-layers classes #:key (initial '()))
  (if (not (list? classes))
      (superclass-layers (list classes) #:initial initial)
      (let ((supers (apply union (map class-direct-supers classes))))
	(if (null? supers)
	    initial
	    (superclass-layers supers #:initial (append initial 
							(list supers)))))))

#;(define (superclass-layers class)
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
  (class-tree-append-map-down
   (lambda(class)
     (if (slot-bound? class 'name)
	 `((,(class-name class) ,class))
	 '()))
   root))

(define-class <write-and-display-signature> ())

(define-generic signature)

(define-method (signature (object <write-and-display-signature>))
  `(signature of ,(class-name (class-of object)) should be overloaded!))

(define-generic display)

(define-method (display (object <write-and-display-signature>) port)
  (display (signature object) port))

(define-generic write)

(define-method (write (object <write-and-display-signature>) port)
  (write (signature object) port))

(define-class <call-before-slot-set!> ())

(define-generic before-slot-set!)

(define-method (before-slot-set! (object <call-before-slot-set!>) slot-name value)
  ;(<< "calling `before-slot-set!` on " object " with " slot-name ", " value)
  (noop))

(define-method (slot-set! (object <call-before-slot-set!>) slot-name value)
  (before-slot-set! object slot-name value)
  (next-method))

(define-class <unique-id> (<write-and-display-signature>)
  (id #:init-thunk (lambda()(gensym "domain")) #:init-keyword #:id))

#;(define*-class <unique-id> (<register-write-access>)
  (id #:init-thunk (lambda()(gensym "domain")) #:allow-override #t
      #:on-write (lambda(this name value)
		   (hash-set! #[this '%%write-registry] name value))))

(define-method (signature (object <unique-id>))
  (list (class-name (class-of object)) (slot-ref object 'id)))

(define *object-registry* #[])


(define-class <registered-object> (<unique-id> <call-before-slot-set!>))

(define-method (before-slot-set! (object <registered-object>) slot-name value)
  ;;(<< `(SETTING SLOT ,slot-name OF ,object TO ,value))
  (cond ((equal? slot-name 'id)
	 (hash-remove! *object-registry* #[object 'id])
	 (if #[*object-registry* value] (throw 'id-already-exists))
	 (set! #[*object-registry* value] object)))
  (next-method))

(define-method (initialize (this <registered-object>) args)
  (next-method)
  #;(if (in? (class-name (class-of this)) '(<subspace> <passage>))
      (<< `(ADDING OBJECT ,this TO *object-registry*)))
  (set! #[*object-registry* #[this 'id]] this))

(define-generic remove!)

(define-method (remove! (object <registered-object>))
  (hash-remove! #[object 'registry] #[object 'id]))

(define-class <register-write-access> (<call-before-slot-set!>)
  (%%write-registry #:init-thunk make-hash-table))

(define-generic write-register-slots)

(define-method (write-register-slots (object <register-write-access>))
  (let ((peer-layer (find (lambda(layer)(in? <register-write-access> layer))
			   (superclass-layers 
			    (filter (lambda (class)
				      (in? <register-write-access>
					   (class-ancestors class)))
				    (class-direct-supers (class-of object)))))))
    (difference (class-slot-names (class-of object))
		(append-map class-slot-names peer-layer))))

(define-method (before-slot-set! (object <register-write-access>) slot-name value)
  (and-let* ((slot-names (write-register-slots object))
	     ((in? slot-name slot-names)))
    ;;(<< "REMEMBERING " slot-name)
    (set! #[object : '%%write-registry : slot-name] #t))
  (next-method))


(define-method (modified? (object <register-write-access>))
  (not (hash-empty? #[object '%%write-registry])))

(define-method (reset-write-registry! (object <register-write-access>))
  (for-each (\ hash-remove! #[object '%%write-registry] _) 
	    (hash-keys #[object '%%write-registry])))

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
