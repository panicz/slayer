(define-module (red body)
  #:use-module (oop goops)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (extra figures)

  #:use-module (red object)
  #:export (<physical-body> set-body-shape! properties describe-body
			    shift-body-shape!))

(define-class <physical-body> (<editable-object>)
  (default-dimensions #:allocation #:class
    #:init-value '((sphere (radius . 0.5))
		   (box (x . 0.5) (y . 0.5) (z . 0.5))
		   (capsule (radius . 0.5) (height . 1.0))
		   (cylinder (radius . 0.5) (height . 1.0))))
  (generators 
   #:allocation #:class
   #:init-value 
   `((sphere . ,generate-sphere)
     (capsule . ,generate-capsule)
     (cylinder . ,generate-tube)
     (box . ,generate-box)))
  (dimension-editors 
   #:allocation #:class
   #:init-value '()) ;; initalized elsewhere
  (shapes
   #:allocation #:virtual
   #:slot-ref (lambda(self)(map first #[self 'generators]))
   #:slot-set! noop)
  (%shape #:init-value 'box #:init-keyword #:shape)
  (dimensions #:init-value #f)
  (shape 
   #:allocation #:virtual
   #:slot-ref
   (lambda(self)
      #[self '%shape])
   #:slot-set!
   (lambda (self shape)
     (set! #[self '%shape] shape)
     (set! #[self 'dimensions] 
	   (copy-tree #[self : 'default-dimensions : shape]))))
  (name #:init-thunk (lambda()(gensym "body")) #:init-keyword #:name)
  (mass #:init-value 1.0 #:init-keyword #:mass)
  (center-of-mass #:init-value #f32(0 0 0) #:init-keyword #:center-of-mass)
  (inertia-tensor #:init-value #2f32((1 0 0)
				     (0 1 0)
				     (0 0 1)) #:init-keyword #:inertia-tensor)
  (mesh 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (let ((shape #[self 'shape])
	   (dimensions #[self 'dimensions]))
       (or #[self : 'mesh-cache : `(,shape ,@dimensions)]
	   (let ((mesh (apply #[self : 'generators : shape] 
			      (append-map (lambda ((name . value))
					    `(,(symbol->keyword name) ,value))
				   dimensions))))
	     (set! #[self : 'mesh-cache : (copy-tree `(,shape ,@dimensions))]
		   mesh)
	     mesh))))
   #:slot-set! noop))

(define-method (initialize (self <physical-body>) args)
  (next-method)
  (if (not #[self 'dimensions])
      (set! #[self 'dimensions]
	    (copy-tree #[self : 'default-dimensions : #[self 'shape]]))))

(define-method (set-body-shape! #;of (body <physical-body>) 
				     #;to (shape <symbol>) . args)
  (set! #[body 'shape] shape)
  (set! #[body 'dimensions]
	(replace-alist-bindings #;in #[body 'dimensions]
				     #;with (keyword-args->alist args)))
  (set! #[the-dimension-editor 'content]
	#[body : 'dimension-editors : shape])
  (set-target! #;of the-dimension-editor #;as body))

(define-method (properties (body <physical-body>))
  (let ((common-properties `(#:mass ,#[body 'mass] 
				    #:position ,#[body 'position]
				    #:mass-distribution 
				    (,#[body 'center-of-mass]
				     . ,#[body 'inertia-tensor])))
	(dimensions #[body 'dimensions]))
    (case #[body 'shape]
      ((box)
       `(box #:dimensions ,(f32vector #[dimensions 'x]
				      #[dimensions 'y]
				      #[dimensions 'z])
	     ,@common-properties))
      ((sphere)
       `(sphere #:radius ,#[dimensions 'radius]
		,@common-properties))
      ((cylinder)
       `(cylinder #:radius ,#[dimensions 'radius]
		  #:height ,#[dimensions 'height]
		  ,@common-properties))
      ((capsule)
       `(capsule #:radius ,#[dimensions 'radius]
		 #:height ,#[dimensions 'height]
		 ,@common-properties))
      )))

(define-method (describe-body (body <physical-body>))
  `(,#[body 'name] ,(properties body)))

(define-method (shift-body-shape! (body <physical-body>) #;by (n <integer>))
  (let* ((shapes #[body 'shapes])
	 (l (length shapes))
	 (new-shape (list-ref shapes
			      (modulo (+ (list-index (equals? #[body 'shape])
						     shapes)
					 n)
				      l))))
    (set-body-shape! body new-shape)))
