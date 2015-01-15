(define-module (editor rigged body)
  #:use-module (oop goops)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (extra figures)

  #:use-module (editor rigged object)
  #:export (<physical-body> properties describe-body))

(define-class <physical-body> (<editable-object>)
  (default-dimensions #:allocation #:class
    #:init-value '((sphere (radius . 0.5))
		   (box (x . 0.5) (y . 0.5) (z . 0.5))
		   (capsule (radius . 0.5) (height . 1.0))
		   (cylinder (radius . 0.5) (height . 1.0))
		   (trimesh)))
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
  (trimesh #:init-value #f)
  (trimesh-file #:init-value #f)
  (mesh 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (let ((shape #[self 'shape])
	   (dimensions #[self 'dimensions]))
       (or #[self 'trimesh]
	   #[self : 'mesh-cache : `(,shape ,@dimensions)]
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
	    (copy-tree #[self : 'default-dimensions : #[self 'shape]])))
  (for (property . value) in (keyword-args->alist args)
    (cond 
     ((and (eq? property 'mesh)
	   (eq? (first value) 'load-mesh))
      (match (with-input-from-file (second value) read)
	(('mesh . properties)
	 (set! #[self 'trimesh-file] value)
	 (set! #[self 'trimesh] `(mesh (color #f32(1 1 1 0.5)) . ,properties)))))

     ((in? property (map first #[self 'dimensions]))
      (set! #[self : 'dimensions : property] value))
     
     ((in? property (map first (class-slots (class-of self))))
      (set! #[self property] value))
     
     ((eq? property 'mass-distribution)
      (match value
	((center-of-mass . inertia-tensor)
	 (set! #[self 'center-of-mass] center-of-mass)
	 (set! #[self 'inertia-tensor] inertia-tensor))))
     
     (else
      (format #t "Unrecognised body property: ~a\n"  property)))))

(define-method (properties (body <physical-body>))
  (let ((common-properties `(#:mass ,#[body 'mass] 
				    #:position 
				    ,(f32vector #[body : 'position : 0]
						#[body : 'position : 1]
						#[body : 'position : 2])
				    #:quaternion ,#[body 'orientation]
				    #:mass-distribution 
				    (,#[body 'center-of-mass]
				     . ,#[body 'inertia-tensor])))
	(dimensions #[body 'dimensions]))
    (case #[body 'shape]
      ((box)
       `(box #:x ,#[dimensions 'x] 
	     #:y ,#[dimensions 'y]
	     #:z ,#[dimensions 'z]
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
      ((trimesh)
       `(trimesh #:mesh ,#[body 'trimesh-file]
		 ,@common-properties))
      )))

(define-method (describe-body (body <physical-body>))
  `(,#[body 'name] ,(properties #;of body)))
