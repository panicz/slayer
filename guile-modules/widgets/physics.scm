(define-module (widgets physics)
  #:use-module (scum physics)
  #:use-module (widgets base)
  #:use-module (widgets 3d)
  #:use-module (oop goops)
  #:use-module (extra 3d)
  #:use-module (extra math)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra figures)
  #:use-module (slayer)
  #:use-module (slayer 3d)
  #:export (<physics-stage> <physical-object>)
  #:re-export (<3d-view>
	       relative-turn!
	       relative-twist!
	       relative-move!
	       X-SENSITIVITY
	       Y-SENSITIVITY))

(define (new-mesh-for-body body)
  (case (body-type body)
    ((box)
     (let ((dims (body-property body 'dimensions)))
       (generate-box #:x #[dims 0] 
		     #:y #[dims 1] 
		     #:z #[dims 2])))
    ((sphere)
     (let ((radius (body-property body 'radius)))
       (generate-sphere #:radius radius)))
    ((cylinder)
     (let ((radius (body-property body 'radius))
	   (height (body-property body 'height)))
       (generate-tube #:radius radius
		      #:height height)))
    ((plane)
     (square-grid #:size 10.0 #:density 50))
    ((trimesh)
     (match-let (((vertices . indices)
		  (body-property body 'mesh)))
       `(mesh (vertices ,vertices)
	      (color #f32(0.7 0 0))
	      (faces (triangles ,indices)))))
    (else (error "Unknown body type of " body))))

(define-class <physical-object> (<3d-model>)
  (position
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (body-property #[self 'body] 'position))
   #:slot-set!
   (lambda (self value)
     (set-body-property! #[self 'body] 'position value)))
  (orientation 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (body-property #[self 'body] 'quaternion))
   #:slot-set!
   (lambda (self value)
     (set-body-property! #[self 'body] 'quaternion value)))
  (step #:init-value #f #:init-keyword #:step)
  (body #:init-value #f #:init-keyword #:body))

(define-method (initialize (self <physical-object>) args)
  (next-method))

(define-class <physics-stage> (<3d-stage>)
  (%body=>object #:init-thunk make-hash-table)
  (%objects-cache #:init-value '())
  (%last-synchronized-simulation-step #:init-value #f)
  (simulation #:init-value #f #:init-keyword #:simulation)
  (objects
   #:allocation #:virtual
   #:slot-ref 
   (lambda (self)
     (and-let* ((simulation #[self 'simulation])
		(step (current-simulation-step simulation))
		(objects '()))
       (unless (eq? step #[self '%last-synchronized-simulation-step])
	 (for rig in (simulation-rigs #[self 'simulation])
	      (for body in (rig-bodies rig)
		   (if (not #[self : '%body=>object : body])
		       (set! #[self : '%body=>object : body]
			     (make <physical-object> 
			       #:body body
			       #:mesh (new-mesh-for-body body))))
		   (TODO check if the mesh hasn't changed and if it did,
			 modify accordingly!)
		   (let ((object #[self : '%body=>object : body]))
		     (set! #[object 'step] step)
		     (push! objects object))))
	 (set! #[self '%objects-cache] objects)
	 (TODO remove all objects from %body=>object whose step
	       value hasn't been modified!)
	 (set! #[self '%last-synchronized-simulation-step] step))
       #[self '%objects-cache]))
   #:slot-set! noop))
