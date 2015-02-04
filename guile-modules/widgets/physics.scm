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
  #:use-module (extra trimesh)
  #:use-module (slayer)
  #:use-module (slayer 3d)
  #:export (<physics-stage> <physical-object> body-object joint-object)
  #:re-export (<3d-view> 
	       <3d-editor> 
	       select-object! unselect-object! unselect-all!
	       relative-turn! relative-twist! relative-move!
	       X-SENSITIVITY Y-SENSITIVITY
	       ))

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
    ((capsule)
     (let ((radius (body-property body 'radius))
	   (height (body-property body 'height)))
       (match (generate-capsule #:radius radius
				#:height height)
	 (('mesh . data)
	  `(mesh ,@data #;,@(replace-alist-bindings 
		    data
		    (match (assoc-ref data 'faces)
		      (((type . data) ...) 
		       `((faces (,(case type
				    ((quad-strip) 'line-strip 'points)
				    ((quads) 'line-loop)
				    (else type))
				 . ,data) ...))))))))))

    ((plane)
     (square-grid #:size 100.0 #:density 50))
    ((trimesh)
     (let (((vertices . indices) (body-property body 'mesh)))
       (or #[*trimesh-cache* vertices]
	   `(mesh (vertices ,vertices)
		  (color #f32(1 1 1 0.5))
		  (faces (triangles ,indices))))))
    (else (error "Unknown body type of " body))))

(define-class <physical-object> (<3d-model>)
  (rig #:init-keyword #:rig)
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

(define-method (select-object! (object <physical-object>) #;in (view <3d-view>))
  (next-method)
  (format #t "~s\n" (body-name #[object 'body])))

(define-class <physical-joint> (<3d-model>)
  (rig #:init-keyword #:rig)
  (position
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (or (joint-property #[self 'joint] 'anchor) #f32(1 1 1)))
   #:slot-set!
   (lambda (self value)
     (set-joint-property! #[self 'joint] 'anchor value)))
  (orientation 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (case (joint-type #[self 'joint])
       ((hinge slider)
	(let ((axis (joint-property #[self 'joint] 'axis)))	  
	  (rotation-quaternion #;from #f32(0 0 1) #;to axis)))
       (else
	'(1.0 . #f32(0 0 0)))))
   #:slot-set!
   (lambda (self value)
     (set-joint-property! #[self 'joint] 'axis
			  (rotate #f32(0 0 1) #;by value))))
  (joint #:init-value #f #:init-keyword #:joint)
  )

(define-syntax-rule (access-joint-properties joint (prop ...) body . *)
  (let ((prop (joint-property joint 'prop)) ...)
    body . *))

(define (new-mesh-for-joint joint)
  (case (joint-type joint)
    ((hinge)
     (access-joint-properties joint (anchor axis angle hi-stop lo-stop body-1)
       (let* ((target (body-property body-1 'position))
	      (direction (normalized (- target anchor)))
	      (hi-stop-axis (if (finite? hi-stop)
				(rotate direction #;by hi-stop #;rads 
					#;around axis)
				direction))
	      (lo-stop-axis (if (finite? lo-stop)
				(rotate direction #;by lo-stop #;rads 
					#;around axis)
				direction)))
	 ;(<< (!# direction axis hi-stop lo-stop))
	 `(mesh
	   (colors #2f32((1 0 0)(1 1 0)
			 (0 0 1)(0 1 0)
			 (0 0 1)(0 1 0)
			 ))
	   (vertices ,(list->typed-array 
		       'f32 2
		       `((0 0 0) (0 0 1)
			 ,(uniform-vector->list direction)
			 #;,(uniform-vector->list lo-stop-axis))))
	   (faces (points #u8(0))
		  #;(lines #u8(0 1 0 2)))))))
    (else
     '(mesh (vertices #2f32((0 0 0)))
	    (faces (points #u8(0)))))))

(define-class <physics-stage> (<3d-stage>)
  (%body=>object #:init-thunk make-hash-table)
  (%joint=>object #:init-thunk make-hash-table)
  (%permanent-objects #:init-value '())
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
		   (when (not #[self : '%body=>object : body])
		     (set! #[self : '%body=>object : body]
			   (make <physical-object> 
			     #:body body
			     #:mesh (new-mesh-for-body body)
			     #:rig rig)))
		   (TODO check if the mesh hasn't changed and if it did,
			 modify accordingly! -- or just tie the specific 
			 parameters to certain locations to make sure that 
			 they're the same -- however, it might be difficult 
			 with a more advanced GPU architecture)
		   (let ((object #[self : '%body=>object : body]))
		     (set! #[object 'step] step)
		     (push! objects object)))
	      (for joint in (rig-joints rig)
		   (when (not #[self : '%joint=>object : joint])
		     (set! #[self : '%joint=>object : joint]
			   (make <physical-joint>
			     #:joint joint
			     #:mesh (new-mesh-for-joint joint)
			     #:rig rig)))
		   (let* ((object #[self : '%joint=>object : joint])
			  (mesh #[object 'mesh])
			  (the (lambda(prop)(joint-property joint prop))))
		     #;(match mesh
		       (('mesh . data)
			(set-values! (first #[data 'vertices])
				     `(,(the 'anchor)
				       ,(+ (the 'anchor) (the 'axis))))))
		     (push! objects object))))
	 (set! #[self '%objects-cache] objects)
	 (TODO remove all objects from %body=>object whose step
	       value hasn't been modified!)
	 (set! #[self '%last-synchronized-simulation-step] step))
       `(,@#[self '%permanent-objects] ,@#[self '%objects-cache])))
   #:slot-set! noop))

(define (body-object body #;from view)
  (find (lambda (object)
	  (and (is-a? object <physical-object>)
	       (eq? #[object 'body] body)))
	#[view : 'stage : 'objects]))

(define (joint-object joint #;from view)
  (find (lambda (object)
	  (and (is-a? object <physical-joint>)
	       (eq? #[object 'joint] joint)))
	#[view : 'stage : 'objects]))
