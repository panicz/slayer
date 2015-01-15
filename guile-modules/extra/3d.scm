(define-module (extra 3d)
  :use-module (extra common)
  :use-module (extra ref)
  :use-module (oop goops)
  :use-module (extra math)
  :use-module (slayer)
  :use-module (slayer 3d)
  :export (
	   <3d> 
	   <3d-cam> 
	   <3d-cam-clone>
	   <3d-object>
	   <3d-model>
	   make-light
	   transform-matrix!
	   draw-mesh!
	   draw-object!
	   draw-contour!
	   setup-lights!
	   extract-lights
	   skip-lights
	   transform-mesh-vertices
	   x-axis y-axis z-axis
	   coordinate-system-mesh
	   )
  :export-syntax (replace/mesh)
  ;;:re-export (distance)
  )

(define x-axis #f32(1 0 0))
(define y-axis #f32(0 1 0))
(define z-axis #f32(0 0 1))

(define coordinate-system-mesh
  '(mesh 
    (vertices #2f32((0 0 0)(1 0 0)(0 1 0)(0 0 1)))
    (colors #2f32((1 1 1)(1 0 0)(0 1 0)(0 0 1)))
    (faces
     (lines #2u8((0 1)(0 2)(0 3))))))

;; #;(transform (v0 v1 v2 v3 v4 ...) ((v0 v1 v2)(v2 v1 v3)(v2 v3 v4) ...))
;; #;(transform (v0 v1 v2 v3 v4 v5 v6 v7 ...)
;;              ((v0 v1 v2)(v1 v2 v3)(v4 v5 v6)(v5 v6 v7)))

;; #;(transform (v0 v1 v2 v3 v4 ...)
;; 	     ((v0 v1 v2)(v0 v2 v3)(v0 v3 v4) ...))

(define-syntax define-symmetric-method
  (syntax-rules ()
    ((_ (name arg1 arg2) body ...)
     (begin
       (define-method (name arg1 arg2) body ...)
       (define-method (name arg2 arg1) body ...)))))

(define (transform-mesh-vertices proc mesh)
  (match mesh
    (('mesh . definition)
     `(mesh
       ,@(map (match-lambda
		  (('vertices vertices)
		   `(vertices ,(proc vertices)))
		(else
		 else))
	      definition)))))

(define-class <3d> ()
  (position #:init-value #f32(0 0 0) #:init-keyword #:position)
  (orientation #:init-value '(1.0 . #f32(0 0 0)) #:init-keyword #:orientation))

(define-method (distance (a <3d>) (b <3d>))
  (distance #[a 'position] #[b 'position]))

(define-class <3d-cam> (<3d>)
  (fovy #:init-value 70.0)
  (light #:init-thunk 
	 (lambda ()
	   (make-light 
	    #:ambient #f32(.3 .3 .3 1)
	    #:specular #f32(0 0 0 1)
	    #:diffuse #f32(1 1 1 1)))))

(define ((get-original property) self)
  #[#[self 'original] property])

(define ((set-original! property) self value)
  (if #[self 'read-only]
      (throw 'not-allowed)
      (set! #[#[self 'original] property] value)))

(define-class <3d-cam-clone> (<3d-cam>)
  (original #:init-keyword #:original #:init-value #f)
  (read-only #:init-keyword #:read-only #:init-value #t)
  (position #:allocation #:virtual
	    #:slot-ref (get-original 'position)
	    #:slot-set! (set-original! 'position))
  (orientation #:allocation #:virtual
	       #:slot-ref (get-original 'orientation)
	       #:slot-set! (set-original! 'orientation))
  (fovy #:allocation #:virtual
	#:slot-ref (get-original 'fovy)
	#:slot-set! (set-original! 'fovy))
  (light #:allocation #:virtual
	 #:slot-ref (get-original 'light)
	 #:slot-set! (set-original! 'light)))

(define-class <3d-object> (<3d>)
  (mesh #:init-value coordinate-system-mesh
	#:init-keyword #:mesh))

(define-class <3d-model> (<3d-object>)
  (%mesh #:init-value '(mesh))
  (%lights #:init-value '(mesh))
  (mesh #:allocation #:virtual
	#:slot-ref (lambda (this)
		     (append #[this '%lights] #[this '%mesh]))
	#:slot-set! (lambda (this mesh)
		      (let ((lights (extract-lights mesh))
			    (mesh (skip-lights mesh)))
			(set! #[this '%mesh] mesh)
			(set! #[this '%lights] lights)))))

(define-method (initialize (this <3d-model>) args)
  (next-method)
  (let-keywords args #t
		((mesh '(mesh)))
    (set! #[this 'mesh] mesh)))

(define (make-light . properties)
  (let ((light (make-light-)))
    (for (property value) in (map-n 2 list properties)
	 (set-light-property! light (keyword->symbol property) value))
    light))

(let-syntax ((mesh-extractor
	      (syntax-rules ()
		((_ extractor-name arg pattern+actions ...)
		 (match arg
		   (('mesh . definition)
		    `(mesh ,@(filter-map extractor-name definition)))
		   (('with-transforms transforms . actions)
		    (let ((subdefinition (filter-map extractor-name actions)))
		      (if (null? subdefinition)
			  #f
			  `(with-transforms ,transforms
					    ,@subdefinition))))
		   pattern+actions ...
		   (else
		    #f))))))
  
  (define-method (extract-lights (mesh <list>))
    (mesh-extractor extract-lights mesh
      (('light . properties) 
       `(light ,@properties))))

  (define-method (skip-lights (mesh <list>))
    (mesh-extractor skip-lights mesh
      (((? (lambda(x)(not (equal? x 'light))) command) . properties)
       `(,command ,@properties)))))

(define (transform-matrix! transform-spec)
  (for-each (match-lambda
	     (('load-identity!)
	      (load-identity!))
	     (('translate-view! (? array? vector))
	      (translate-view! vector))
	     (('rotate-view! quaternion)
	      (rotate-view! quaternion))
	     (('scale-view! factors ...)
	      (apply scale-view! factors))
	     (else
	      (throw 'unrecognised-transform)))
	    transform-spec))


(let-syntax (((process-mesh mesh-processor arg pattern+actions ...)
	      (match arg
		(('mesh . definition)
		 (for-each (match-lambda
			       (('with-transforms transforms . actions)
				(push-matrix!)
				(transform-matrix! transforms)
				(mesh-processor `(mesh ,@actions))
				(pop-matrix!))
			     pattern+actions ...
			     (else
			      (display `(mesh-processor 
					 match-failure: ,else)))
			     ) definition)
		 (for-each forget-array! 
		     '(vertex-array 
		       color-array 
		       normal-array 
		       texture-coord-array)))
		(else
		 (display `(mesh-processor invalid-mesh: ,else))))))

  (define-method (draw-mesh! (mesh <list>))
    (process-mesh draw-mesh! mesh
		  (('vertices (? array? array))
		   (set-vertex-array! array))
		  (('color color)
		   (set-color! color))
		  (('colors (? array? array))
		   (set-color-array! array))
		  (('normals (? array? array))
		   (set-normal-array! array))
		  (('faces . faces)
		   (for-each (lambda ((type array))
			       (draw-faces! type array))
		       faces))))

  (define-method (setup-lights! (lights <list>))
    (process-mesh setup-lights! lights
		  (('light . properties)
		   (let ((light (apply make-light properties)))
		     ;; the lights created here
		     ;; should be removed by the caller
		     (demand 'remove-light light #;after-rendering)))))
  )

(define-method (draw-mesh! (mesh <3d-model>))
  (draw-mesh! #[mesh '%mesh]))

(define-method (setup-lights! (object <3d-object>))
  (noop))

(define-method (setup-lights! (model <3d-model>))
  (push-matrix!)
  (translate-view! #[model 'position])
  (rotate-view! #[model 'orientation])
  (setup-lights! #[model '%lights])
  (pop-matrix!))

(define-syntax-rule (replace/mesh m (pattern replacement) ...)
  (letrec ((replace (lambda (primitive)
		      (match primitive
			(('with-transforms transforms . actions)
			 `(with-transforms 
			   ,transforms
			   ,@(map replace actions)))
			(pattern replacement)
			...
			(else primitive)))))
    (match m
      (('mesh . primitives)
       `(mesh ,@(map replace primitives))))))

(define-method (draw-object! (model <3d-object>))
  (push-matrix!)
  (translate-view! #[model 'position])
  (rotate-view! #[model 'orientation])
  (draw-mesh! #[model 'mesh])
  (pop-matrix!))

(define-method (draw-object! (model <3d-model>))
  (push-matrix!)
  (translate-view! #[model 'position])
  (rotate-view! #[model 'orientation])
  (draw-mesh! #[model '%mesh])
  ;;(draw-mesh! coordinate-system-mesh)
  (pop-matrix!))

(define-method (draw-contour! (model <3d-object>))
  (push-matrix!)
  (translate-view! #[model 'position])
  (rotate-view! #[model 'orientation])
  (scale-view! 1.1)
  (draw-mesh! (replace/mesh #[model 'mesh]
			    (((or 'color 'colors) . _)
			     `(color #f32(0 0.3 0.7 0.5)))))
  (pop-matrix!))

(define-method (draw-contour! (model <3d-model>))
  (push-matrix!)
  (translate-view! #[model 'position])
  (rotate-view! #[model 'orientation])
  (scale-view! 1.1)
  (draw-mesh! (replace/mesh #[model '%mesh]
			    (((or 'color 'colors) . _)
			     `(color #f32(0 0.3 0.7 0.5)))))
  (pop-matrix!))


#|
(define (projection-matrix fovy aspect near far)
  (and-let* (( (not (= aspect 0)))
	     (radians (* fovy 0.5 pi 1/180))
	     (sine (sin radians))
	     ( (not (= sine 0)))
	     (depth (- far near))
	     ( (not (= depth 0)))
	     (cotangent (/ (cos radians) sine)))
    (list->uniform-array
     (apply 
      map list ;i.e. transpose
      `((,(/ cotangent aspect) 0          0                             0)
	(0                     ,cotangent 0                             0)
	(0                     0          ,(- (/ (+ near far) depth))  -1)
	(0                     0          ,(* -2 near far (/ 1 depth))  0))))))  
|#
