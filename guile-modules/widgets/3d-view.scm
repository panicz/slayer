(define-module (widgets 3d-view)
  #:use-module (oop goops)
  #:use-module (widgets base)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra ref)
  #:use-module (extra 3d)
  #:use-module (slayer 3d)
  #:export (<3d-view> 
	    add-object! 
	    draw-mesh! 
	    draw-objects
	    relative-turn!
	    relative-twist!
	    relative-move!
	    mouse->3d
	    X-SENSITIVITY
	    Y-SENSITIVITY))

(define-class <3d-view> (<extended-widget>)
  (camera #:init-thunk (lambda()(make <3d-cam>)))
  (objects #:init-value '()))

(define-method (initialize (mesh <3d-mesh>) args)
  (next-method)
  ; (display "initializing mesh\n")
  (set! #[mesh 'mesh] 
	#;(generate-open-cylinder #:base-points 6)
	#;(hemisphere #:density 30)
	;(generate-cube)
	;(square-grid #:size 10.0 #:density 50)
	(generate-capsule #:height 0.0)
	#;(generate-hemisphere #:radius 0.2)
	#;(generate-circle #:radius 0.2)
	#;(with-input-from-file "3d/basket.3d" read)))

(define (transform-matrix! transform-spec)
  (for-each (match-lambda
	     (('load-identity!)
	      (load-identity!))
	     (('translate-view! (? array? vector))
	      (translate-view! vector))
	     (('rotate-view! quaternion)
	      (rotate-view! quaternion))
	     (else
	      (<< "unrecognised transformation: "else)))
	    transform-spec))

(let-syntax ((process-mesh
	      (syntax-rules ()
		((_ mesh-processor arg pattern+actions ...)
		 (match arg
		   (('mesh . definition)
		    (for-each (match-lambda
			       (('with-transforms transformas . actions)
				(push-matrix!)
				(transform-matrix! transformations)
				(mesh-processor `(mesh ,actions))
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
		    (display `(mesh-processor invalid-mesh: ,else))))))))

  (define (draw-mesh! mesh)
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
		   (for-each (match-lambda ((type array)
					    (draw-faces! type array)))
			     faces))))

  (define (setup-lights! mesh)
    (process-mesh setup-lights! mesh
		  (('light . properties)
		   (let ((light (apply make-light properties)))
		     ;; the lights created here
		     ;; should be removed by the caller
		     (demand 'remove-light light)))))
  )

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
       `(,command ,@properties))))
  )

(define-method (extract-lights (object <3d-mesh>))
  (extract-lights #[object 'mesh]))

(define-method (draw (object <3d-mesh>))
  (push-matrix!)
  (translate-view! #[object 'position])
  (rotate-view! #[object 'orientation])
  (draw-mesh! (skip-lights #[object 'mesh]))
  (pop-matrix!))

(define-method (draw-objects (view <3d-view>))
  (let ((lights '()))
    (supply
     (((remove-light l)
       (push! lights l)))
     (let ((light-defs (map extract-lights #[view 'objects])))
       ;; lights need to be set up before the scene is rendered,
       ;; but they can be positioned in the same place as the vertices,
       ;; so we need to extract the lights (along with any matrix
       ;; transformations) from the mesh definition first
       (for-each setup-lights! light-defs)
       (for-each draw #[view 'objects])
       (for-each remove-light! lights)))))

(define-method (draw (view <3d-view>))
  (let ((original-viewport (current-viewport)))
    (apply set-viewport! (area view))
    (set-perspective-projection! #[view : 'camera : 'fovy])
    (push-matrix!)
    (translate-view! #f32(0 0 -0.1))
    (rotate-view! (~ #[view : 'camera : 'orientation]))
    (translate-view! (- #[view : 'camera : 'position]))
    (draw-objects view)
    (pop-matrix!)
    (apply set-viewport! original-viewport)))


(define-method (add-object! (view <3d-view>) (object <3d>))
  (set! #[view 'objects] (cons object #[view 'objects])))

(define X-SENSITIVITY (make-fluid 0.01))
(define Y-SENSITIVITY (make-fluid 0.01))

(define-method (relative-turn! (object <3d>) (x <number>) (y <number>))
  (set! #[object 'orientation]
	(normalized 
	 (+ #[object 'orientation] 
	    (* (quaternion 0.0 (* x #[X-SENSITIVITY]
				  (rotate #f32(0 1 0) #[object 'orientation])))
	       #[object 'orientation])
	    (normalized (+ #[object 'orientation]
			   (* (quaternion 0.0 
					  (* y #[Y-SENSITIVITY]
					     (rotate #f32(1 0 0)
						     #[object 'orientation])))
			      #[object 'orientation])))))))

(define-method (relative-twist! (object <3d>) axis)
  (set! #[object 'orientation]
	(normalized
	 (+ #[object 'orientation]
	    (* (quaternion 0.0 (rotate axis #[object 'orientation]))
	       #[object 'orientation])))))

(define-method (relative-move! (object <3d>) direction)
  (increase! #[object 'position]
	     (rotate direction #[object 'orientation])))

(define-method (mouse->3d (view <3d-view>) (x <integer>) (y <integer>))
  (let* ((camera #[view 'camera])
	 (matrix (*
		  (translation-matrix (- #[camera 'position]))
		  (quaternion->matrix (~ #[camera 'orientation]))

		 ))
	 (projection (perspective-projection
		      #[camera 'fovy]
		      (/ (* 1.0 #[view 'w]) #[view 'h]))))
    (unproject x y #f
	       matrix
	       projection
	       (area view))))
