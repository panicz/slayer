(define-module (widgets 3d-view)
  #:use-module (oop goops)
  #:use-module (widgets base)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra ref)
  #:use-module (extra 3d)
  #:use-module (slayer 3d)
  #:export (<3d-view> add-object! draw-mesh draw-objects transform-mesh-vertices))

(define-method (initialize (mesh <3d-mesh>) args)
  (next-method)
  ; (display "initializing mesh\n")
  (set! #[mesh 'mesh] 
	#;(generate-open-cylinder #:base-points 6)
	#;(hemisphere #:density 30)
	;(generate-cube)
	(square-grid #:size 10.0 #:density 50)
	;(generate-capsule #:height 0.0)
	#;(generate-hemisphere #:radius 0.2)
	#;(generate-circle #:radius 0.2)
	#;(with-input-from-file "3d/basket.3d" read)))

(define (draw-mesh mesh)
  (match mesh
    (('mesh . definition)
     (for-each (match-lambda
		(('vertices (? array? array))
		 (set-vertices-array! array))
		(('color color)
		 (set-color! color))
		(('colors (? array? array))
		 (set-colors-array! array))
		(('normals (? array? array))
		 (set-normal-array! array))
		(('faces . faces)
		 (for-each (match-lambda 
			    ((type array)
			     (draw-faces! type array)))
			   faces))
		(('push-matrix!)
		 (push-matrix!))
		(('pop-matrix!)
		 (pop-matrix!))
		(('load-identity!)
		 (load-identity!))
		(('translate-view! (? array? vector))
		 (translate-view! vector))
		(('rotate-view! (? array? quaternion))
		 (rotate-view! quaternion))
		)
	       definition)
     (for-each disable-client-state! 
	       '(vertex-array color-array normal-array texture-coord-array)))
    (else
     (display `(no-match ,else)))))

(define (transform-mesh-vertices proc mesh)
  (match mesh
    (('mesh . definition)
     `(mesh
       ,@(map (match-lambda
		  (('vertices vertices)
		   `(vertices ,(list->uniform-array 
				(map proc (array->list vertices)))))
		(else
		 else))
	      definition)))))

(define-method (draw (object <3d-mesh>))
  (push-matrix!)
  (translate-view! #[object 'position])
  (rotate-view! #[object 'orientation])
  (draw-mesh #[object 'mesh])
  (pop-matrix!))

(define-class <3d-view> (<extended-widget>)
  (camera #:init-thunk (lambda()(make <3d-cam>)))
  (objects #:init-value '()))

(define-method (draw-objects (view <3d-view>))
  (for-each draw #[view 'objects]))

(define-method (draw (view <3d-view>))
  (let ((original-viewport (current-viewport)))
    (set-viewport! #[view 'x] #[view 'y] #[view 'w] #[view 'h])
    (push-matrix!)
    (perspective-projection! #[view : 'camera : 'fovy])
    (translate-view! #f32(0 0 -0.1))
    (rotate-view! (~ #[view : 'camera : 'orientation]))
    (translate-view! (- #[view : 'camera : 'position]))

    (draw-objects view)
    (pop-matrix!)
    (apply set-viewport! original-viewport)))

(define-method (add-object! (view <3d-view>) (object <3d>))
  (set! #[view 'objects] (cons object #[view 'objects])))

(define X-SENSITIVITY (make-fluid 0.01))
(define Y-SENSITIVITY (make-fluid -0.01))

(define-method (turn (object <3d>) (x <number>) (y <number>))
  (set! #[object 'orientation]
	(normalized 
	 (+ #[object 'orientation] 
	    (* (quaternion 0.0 (* x #[X-SENSITIVITY] #f32(0 1 0)))
	       #[object 'orientation])
	    (normalized (+ #[object 'orientation]
			   (* (quaternion 0.0 (* y #[Y-SENSITIVITY] #f32(1 0 0)))
			      #[object 'orientation])))))))

(define-method (initialize (view <3d-view>) args)
  (next-method)
  (let ((camera #[view 'camera]))
    (set! #[view 'drag] (lambda (x y dx dy)
			  (turn #[view : 'camera] dx dy)))
    ))

