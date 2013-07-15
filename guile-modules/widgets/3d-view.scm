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
	    draw-mesh 
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



(define *lights* '())

(use-modules (extra common))

(define-method (draw-mesh mesh (context <3d-view>))
  (define (transform transformation-spec)
    (for-each (match-lambda
	       (('load-identity!)
		(load-identity!))
	       (('translate-view! (? array? vector))
		(translate-view! vector))
	       (('rotate-view! quaternion)
		(rotate-view! quaternion))
	       (else
		(<< "unrecognised transformation: "else)))
	      transformation-spec))
  (match mesh
    (('mesh . definition)
     (for-each (match-lambda
		(('vertices (? array? array))
		 (set-vertex-array! array))
		(('color color)
		 (set-color! color))
		(('colors (? array? array))
		 (set-color-array! array))
		(('normals (? array? array))
		 (set-normal-array! array))
		(('light . properties)
		 (let ((light (make-light)))
		   (for (property value) in (map-n 2 list properties)
			(set-light-property! 
			 light
			 (keyword->symbol property)
			 value))
		   #;(set! *lights* (cons light *lights*))))
		(('faces . faces)
		 (for-each (match-lambda ((type array)
					  (draw-faces! type array)))
			   faces))
		(('with-transformations 
		  transformations actions ...)
		 (push-matrix!)
		 (transform transformations)
		 (draw-mesh `(mesh ,@actions #;...))
		 (pop-matrix!))
		(else
		 (<< "no mathing pattern: "else))
		)
	       definition)
     (for-each forget-array! 
	       '(vertex-array color-array normal-array texture-coord-array)))
    (else
     (display `(no-match ,else)))))

(define-method (draw (object <3d-mesh>)(view <3d-view>))
  (push-matrix!)
  (translate-view! #[object 'position])
  (rotate-view! #[object 'orientation])
  (draw-mesh #[object 'mesh] view)
  (pop-matrix!))

(define-method (draw-objects (view <3d-view>))
  (for-each (lambda(object)(draw object view)) #[view 'objects]))

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
