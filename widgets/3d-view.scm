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
			   faces)))
	       definition)
     (reset-state!))
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
    (translate-view! #[view : 'camera : 'position])
    (rotate-view! #[view : 'camera : 'orientation])
    (draw-objects view)
    (pop-matrix!)
    (apply set-viewport! original-viewport)))

(define-method (add-object! (view <3d-view>) (object <3d>))
  (set! #[view 'objects] (cons object #[view 'objects])))

(define-method (initialize (view <3d-view>) args)
  (next-method)
  (let ((camera #[view 'camera]))
    (set! #[view 'click]
	  (lambda (x y)
	    (set! #[view : 'data : 'anchor] (list x y))
	    (set! #[view : 'data : 'original-orientation]
		  #[camera 'orientation])))
    (set! #[view 'unclick]
	  (lambda (x y)
	    (hash-remove! #[view 'data] 'anchor)
	    (hash-remove! #[view 'data] 'original-orientation)))
    (set! #[view 'drag] 
	  (lambda (x y dx dy)
	    (match #[view : 'data : 'anchor]
	      ((x0 y0)
	       (let ((p #[view : 'data : 'original-orientation])
		     (vx (- x x0))
		     (vy (- y y0)))
		 (let* ((axis (wedge3x3 
			       (list->f32vector (list vx vy 0.0))
			       #f32(0.0 0.0 1.0)))
			(norm (norm axis))
			(θ (exact->inexact 
			    (/ norm (min #[view 'w] #[view 'h]))))
			(q (quaternion (sin θ) 
				       (* (cos θ)
					  (* (/ 1 norm) axis)))))
		   (if (> norm 0.1)
		       (set! #[camera 'orientation] 
			     (* (~ q) p q)))
		   )))
	      (else
	       (display "dragging with anchor unset (strange?)\n")))))))
