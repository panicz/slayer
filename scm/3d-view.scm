(use-modules (extra math)
	     (extra 3d))

(define-method (initialize (mesh <3d-mesh>) args)
  (next-method)
  ; (display "initializing mesh\n")
  (set! #[mesh 'mesh] 
	#;(generate-open-cylinder #:base-points 6)
	#;(hemisphere #:density 30)
	(generate-capsule)
	#;(generate-hemisphere #:radius 0.2)
	#;(generate-circle #:radius 0.2)
	#;(with-input-from-file "3d/cube.3d" read)))

(define (draw-mesh mesh)
  (match mesh
    (('mesh . definition)
     (for-each (match-lambda
		(('vertices (? array? array))
		 (set-vertices-array! array))
		(('colors (? array? array))
		 (set-colors-array! array))
		(('faces . faces)
		 (for-each (match-lambda 
			    ((type array)
			     (draw-faces! type array)))
			   faces)))
	       definition))
    (else
     (display `(no-match ,else)))))

(define-method (draw (object <3d-mesh>))
  (push-matrix!)
  (translate-view! #[object 'position])
  (rotate-view! #[object 'orientation])
  (draw-mesh #[object 'mesh])
  (pop-matrix!))

(define-class <3d-view> (<extended-widget>)
  (camera #:init-thunk (lambda()(make <3d-cam>)))
  (objects #:init-value '()))

(define-method (draw (view <3d-view>))
  (let ((original-viewport (current-viewport)))
    (set-viewport! #[view 'x] #[view 'y] #[view 'w] #[view 'h])
    (push-matrix!)
    (perspective-projection! #[view : 'camera : 'fovy])
    (translate-view! #[view : 'camera : 'position])
    (rotate-view! #[view : 'camera : 'orientation])
    (for-each draw #[view 'objects])
    (pop-matrix!)
    (apply set-viewport! original-viewport)))

(define-method (add-object! (view <3d-view>) (object <3d>))
  (set! #[view 'objects] (cons object #[view 'objects])))

(display "loaded 3d.scm\n")
