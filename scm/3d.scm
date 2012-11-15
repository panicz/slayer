(display "loading 3d.scm\n")

(use-modules (extra math))

(define *tolerance* 0.0001)
;; this needs to be fixed in the core guile

(define-class <3d> ()
  (position #:init-value #f32(0 0 0))
  (orientation #:init-value '(0 . #f32(1 0 0))))

(define-class <3d-cam> (<3d>)
  (fovy #:init-value 70.0))

(define-class <3d-shape> (<3d>)
  (shape #:init-value '()))

(define-class <3d-mesh> (<3d-shape>)
  (mesh #:init-thunk 
	(lambda()(with-input-from-file "3d/cube.3d" read))))


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

(define-class <3d-view> (<widget>)
  (camera #:init-thunk (lambda()(make <3d-cam>)))
  (objects #:init-value '()))

(define-method (draw (view <3d-view>))
  (let ((original-viewport (current-viewport)))
    (set-viewport! #[view 'x] #[view 'y] #[view 'w] #[view 'h])
    (push-matrix!)
    (perspective-projection! #[view : 'camera : 'fovy])
    (translate-view! #[view : 'camera : 'position])
    (rotate-view! #[view : 'camera : 'orientation])
    (for object in #[view 'objects]
	 (draw object))
    (pop-matrix!)
    (apply set-viewport! original-viewport)))

(define-method (add-object! (view <3d-view>) (object <3d>))
  (set! #[view 'objects] (cons object #[view 'objects])))

(display "loaded 3d.scm\n")
