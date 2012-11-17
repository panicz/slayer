(define-module (extra 3d)
  :use-module (oop goops)
  :use-module (extra ref)
  :use-module (extra math)
  :use-module (extra common)
  :export (
	   <3d> 
	   <3d-cam> 
	   <3d-shape>
	   <3d-mesh>
	   ))

(define-class <3d> ()
  (position #:init-value #f32(0 0 0))
  (orientation #:init-value '(0 . #f32(1 0 0))))

(define-class <3d-cam> (<3d>)
  (fovy #:init-value 70.0))

(define-class <3d-shape> (<3d>)
  (shape #:init-value '()))

(define-class <3d-mesh> (<3d-shape>)
  (mesh #:init-value '() 
	#;(\ with-input-from-file "3d/cube.3d" read)))
