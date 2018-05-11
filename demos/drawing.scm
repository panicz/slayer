#!../src/slayer -r -d3d
!#
(use-modules (slayer)
	     (slayer image)
	     (slayer drawing)
	     (extra slayer)
	     (ice-9 nice-9))

(keydn 'esc quit)

(set-window-title! "Vector graphics demo")

(define image (rectangle 640 480 #xffffff))

(define generate
  (lambda ()
    (set-source-rgb! 1 1 1)
    (paint!)
    
    (set-source-rgb! 0 0 0)
    (move-to! 0 0)
    (line-to! 320 240)
    (move-to! 320 0)
    (line-to! 0 240)
    (set-line-width! 5)
    (stroke!)

    (rectangle! 0 0 160 120)
    (set-source-rgba! 1 0 0 0.5)
    (fill!)

    (rectangle! 0 120 160 120)
    (set-source-rgba! 0 1 0 0.3)
    (fill!)

    (rectangle! 160 0 160 120)
    (set-source-rgba! 0 0 1 0.7)
    (fill!)

    (set-source-rgb! 1 0 1)
    (stroke!)

    (move-to! 80 60)
    (set-source-rgb! 0.5 0.5 0.5)
    (set-font-size! 12)
    (set-font-face! "cairo:monospace")
    (show-text! "dupa")
    ))

(set-display-procedure! #;generate
			(lambda () 
   (draw-image! image 160 120)))

     #|
     (define (render-primitive! primitive)
     (match primitive
     (('line . args)
     (render-line! args))
     (('box . args)
     (render-box! args))
     (('shape . args)
     (render-shape! args))))

     (define (render! description)
     (match description
     (((with ((properties values) ...) . inner) . outer)
     (let ((old-values (map current-value properties)))
     (for-each set-value! properties values)
     (render! inner)
     (for-each set-value! properties old-values))
     (render! outer))
     ((first . rest)
     (render-primitive! first)
     (render! rest))
     (_
     (values))))
     
     (define shape
     |#     

(with-drawing-output-to-surface
 image generate)

(display (measure-without-drawing generate))


#;(set-display-procedure!
 (lambda ()
   (draw! '((shape #:contour-thickness 1.0 #:contour-color (1 0 0)
		   (0 0) (0 1) (1 1) (1 0))
	    (space #:position (5 5) #:size (2 2))
	    (caption #:position (10 10) #:text "dupa" #:font "cairo:monospace"
		     #:size 12)))))
