(keydn 'esc
  (lambda (type state code name mod unicode)
    (quit)))

(define (make-image image x y)
  (let ((image (make <image> #:image image #:x x #:y y 
		     #:w (image-width image) 
		     #:h (image-height image))))
    (slot-set! image 'drag 
	       (lambda (type state x y xrel yrel)		 
		 (slot-set! image 'x (+ (slot-ref image 'x) xrel))
		 (slot-set! image 'y (+ (slot-ref image 'y) yrel))))
    image))


(define *default-font* (load-font "./VeraMono.ttf" 12))
(set-font-style! *default-font* 1)

(add-child! *stage* (make-image (load-image "./tk4d.png") 50 50))
(add-child! *stage* (make-image (render-text "the game" *default-font*) 150 150))
(add-child! *stage* (make-image (load-image "./ku.png") 50 150))

(keydn 'mouse1 
  (lambda (type name state x y)
    (and-let* ((w (widget-nested-find (lambda(w)
					(in-area? (list x y) (area w)))
				      *stage*)))
	      ;(display `(grabbing ,w with children at ,(map area (slot-ref w 'children))))(newline)
	      (set! *active-widget* w)
	      ((slot-ref w 'click) type name state x y))))

(keyup 'mouse1 (lambda (type name state x y) (set! *active-widget* *stage*)))

(mousemove (lambda (type state x y xrel yrel) 
	     ((slot-ref *active-widget* 'drag) type state x y xrel yrel)))

(keydn 'e
  (lambda e (display e)
	  (newline)))

(set-caption "*SLAYER*")

(let ((tk4d (load-image "./tk4d.png"))
      (ku (load-image "./ku.png")))
  (keydn 't (lambda e (draw-image tk4d 20 20)))
  (keydn 'k (lambda e (draw-image ku 50 90)))
  ;(keydn 'y (lambda e (draw-image  20 100)))
  (keydn 'c (lambda e (clear-screen))))
