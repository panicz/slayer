(keydn 'esc
  (lambda (type state code name mod unicode)
    (quit)))

(define (make-image source x y)
  (let* ((image (load-image source))
	 (image-object (make <image> #:image image #:x x #:y y #:w (image-width image) #:h (image-height image))))
    ; (slot-set! image-object 'click (lambda e (display source) (newline)))
    (slot-set! image-object 'drag 
	       (lambda (type state x y xrel yrel)
		 ;(display `(moving ,image-object from (,(slot-ref image-object 'x) ,(slot-ref image-object 'y)) by (,xrel ,yrel))) (newline)
		 (slot-set! image-object 'x
			    (+ (slot-ref image-object 'x)
			       xrel))
		 (slot-set! image-object 'y
			    (+ (slot-ref image-object 'y)
			       yrel))))
    image-object))

(add-child! *stage* (make-image "./ku.png" 50 50))

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
  (keydn 'c (lambda e (clear-screen))))
