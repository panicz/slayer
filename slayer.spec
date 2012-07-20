(keydn 'esc
  (lambda (type state code name mod unicode)
    (quit)))


(define *default-font* (load-font "./VeraMono.ttf" 12))
(set-font-style! *default-font* 1)

(add-child! *stage* (make-image (load-image "./tk4d.png") 50 50))
(add-child! *stage* (make-image (render-text "the game" *default-font*) 150 150))
(add-child! *stage* (make-image (load-image "./ku.png") 50 150))

;(set! hash{z} 20)

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


(keydn 't (lambda e (input-mode 'typing)))
