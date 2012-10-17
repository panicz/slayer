
(keydn 'esc
       (function (type state code name mod unicode)
	 (quit)))

(keydn 'v
       (function e
	 (display (version)) (newline)))

(define ku (load-image "./ku.png"))

(add-child! *stage* (make-image ku 50 50))
(add-child! *stage* (make-image (render-text "the game" *default-font* #x000000 #xccaa22) 150 150))

(add-child! *stage* (make-image (rectangle 50 50 #x20eeaa22) 50 50))

;; (let ((ku (make-image (load-image "./ku.png")  50 150)))
;;   (make-timer 100 (lambda()
;; 		    (set! #[ ku 'x ] (+ #[ ku 'x ] 1))
;; 		    (set! #[ ku 'y ] (+ #[ ku 'y ] 1));;   (add-child! *stage* ku)))


;; (with-input-from-string "read" read)

(add-child! *stage* (make-text-area))

(keydn 'mouse1 
  (function (type name state x y)
    (and-let* ((w (widget-nested-find (function(w)
					(in-area? (list x y) (area w)))
				      *stage*)))
	      ;(display `(grabbing ,w with children at ,(map area (slot-ref w 'children))))(newline)
	      (set! *active-widget* w)
	      (#[ w 'click ] type name state x y))))

(keyup 'mouse1 (function (type name state x y) (set! *active-widget* *stage*)))

(mousemove (function (type state x y xrel yrel) 
	     (#[ *active-widget* 'drag ] type state x y xrel yrel)))

(keydn 'e
  (function e (display e)
	  (newline)))

(set-caption! "*SLAYER*")

(keydn 't (function e (input-mode 'typing)))

;(make-timer 1000 (function()(display "hello")) ) 
