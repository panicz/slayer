
(keydn 'esc
       (function (type state code name mod unicode)
	 (quit)))

(keydn 'v
       (function e
	 (display (version)) (newline)))

(define ku (load-image "./ku.png"))

(add-child! *stage* (make-image ku 75 25))
;(add-child! *stae* (make-button #:text "the game" #:x 150 #:y 150))
;(add-child! *stage* (make-image (rectangle 25 25 #x20eeaa22) 90 10))

(define 3d-object #f)
(define 3d-camera #f)
(if (defined? '<3d-view>)
    (begin 
      (set! 3d-object (make <3d-mesh>))
      (let ((view (make <3d-view> #:x 50 #:y 50 #:w 540 #:h 400)))
	(add-child! *stage* view)
	(set! 3d-camera #[view 'camera])
	(add-object! view 3d-object))))

;; (let ((ku (make-image (load-image "./ku.png")  50 150)))
;;   (make-timer 100 (lambda()
;; 		    (set! #[ ku 'x ] (+ #[ ku 'x ] 1))
;; 		    (set! #[ ku 'y ] (+ #[ ku 'y ] 1));;   (add-child! *stage* ku)))


;; (with-input-from-string "read" read)

(add-child! *stage* (make-text-area))

(keydn 'mouse1 
  (function (type name state x y)
    (and-let* ((w (widget-nested-find 
		   (lambda(w)
		     (in-area? (list x y)
			       (absolute-area w)))
		   *stage*)))
      ;;(shout (list 'ancestors (ancestors w) 'children (map absolute-area #[w 'children])))
      ;;(and-let* ((children #[w 'children])
      ;;((not (null? children))))
      ;;(shout (list 'ancestors-of-first-child (ancestors (first #;of children)))))
      ;;(shout (absolute-area w))
      ;;(if (is-a? w <bitmap>)
      ;;(shout (class-of w)))
      ;;(display `(grabbing ,w with children at ,(map area (slot-ref w 'children))))(newline)
      (set! *active-widget* w))
    (if *active-widget* (#[ *active-widget* 'click ] type name state x y))))

(keyup 'mouse1 (function (type name state x y) 
		 (if *active-widget* (#[ *active-widget* 'unclick ] type name state x y))
		 (set! *active-widget* *stage*)))

(keydn 'mouse2
       (function (type name state x y)
	 (and-let* ((w (widget-nested-find (lambda(w)
					     (in-area? (list x y)
						       (absolute-area w)))
					   *stage*)))
	   (#[ w 'right-click ] type name state x y))))

(mousemove (function (type state x y xrel yrel)
	     (let ((mouseover-widget 
		    (widget-nested-find (lambda (w) (in-area? (list x y) (absolute-area w))) *stage*)))
	       (if (and mouseover-widget (not (equal? mouseover-widget *nearby-widget*)))
		   (begin
		     (if *nearby-widget* (#[ *nearby-widget* 'mouse-out ] type state x y xrel yrel))
		     (set! *nearby-widget* mouseover-widget)
		     (#[ *nearby-widget* 'mouse-over ] type state x y xrel yrel)))
	       (#[ *active-widget* 'drag ] type state x y xrel yrel))))

(let ((menu (make-container #:x 0 #:y 0 #:name "menu" 
			    #:content (list (make-button #:text "button 1")
					    (make-button #:text "button 2")
					    (make-button #:text "button 3")))))
  (set! #[*stage* 'right-click]
	(function (type name state x y)
	  (if (not (find (lambda(x)(equal? x menu)) #[*stage* 'children]))
	      (add-child! *stage* menu))
	  (set! #[menu 'x] x)
	  (set! #[menu 'y] y)))
  (set! #[*stage* 'click]
	(function (type name state x y)
	  (remove-child! *stage* menu))))

(keydn 'e
  (function e (display e)
	  (newline)))

(set-caption! "*SLAYER*")

(keydn 't (function e (input-mode 'typing)))

(define (rotator deg axis)
  (let* ((rad (deg->rad deg))
	 (p (quaternion (cos rad) (* (sin rad) (normalized axis)))))
    (lambda(q)(* p q (~ p)))))


(if 3d-camera
    (begin
      (keydn 'right (function e 
		     (increase! #[3d-camera 'position] #(0.07 0 0))))
      (keydn 'left (function e 
		   (increase! #[3d-camera 'position] #(-0.07 0 0))))
      (keydn 'down (function e 
		     (increase! #[3d-camera 'position] #(0 0.07 0))))
      (keydn 'up (function e 
		      (increase! #[3d-camera 'position] #(0 -0.07 0))))
      (keydn "[" (function e 
		   (increase! #[3d-camera 'position] #f32(0 0 0.7))))
      (keydn "]" (function e 
		   (increase! #[3d-camera 'position] #f32(0 0 -0.7))))

      (keydn '= (function e 
		     (transform! 
		      (rotator (/ pi 12) #f32(0 0 1)) 
		      #[3d-camera 'orientation])))
      (keydn '- (function e 
		      (transform! 
		       (rotator (/ pi -12) #f32(0 0 1)) 
		       #[3d-camera 'orientation])))
      ))


;(make-timer 1000 (function()(display "hello")) ) 
