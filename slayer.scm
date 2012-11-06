(keydn 'esc (function()(quit)))

(keydn 'v
       (function ()
	 (display (version)) (newline)))

(define ku (load-image "./ku.png"))

(add-child! *stage* (make-image ku 75 25))

(define 3d-object #f)
(define 3d-camera #f)
(if (defined? '<3d-view>)
    (begin 
      (set! 3d-object (make <3d-mesh>))
      (let ((view (make <3d-view> #:x 50 #:y 50 #:w 540 #:h 400)))
	(add-child! *stage* view)
	(set! 3d-camera #[view 'camera])
	(add-object! view 3d-object))))

(add-child! *stage* (make-text-area))

(keydn 'mouse1 
  (function (x y)
    (and-let* ((w (widget-nested-find 
		   (lambda(w)
		     (in-area? (list x y)
			       (absolute-area w)))
		   *stage*)))
      (set! *active-widget* w))
    (if *active-widget* (#[ *active-widget* 'click ] x y))))

(keyup 'mouse1 (function (x y) 
		 (if *active-widget* (#[ *active-widget* 'unclick ] x y))
		 (set! *active-widget* *stage*)))

(keydn 'mouse2
       (function (x y)
	 (and-let* ((w (widget-nested-find 
			(lambda(w)
			  (in-area? (list x y)
				    (absolute-area w)))
			*stage*)))
	   (#[ w 'right-click ] x y))))

(mousemove (function (x y xrel yrel)
	     (let ((mouseover-widget 
		    (widget-nested-find (lambda (w) (in-area? (list x y) (absolute-area w))) *stage*)))
	       (if (and mouseover-widget (not (equal? mouseover-widget *nearby-widget*)))
		   (begin
		     (if *nearby-widget* (#[ *nearby-widget* 'mouse-out ] x y xrel yrel))
		     (set! *nearby-widget* mouseover-widget)
		     (#[ *nearby-widget* 'mouse-over ] x y xrel yrel)))
	       (#[ *active-widget* 'drag ] x y xrel yrel))))

(let ((menu (make-container #:x 0 #:y 0 #:name "menu" 
			    #:content (list (make-button #:text "button 1")
					    (make-button #:text "button 2")
					    (make-button #:text "button 3")))))
  (set! #[*stage* 'right-click]
	(function (x y)
	  (if (not (find (lambda(x)(equal? x menu)) #[*stage* 'children]))
	      (add-child! *stage* menu))
	  (set! #[menu 'x] x)
	  (set! #[menu 'y] y)))
  (set! #[*stage* 'click]
	(function (x y)
	  (remove-child! *stage* menu))))

(define (rotator deg axis)
  (let* ((rad (deg->rad deg))
	 (p (quaternion (cos rad) (* (sin rad) (normalized axis)))))
    (lambda(q)(* p q (~ p)))))

(if 3d-camera
    (begin
      (keydn 'right (function () 
		     (increase! #[3d-camera 'position] #(0.07 0 0))))
      (keydn 'left (function ()
		   (increase! #[3d-camera 'position] #(-0.07 0 0))))
      (keydn 'down (function ()
		     (increase! #[3d-camera 'position] #(0 0.07 0))))
      (keydn 'up (function ()
		      (increase! #[3d-camera 'position] #(0 -0.07 0))))
      (keydn "[" (function () 
		   (increase! #[3d-camera 'position] #f32(0 0 0.7))))
      (keydn "]" (function ()
		   (increase! #[3d-camera 'position] #f32(0 0 -0.7))))

      (keydn '= (function ()
		     (transform! 
		      (rotator (/ pi 12) #f32(0 0 1)) 
		      #[3d-camera 'orientation])))
      (keydn '- (function ()
		      (transform! 
		       (rotator (/ pi -12) #f32(0 0 1)) 
		       #[3d-camera 'orientation])))

      (keydn 'd (function () 
		     (increase! #[3d-object 'position] #(0.07 0 0))))
      (keydn 'a (function ()
		   (increase! #[3d-object 'position] #(-0.07 0 0))))
      (keydn 's (function ()
		     (increase! #[3d-object 'position] #(0 0.07 0))))
      (keydn 'w (function ()
		      (increase! #[3d-object 'position] #(0 -0.07 0))))
      (keydn 'r (function () 
		   (increase! #[3d-object 'position] #f32(0 0 0.7))))
      (keydn 'f (function ()
		   (increase! #[3d-object 'position] #f32(0 0 -0.7))))

      (keydn 'e (function ()
		     (transform! 
		      (rotator (/ pi 12) #f32(0 0 1)) 
		      #[3d-object 'orientation])))
      (keydn 'q (function ()
		      (transform! 
		       (rotator (/ pi -12) #f32(0 0 1)) 
		       #[3d-object 'orientation])))

      ))


;(make-timer 1000 (function()(display "hello")) ) 
