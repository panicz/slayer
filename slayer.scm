(keydn 'esc (function()(quit)))

(set-display-procedure! (lambda()(draw *stage*)))

(keydn 'v
       (function ()
	 (display (version)) (newline)))

(define ku (load-image "./art/ku.png"))

(add-child! *stage* (make-image ku 75 25))

(define 3d-object #f)

(define 3d-camera #f)

(load "game.scm")

(if (defined? '<3d-view>)
    (begin 
      (set! 3d-object (make <3d-mesh>))
      (let ((view (make <3d-view> #:x 50 #:y 50 #:w 260 #:h 400)))
	(add-child! *stage* view)
	(set! #[view 'click]
	      (function (x y)
		(set! #[view : 'data : 'anchor] (list x y))
		(set! #[view : 'data : 'original-orientation] 
		      #[3d-object 'orientation])))
	(set! #[view 'unclick]
	      (function (x y)
		(hash-remove! #[view 'data] 'anchor)
		(hash-remove! #[view 'data] 'original-orientation)))
	(set! #[view 'drag] 
	      (function (x y dx dy)
		(match #[view : 'data : 'anchor]
		  ((x0 y0)
		   (let ((p #[view : 'data : 'original-orientation])
			 (vx (- x x0))
			 (vy (- y y0)))
		     (let* ((axis (wedge3x3 
				   (list->f32vector (list vx vy 0.0))
				   #f32(0.0 0.0 1.0)))
			    (norm (norm axis))
			    (θ (exact->inexact 
				(/ norm (min #[view 'w] #[view 'h]))))
			    (q (quaternion (sin θ) 
					   (* (cos θ)
					      (* (/ 1 norm) axis)))))
		       (if (> norm 0.1)
			   (set! #[3d-object 'orientation] 
				 (* (~ q) p q)))
		     )))
		  (else
		   (display "dragging with anchor unset (strange?)\n")))))
	(set! 3d-camera #[view 'camera])
	(add-object! view 3d-object))))

(if (defined? '<network-3d-view>)
    (begin
      (let ((gv (make <network-3d-view> #:x 320 #:y 50 
		      #:w 260 #:h 400 
		      #:address "127.0.0.1:41337"
		      #:types (export-types <player>)
		      
		      )))
	(request gv '(owned-objects)
		 (lambda(owned-objects)
		   (begin
		     (display `(owned objects: ,owned-objects))
		     (newline))
		   (match owned-objects
		     ((('<player> id) _ ...)
		      (keydn 'space (\ command gv `(jump)))
		      (keydn 'f1 (\ command gv `(turn 5)))
		      (keydn 'lshift (\ command gv `(crouch)))
		      (keydn 'lalt (\ display "LEFT ALT\n"))
		      ))))
	(add-child! *stage* gv))
      ))

(add-child! *stage* (make-text-area))

(keydn 
 'mouse1 
 (function (x y)
   (and-let* ((w (widget-nested-find 
		  (lambda(w)
		    (in-area? (list x y)
			      (absolute-area w)))
		  *stage*)))
     (set! *active-widget* w))
   (if *active-widget* (#[ *active-widget* 'click ] x y))))

(keyup 
 'mouse1 
 (function (x y) 
   (if *active-widget* (#[ *active-widget* 'unclick ] x y))
   (set! *active-widget* *stage*)))

(keydn 
 'mouse2
 (function (x y)
   (and-let* ((w (widget-nested-find 
		  (lambda(w)
		    (in-area? (list x y)
			      (absolute-area w)))
		  *stage*)))
     (#[ w 'right-click ] x y))))

(mousemove 
 (function (x y xrel yrel)
   (let ((mouseover-widget 
	  (widget-nested-find 
	   (lambda (w) 
	     (in-area? (list x y) (absolute-area w))) 
	   *stage*)))
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
