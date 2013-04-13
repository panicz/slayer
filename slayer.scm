(use-modules (slayer input)
	     (slayer video)
	     (slayer image)
	     (slayer)
	     (widgets base)
	     (widgets bitmap)
	     (widgets text-area)
	     (widgets 3d-view)
	     (oop goops)
	     (extra ref)
	     (extra common)
	     (extra 3d)
	     (extra math))

(define *stage* (make <widget> #:w (screen-width) #:h (screen-height)))

(define *input-widget* #f)
(define *active-widget* *stage*)
(define *nearby-widget* #f)

(keydn 'esc quit)

(set-display-procedure! (\ draw *stage*))
(set-resize-procedure! (lambda (w h)
			 (set! #[*stage* 'w] w)
			 (set! #[*stage* 'h] h)))

(define ku (load-image "./art/ku.png"))

(add-child! *stage* (make-image ku 75 25))
(add-child! *stage* (make-text-area))

(keydn 'mouse1 
       (lambda (x y)
	 (and-let* ((w (widget-nested-find 
			(lambda(w)
			  (in-area? (list x y)
				    (absolute-area w)))
			*stage*)))
	   (set! *active-widget* w))
	 (if *active-widget* (#[ *active-widget* 'click ] x y))))

(keyup 'mouse1 
	(lambda (x y) 
	  (if *active-widget* (#[ *active-widget* 'unclick ] x y))
	  (set! *active-widget* *stage*)))

(keydn 'mouse2 
       (lambda (x y)
	 (and-let* ((w (widget-nested-find 
			(lambda(w)
			  (in-area? (list x y)
				    (absolute-area w)))
			*stage*)))
	   (#[ w 'right-click ] x y))))

(mousemove 
 (lambda (x y xrel yrel)
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


(define 3d-object #f)
(define 3d-camera #f)

(if (defined? '<3d-view>)
    (begin 
      (set! 3d-object (make <3d-mesh>))
      (let ((view (make <3d-view> #:x 50 #:y 50 #:w 540 #:h 400)))
	(add-child! *stage* view)
	(set! #[view 'click]
	      (lambda (x y)
		(set! #[view : 'data : 'anchor] (list x y))
		(set! #[view : 'data : 'original-orientation] 
		      #[3d-object 'orientation])))
	(set! #[view 'unclick]
	      (lambda (x y)
		(hash-remove! #[view 'data] 'anchor)
		(hash-remove! #[view 'data] 'original-orientation)))
	(set! #[view 'drag] 
	      (lambda (x y dx dy)
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

(define (rotator deg axis)
  (let* ((rad (deg->rad deg))
	 (p (quaternion (cos rad) (* (sin rad) (normalized axis)))))
    (lambda(q)(* p q (~ p)))))

(if 3d-camera
    (begin
      (keydn 'right (lambda () 
		     (increase! #[3d-camera 'position] #(0.07 0 0))))
      (keydn 'left (lambda ()
		   (increase! #[3d-camera 'position] #(-0.07 0 0))))
      (keydn 'down (lambda ()
		     (increase! #[3d-camera 'position] #(0 0.07 0))))
      (keydn 'up (lambda ()
		      (increase! #[3d-camera 'position] #(0 -0.07 0))))
      (keydn "[" (lambda () 
		   (increase! #[3d-camera 'position] #f32(0 0 0.7))))
      (keydn "]" (lambda ()
		   (increase! #[3d-camera 'position] #f32(0 0 -0.7))))

      (keydn '= (lambda ()
		     (transform! 
		      (rotator (/ pi 12) #f32(0 0 1)) 
		      #[3d-camera 'orientation])))
      (keydn '- (lambda ()
		      (transform! 
		       (rotator (/ pi -12) #f32(0 0 1)) 
		       #[3d-camera 'orientation])))

      (keydn 'd (lambda () 
		     (increase! #[3d-object 'position] #(0.07 0 0))))
      (keydn 'a (lambda ()
		   (increase! #[3d-object 'position] #(-0.07 0 0))))
      (keydn 's (lambda ()
		     (increase! #[3d-object 'position] #(0 0.07 0))))
      (keydn 'w (lambda ()
		      (increase! #[3d-object 'position] #(0 -0.07 0))))
      (keydn 'r (lambda () 
		   (increase! #[3d-object 'position] #f32(0 0 0.7))))
      (keydn 'f (lambda ()
		   (increase! #[3d-object 'position] #f32(0 0 -0.7))))

      (keydn 'e (lambda ()
		     (transform! 
		      (rotator (/ pi 12) #f32(0 0 1)) 
		      #[3d-object 'orientation])))
      (keydn 'q (lambda ()
		      (transform! 
		       (rotator (/ pi -12) #f32(0 0 1)) 
		       #[3d-object 'orientation])))

      ))
