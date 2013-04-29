(use-modules (slayer image)
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

(keydn 'esc quit)

(define ku (load-image "./art/ku.png"))

(add-child! *stage* (make-image ku 75 25))
(add-child! *stage* (make-text-area))

(keydn 'mouse1 select-widget-at)
(keyup 'mouse1 unselect-widget-at)
(keydn 'mouse2 right-click-widget-at)
(mousemove drag-over-widget)

(define 3d-object #f)
(define 3d-camera #f)

(if (defined? '<3d-view>)
    (begin 
      (set! 3d-object (make <3d-mesh>))
      (let ((view (make <3d-view> #:x 50 #:y 50 #:w 540 #:h 400)))
	(add-child! *stage* view)
	(set! 3d-camera #[view 'camera])
	(add-object! view 3d-object))))

(define (rotator deg axis)
  (let* ((rad (deg->rad deg))
	 (p (quaternion (cos rad) (* (sin rad) (normalized axis)))))
    (lambda(q)(* p q (~ p)))))

(if 3d-camera
    (begin
      (keydn 'right (lambda () (increase! #[3d-camera 'position] #(0.07 0 0))))
      (keydn 'left (lambda () (increase! #[3d-camera 'position] #(-0.07 0 0))))
      (keydn 'down (lambda () (increase! #[3d-camera 'position] #(0 0.07 0))))
      (keydn 'up (lambda () (increase! #[3d-camera 'position] #(0 -0.07 0))))
      (keydn "[" (lambda () (increase! #[3d-camera 'position] #f32(0 0 0.7))))
      (keydn "]" (lambda () (increase! #[3d-camera 'position] #f32(0 0 -0.7))))
      (keydn '= (lambda () 
		  (transform! 
		   (rotator (/ pi 12) #f32(0 0 1)) 
		   #[3d-camera 'orientation])))
      (keydn '- (lambda ()
		  (transform! 
		   (rotator (/ pi -12) #f32(0 0 1)) 
		   #[3d-camera 'orientation])))
      (keydn 'd (lambda () (increase! #[3d-object 'position] #(0.07 0 0))))
      (keydn 'a (lambda () (increase! #[3d-object 'position] #(-0.07 0 0))))
      (keydn 's (lambda () (increase! #[3d-object 'position] #(0 0.07 0))))
      (keydn 'w (lambda () (increase! #[3d-object 'position] #(0 -0.07 0))))
      (keydn 'r (lambda () (increase! #[3d-object 'position] #f32(0 0 0.7))))
      (keydn 'f (lambda () (increase! #[3d-object 'position] #f32(0 0 -0.7))))
      (keydn 'e (lambda ()
		     (transform! 
		      (rotator (/ pi 12) #f32(0 0 1)) 
		      #[3d-object 'orientation])))
      (keydn 'q (lambda ()
		      (transform! 
		       (rotator (/ pi -12) #f32(0 0 1)) 
		       #[3d-object 'orientation])))
      ))
