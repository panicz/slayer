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

(define 3d-object (make <3d-mesh>))
(define view (make <3d-view> #:x 50 #:y 50 #:w 540 #:h 400))
(add-child! *stage* view)
(define 3d-camera #[view 'camera])
(add-object! view 3d-object)

(keydn 'w (lambda () 
	     (increase! #[view : 'camera : 'position]
			(rotate #f32(0 0 -0.7) 
				#[view : 'camera : 'orientation]))))

(keydn 's (lambda () 
	    (increase! #[view : 'camera : 'position]
		       (rotate #f32(0 0 0.7) 
			       #[view : 'camera : 'orientation]))))

(keydn 'a (lambda () 
	     (increase! #[view : 'camera : 'position]
			(rotate #f32(-0.7 0 0) 
				#[view : 'camera : 'orientation]))))

(keydn 'd (lambda () 
	    (increase! #[view : 'camera : 'position]
		       (rotate #f32(0.7 0 0) 
			       #[view : 'camera : 'orientation]))))
