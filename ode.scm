(use-modules (slayer)
	     (slayer 3d)
	     (widgets base)
	     (widgets ode-view)
	     (oop goops)
	     (extra ref)
	     (extra common)
	     (extra 3d)
	     (extra math)
	     (libs physics))

(keydn 'esc quit)

(keydn 'mouse1 select-widget-at)
(keyup 'mouse1 unselect-widget-at)
(keydn 'mouse2 right-click-widget-at)
(mousemove drag-over-widget)

(define *sim* (primitive-make-simulation))

(define-rigs-for *sim*
  (ground (with-input-from-file "rigs/ground.rig" read))
  (buggy (with-input-from-file "rigs/car.rig" read)))

(set-simulation-property! *sim* 'gravity #f32(0 0 -9.8))

(make-rig *sim* 'ground)
(make-rig *sim* 'buggy)

(define *view* 
  (make <ode-view> #:x 10 #:y 10 
	#:w (- (screen-width) 10)
	#:h (- (screen-height) 10)
	#:simulation *sim*))

(define X-SENSITIVITY -0.01)
(define Y-SENSITIVITY -0.01)

(define-method (relative-turn (object <3d>) (x <number>) (y <number>))
  (set! #[object 'orientation]
	(normalized 
	 (+ #[object 'orientation] 
	    (* (quaternion 0.0 (* x X-SENSITIVITY 
				  (rotate #f32(0 1 0) #[object 'orientation])))
	       #[object 'orientation])
	    (normalized (+ #[object 'orientation]
			   (* (quaternion 0.0 
					  (* y Y-SENSITIVITY 
					     (rotate #f32(1 0 0)
						     #[object 'orientation])))
			      #[object 'orientation])))))))

(add-child! *stage* *view*)

(set! #[*view* : 'camera : 'position] #f32(0 0 -5))

(define-syntax utimer
  (syntax-rules ()
    ((_ usecs action ...)
     (let ((tick (register-userevent (lambda () action ...))))
       (call-with-new-thread
	(lambda ()
	  (while #t
	    (generate-userevent tick)
	    (usleep usecs))))))))

(utimer 100000 (simulation-step! *sim*))

(define *modes* #[])

(utimer 30000 (for-each (lambda(f)(f)) (hash-values *modes*)))

(define (key name fun)
  (keydn name (lambda()(hash-set! *modes* name fun)))
  (keyup name (lambda()(hash-remove! *modes* name))))

(set! #[*view* 'drag] (lambda (x y dx dy)
			(relative-turn #[*view* : 'camera] dx dy)))

(key 'w (lambda () 
	  (increase! #[*view* : 'camera : 'position]
		     (rotate #f32(0 0 -0.07) 
			     #[*view* : 'camera : 'orientation]))))

(key 's (lambda () 
	  (increase! #[*view* : 'camera : 'position]
		     (rotate #f32(0 0 0.07) 
			     #[*view* : 'camera : 'orientation]))))

(key 'a (lambda () 
	  (increase! #[*view* : 'camera : 'position]
		     (rotate #f32(-0.07 0 0) 
			     #[*view* : 'camera : 'orientation]))))

(key 'd (lambda () 
	  (increase! #[*view* : 'camera : 'position]
		     (rotate #f32(0.07 0 0) 
			     #[*view* : 'camera : 'orientation]))))
