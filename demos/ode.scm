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

(define *sim* (primitive-make-simulation))

(define-rigs-for *sim*
  (ground (with-input-from-file "art/rigs/ground.rig" read))
  (buggy (with-input-from-file "art/rigs/car.rig" read)))

(set-simulation-property! *sim* 'gravity #f32(0 0 -0.5))
(make-rig *sim* 'ground)
(make-rig *sim* 'buggy)

(define *view* 
  (make <ode-view> #:x 10 #:y 10 
	#:w (- (screen-width) 10)
	#:h (- (screen-height) 10)
	#:simulation *sim*))

(add-child! *stage* *view*)

;(set! #[*view* : 'camera : 'position] #f32(0 0 -5))

(define-syntax utimer
  (syntax-rules ()
    ((_ usecs action ...)
     (let ((tick (register-userevent (lambda () action ...))))
       (call-with-new-thread
	(lambda ()
	  (while #t
	    (generate-userevent tick)
	    (usleep usecs))))))))

(utimer 25000 (simulation-step! *sim*))

(define *modes* #[])

(utimer 30000 (for-each (lambda(f)(f)) (hash-values *modes*)))

(define (key name fun)
  (keydn name (lambda()(hash-set! *modes* name fun)))
  (keyup name (lambda()(hash-remove! *modes* name))))

(key 'q (lambda () (relative-twist! #[*view* 'camera] #f32(0 0 0.02))))
(key 'e (lambda () (relative-twist! #[*view* 'camera] #f32(0 0 -0.02))))
(key 'w (lambda () (relative-move! #[*view* 'camera] #f32(0 0 -0.07))))
(key 's (lambda () (relative-move! #[*view* 'camera] #f32(0 0 0.07))))
(key 'a (lambda () (relative-move! #[*view* 'camera] #f32(-0.07 0 0))))
(key 'd (lambda () (relative-move! #[*view* 'camera] #f32(0.07 0 0))))
(key 'r (lambda () (relative-move! #[*view* 'camera] #f32(0 0.07 0))))
(key 'f (lambda () (relative-move! #[*view* 'camera] #f32(0 -0.07 0))))

(key 'up (lambda () (relative-turn! #[*view* 'camera] 0 2)))
(key 'down (lambda () (relative-turn! #[*view* 'camera] 0 -2)))

(key 'left (lambda () (relative-turn! #[*view* 'camera] 2 0)))
(key 'right (lambda () (relative-turn! #[*view* 'camera] -2 0)))

(set! #[*view* 'drag] (lambda (x y dx dy)
			(relative-turn! #[*view* 'camera] (- dx) (- dy))))

(keydn '/ (lambda () (<< `(position: ,#[*view* : 'camera : 'position])
			 `(rotation: ,#[*view* : 'camera : 'orientation]))))

(set! #[*view* : 'camera : 'position] 
      #f32(0.06452277302742 1.43872618675232 0.183476984500885))


(set! #[*view* : 'camera : 'orientation] 
      (quaternion 0.0 #f32(0.0 0.707106781186548 0.707106781186548)))

				   


#;(set! #[*view* : 'camera : 'orientation] 
      (quaternion -0.00158714875474163 
		  #f32(-0.00434838561341166 
		       -0.698689818382263 
		       -0.715409755706787)))
