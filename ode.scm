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

(add-child! *stage* *view*)

(set! #[*view* : 'camera : 'position] #f32(0 0 -5))

(let ((redraw (register-userevent noop)))
  (call-with-new-thread 
   (lambda ()
     (while #t
       (simulation-step! *sim*)
       (generate-userevent redraw)
       (usleep 100000)))))


(keydn "[" (lambda () (increase! #[*view* : 'camera : 'position] #f32(0 0 0.7))))
(keydn "]" (lambda () (increase! #[*view* : 'camera : 'position] #f32(0 0 -0.7))))
