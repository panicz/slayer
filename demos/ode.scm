#!../src/slayer -e3d
exit # this prevents from executing the rest of the file by the shell
!#
(use-modules (slayer)
	     (slayer 3d)
	     (widgets base)
	     (widgets physics)
	     (oop goops)
	     (extra ref)
	     (extra common)
	     (extra 3d)
	     (extra math)
	     (extra slayer)
	     (scum physics))

(keydn 'esc quit)

(define *sim* (primitive-make-simulation))

(define-rigs-for *sim*
  (ground (with-input-from-file "art/rigs/ground.rig" read))
  (buggy (with-input-from-file "art/rigs/car.rig" read)))

(set-simulation-property! *sim* 'gravity #f32(0 0 -0.98))
;(set-simulation-property! *sim* 'erp 0.8)
;(set-simulation-property! *sim* 'cfm 1.0)

(make-rig *sim* 'ground)
(and-let* ((buggy (make-rig *sim* 'buggy))
	   (chassis (body-named 'chassis buggy))
	   (front (joint-named 'front buggy))
	   (v 0)(w 0))
  (keydn 'lctrl (lambda()
		  (let ((max-force-2 (joint-property front 'max-force-2)))
		    (if (> max-force-2 0)
			(set-joint-property! front 'max-force-2 0.0)
			(set-joint-property! front 'max-force-2 100.0)))))       
  (key 'lshift (lambda()(force! chassis #f32(6 0 0))))
  (key 'return (lambda()(force! chassis #f32(0 0 6))))
  (key 'k (lambda () 
	     (increase! v 0.02)
	     (set-joint-property! front 'velocity-2 v)))
  (key 'i (lambda ()
	     (decrease! v 0.02)
	     (set-joint-property! front 'velocity-2 v)))
  (key 'l (lambda ()
	     (increase! w 0.001)
	     (set-joint-property! front 'velocity w)))
  (key 'j (lambda ()
	     (decrease! w 0.001)
	     (set-joint-property! front 'velocity w)))
  (key 'space
       (lambda ()
	 (set! v 0)
	 (set! w 0)
	 (set-joint-property! front 'velocity w)
	 (set-joint-property! front 'velocity-2 v))))

#;(let* ((buggy (make-rig *sim* 'buggy))
       (controls (rig-controls rig))
       (one (char->integer #\1)))
  (for i in 0 .. (last-index controls)
       (key (list->string (integer->char (+ one i)))
	    (lambda () (rig-add-control! 
			buggy 
			(if (modifier-pressed? 'shift)
			    0.1
			    -0.1))))))

(define *sim-stage* (make <physics-stage> #:simulation *sim*))

(define *view* 
  (make <3d-view> #:x 10 #:y 10 
	#:w (- (screen-width) 10)
	#:h (- (screen-height) 10)
	#:stage *sim-stage*))

(add-child! *stage* *view*)

;(set! #[*view* : 'camera : 'position] #f32(0 0 -5))

(add-timer! 25 (lambda()(make-simulation-step! *sim*)))

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
      #f32(0 1.5 0.2))

(set! #[*view* : 'camera : 'orientation] 
      (quaternion 0.0 #f32(0.0 0.707106781186548 0.707106781186548)))

