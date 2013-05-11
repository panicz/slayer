#!../src/slayer -e3d -islayer.scm
!#
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

(define-syntax utimer
  (syntax-rules ()
    ((_ usecs action ...)
     (let ((tick (register-userevent (lambda () action ...))))
       (call-with-new-thread
	(lambda ()
	  (while #t
	    (generate-userevent tick)
	    (usleep usecs))))))))

(set-caption! "WELCOME TO SLAYER")

(define 3d-object (make <3d-mesh>))
(define view (make <3d-view> #:x 50 #:y 50 #:w 540 #:h 400))
(add-child! *stage* view)
(add-object! view 3d-object)

(define ku (load-image "./art/ku.png"))

(add-child! 
 *stage* 
 (make-text-area 
  #:text 
  (string-append
   "'(click somewhere around HERE and start typing scheme code)\n"
   "'(use F1 to evaluate last sexp)\n"
   "'((by default the result is printed to stdout))\n\n"
   "'(use '(W S A D) and mouse to navigate the 3d area)\n"
   "'(use 'ESC to finish typing (after having clicked above)\n"
   "  or to exit application)\n")))


(add-child! *stage* (make-image ku 475 25))

(define *modes* #[])

(utimer 30000 (for-each (lambda(f)(f)) (hash-values *modes*)))

(define (key name fun)
  (keydn name (lambda()(hash-set! *modes* name fun)))
  (keyup name (lambda()(hash-remove! *modes* name))))


(define newtra (load-music "art/newtra.mp3"))
(define alert (load-sound "art/alert.wav"))

(keydn 'm (lambda()(play-music! newtra)))
(keydn 'n (lambda()(play-sound! alert)))


(define X-SENSITIVITY -0.01)
(define Y-SENSITIVITY -0.01)

#;(define-method (turn (object <3d>) (x <number>) (y <number>))
  (set! #[object 'orientation]
	(normalized 
	 (+ #[object 'orientation] 
	    (* (quaternion 0.0 (* x X-SENSITIVITY #f32(0 1 0)))
	       #[object 'orientation])
	    (normalized (+ #[object 'orientation]
			   (* (quaternion 0.0 (* y Y-SENSITIVITY #f32(1 0 0)))
			      #[object 'orientation])))))))

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

(set! #[view 'drag] (lambda (x y dx dy)
		      (relative-turn #[view : 'camera] dx dy)))

(key 'w (lambda () 
	  (increase! #[view : 'camera : 'position]
		     (rotate #f32(0 0 -0.07) 
			     #[view : 'camera : 'orientation]))))

(key 's (lambda () 
	  (increase! #[view : 'camera : 'position]
		     (rotate #f32(0 0 0.07) 
			     #[view : 'camera : 'orientation]))))

(key 'a (lambda () 
	     (increase! #[view : 'camera : 'position]
			(rotate #f32(-0.07 0 0) 
				#[view : 'camera : 'orientation]))))

(key 'd (lambda () 
	    (increase! #[view : 'camera : 'position]
		       (rotate #f32(0.07 0 0) 
			       #[view : 'camera : 'orientation]))))
