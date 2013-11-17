#!../src/slayer -e3d
!#
(use-modules 
 (slayer image)
 (slayer)
 (widgets base)
 (widgets bitmap)
 (widgets text-area)
 (oop goops)
 (extra ref)
 (extra common)
 (extra math)
 (extra figures))

(keydn 'esc quit)
(set-window-title! "WELCOME TO SLAYER")

(cond-expand 
 (slayer-3d (use-modules (slayer 3d) (widgets 3d-view) (extra 3d)))
 (else (begin)))

(cond-expand 
 (slayer-audio (use-modules (slayer audio)))
 (else (begin)))


(define *modes* #[])
(define *mutex* (make-locked-mutex))

(add-timer! 
 30 #;ms
 (lambda()
   (for (key => proc) in *modes*
	(proc))))

(define (key name fun)
  (keydn name 
	 (lambda()
	   (hash-set! *modes* name fun)))
  (keyup name 
	 (lambda()
	   (hash-remove! *modes* name)
	   )))

(cond-expand (slayer-3d

(define *sphere* (generate-capsule #:height 0))

(define 3d-object (make <3d-model> 
		    #:mesh *sphere*))
(define view (make <3d-view> #:x 50 #:y 50 #:w 540 #:h 400))
(add-child! *stage* view)
(add-object! view 3d-object)

) (else (begin))) ;; cond-expand slayer-3d

(add-child! 
 *stage* 
 (make-text-area 
  #:text 
  (string-append
   "'(click somewhere around HERE and start typing scheme code)\n"
   "'(use F1 to evaluate last sexp)\n"
   "'((by default the result is printed to stdout))\n\n"

   (cond-expand 
    (slayer-3d
     (string-append
      "'(use '(W S A D) and mouse to navigate the 3d area)\n"
      "'(right-click on the ball to create a new one!)\n"))
    (slayer-3d-available
     (string-append
      "'(to see the 3D version of this demo, supply the -e3d option\n"
      " as a command line argument, or reconfigure with "
      "--enable-default-3d)\n\n"))
    (else
     (string-append
      "'(demo compiled without opengl support)\n")))

   "'(use 'ESC to finish typing (after having clicked above)\n"
   "  or to exit application)\n"
   (cond-expand
    (slayer-audio
     "'(press m to hear some music)")
    (else ""))
   )))

(define ku (load-image "./art/ku.png"))
(add-child! *stage* (make-image ku 475 25))

(cond-expand (slayer-audio

(define newtra (load-music "art/sounds/newtra.mp3"))
(define alert (load-sound "art/sounds/alert.wav"))

(keydn 'm (lambda()(play-music! newtra)))
(keydn 'n (lambda()(play-sound! alert)))

) (else (begin))) ;; cond-expand slayer-audio

(cond-expand (slayer-3d

(set! #[view 'drag]
      (lambda(x y dx dy)
	(relative-turn! #[view 'camera] (- dx) (- dy))))

(set! #[view 'right-mouse-down]
      (lambda(x y)
	(add-object! view 
		     (make <3d-model> 
		       #:position (mouse->3d view x y)
		       #:mesh *sphere*
		       ))))

(key 'q (lambda () (relative-twist! #[view 'camera] #f32(0 0 0.02))))
(key 'e (lambda () (relative-twist! #[view 'camera] #f32(0 0 -0.02))))
(key 'w (lambda () (relative-move! #[view 'camera] #f32(0 0 -0.07))))
(key 's (lambda () (relative-move! #[view 'camera] #f32(0 0 0.07))))
(key 'a (lambda () (relative-move! #[view 'camera] #f32(-0.07 0 0))))
(key 'd (lambda () (relative-move! #[view 'camera] #f32(0.07 0 0))))
(key 'r (lambda () (relative-move! #[view 'camera] #f32(0 0.07 0))))
(key 'f (lambda () (relative-move! #[view 'camera] #f32(0 -0.07 0))))

(key 'up (lambda () (relative-turn! #[view 'camera] 0 2)))
(key 'down (lambda () (relative-turn! #[view 'camera] 0 -2)))
(key 'left (lambda () (relative-turn! #[view 'camera] 2 0)))
(key 'right (lambda () (relative-turn! #[view 'camera] -2 0)))

) (else (begin))) ;;cond-expand slayer-3d
