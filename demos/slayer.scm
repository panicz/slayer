#!../src/slayer -e3d
!#
(use-modules (slayer)
	     (slayer image)
	     (widgets base)
	     (widgets sprite)
	     (widgets image-clipper)
	     (widgets text-area)
	     (oop goops)
	     (extra slayer)
	     (extra ref)
	     (extra common)
	     (extra math)
	     (extra shape)
	     (extra figures))

(keydn 'esc quit)

(set-window-title! "WELCOME TO SLAYER")

(cond-expand 
 (slayer-3d (use-modules (slayer 3d) (widgets 3d) (extra 3d)))
 (else (begin)))

(cond-expand 
 (slayer-audio (use-modules (slayer audio)))
 (else (begin)))

(cond-expand (slayer-3d

(define *sphere* (generate-capsule #:height 0))

(define 3d-object (make <3d-object> 
		    #:mesh *sphere*
		    ))

(define world (make <3d-stage>))

(define view (make <3d-editor> #:x 50 #:y 50 #:w 540 #:h 400
		   #:stage world))

(add-child! view #;to *stage*)

(add-object! 3d-object #;to world)

(add-child! #;(make <image-clipper> #:image ku #:x 580 #:y 400 #:w 40 #:h 40)
 (parameter-editor
  3d-object 
  ("x: " #[3d-object : 'position : 0])
  ("y: " #[3d-object : 'position : 1])
  ("z: " #[3d-object : 'position : 2]))
 #;to *stage*)

) (else (begin))) ;; cond-expand slayer-3d

(let ((text-area 
       (make <text-area>
	 #:x 20 #:y 20
	 #:w 256 #:h 64
	 #:max-lines 12
	 #:text-color #x000000
	 #:background-color #xffffff
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
	  ))))
  (add-child! text-area #;to *stage*))

(define ku (load-image "./art/ku.png"))

(add-child! (make-image ku 475 25) #;to *stage*)

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

;; załóżmy coś takiego: w trakcie rysowania używamy bufora szablonowego
;; do przechowywania indeksu kolejnego obiektu. każdy obiekt zawiera
;; kolejną liczbę całkowitą od 1 do 255. Po przejściu do przez licznik
;; liczymy znów od 1. W trakcie rysowania chcielibyśmy kojarzyć liczbę
;; z obiektami, tak żeby w razie niejednoznaczności móc uzyskać dostęp
;; tylko do obiektów, które dostały taki sam indeks, żeby w razie czego
;; móc operację powtórzyć. [ewentualnie moglibyśmy używać kolorów]

;; no dobrze, ale jak miałoby się to odbywać po stronie C/OpenGLa?
;; na przykład tak: klikamy prawym przyciskiem myszki. wówczas

(keydn 'g (grab-mode view))
(keydn 'h (rotate-mode view))

(set! #[view 'left-click]
      (lambda (x y)
	(unselect-all! view)
	(let ((object (object-at-position x y view)))
	  (if object
	      (select-object! object #;from view)))))

(set! #[view 'right-mouse-down]
      (lambda (x y)
	(add-object! (make <3d-model> 
		       #:position (screen->3d view x y)
		       #:mesh *sphere*)
		     #;to world)))

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
