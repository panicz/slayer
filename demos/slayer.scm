#!../src/slayer -e3d
!#
(use-modules (slayer)
	     (slayer image)
	     (widgets base)
	     (widgets bitmap)
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
      (hash-remove! *modes* name))))

(cond-expand (slayer-3d

(define *sphere* (generate-capsule #:height 0))


(list->typed-array 'f32 2 '((1 2 3)
			    (4 5 6)))

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

;; załóżmy coś takiego: w trakcie rysowania używamy bufora szablonowego
;; do przechowywania indeksu kolejnego obiektu. każdy obiekt zawiera
;; kolejną liczbę całkowitą od 1 do 255. Po przejściu do przez licznik
;; liczymy znów od 1. W trakcie rysowania chcielibyśmy kojarzyć liczbę
;; z obiektami, tak żeby w razie niejednoznaczności móc uzyskać dostęp
;; tylko do obiektów, które dostały taki sam indeks, żeby w razie czego
;; móc operację powtórzyć. [ewentualnie moglibyśmy używać kolorów]

;; no dobrze, ale jak miałoby się to odbywać po stronie C/OpenGLa?
;; na przykład tak: klikamy prawym przyciskiem myszki. wówczas


(keydn 'g
  (lambda ()
    (if (not (null? #[view 'selected]))
	(let ((old-bindings (current-key-bindings))
	      (first-selected (first #[view 'selected]))
	      (original-positions (map #[_ 'position] #[view 'selected])))
	  (match-let* (((x y z) (3d->screen view #[first-selected 'position])))
	    (set-mouse-position! x y)
	    (set-key-bindings!
	     (key-bindings
	      (keydn 'esc
		(lambda () 
		  ;; restore original positions of objects
		  (for (object position) in (zip #[view 'selected]
						 original-positions)
		       (set! #[object 'position] position))
		  (set-key-bindings! old-bindings)))
	      
	      (keydn 'mouse-left
		(lambda (x y)
		  (set-key-bindings! old-bindings)))
	    
	    (mousemove 
	     (lambda (x y xrel yrel)
	       (for object in #[view 'selected]
		    (set! #[object 'position] (screen->3d view x y z))))))))))))


#|
(keydn 'g ;; miejmy świadomość gównianości tego kodu -- trzeba będzie
  (lambda () ;; go zastąpić jakimś sprytnym mechanizmem
    (let ((old-esc (keydn 'esc))
	  (old-g (keydn 'g))
	  (old-left-click #[view 'left-click])
	  (old-right-mouse-down #[view 'right-mouse-down])
	  (old-drag #[view 'drag]))
      (keydn 'esc (lambda () 
		    (set! #[view 'left-click] old-left-click)
		    (set! #[view 'right-mouse-down] old-right-mouse-down)
		    (set! #[view 'drag] old-drag)
		    (keynd 'g old-g)
		    (keydn 'esc old-esc)))
      (keydn 'g noop)
      (set! #[view 'left-click] noop)
      (set! #[view 'right-mouse-down] noop)
      (set! #[view 'drag] noop)
    )))
|#

(set! #[view 'left-click]
      (lambda (x y)
	(unselect-all! view)
	(let ((object (object-at-position x y view)))
	  (if object
	      (select-object! view object)))))

(set! #[view 'right-mouse-down]
      (lambda (x y)
	(if (object-at-position x y view)
	    (let ((xyz (screen->3d view x y)))
	      (format #t "~a ~a -> ~a -> ~a\n"x y xyz 
		      (3d->screen view xyz))
	      (add-object! view 
			   (make <3d-model> 
			     #:position (screen->3d view x y)
			     #:mesh 
			     *sphere*
			     #;(apply line-from-origin (array->list xyz))))))))

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
