#!../src/slayer
!#
(use-modules (slayer)
	     (slayer image)	     
	     (slayer font)
	     (extra common))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define world-signature '((x y dx dy sec carrying voltage alive? progress) sectors))

(define-macro (recreate-world)
  (define (unquoted data)
    (if (list? data)
	(map unquoted data)
	(list 'unquote data)))
  (list 'quasiquote (unquoted world-signature)))

(define-macro (with-world-signature body)
  `(match-lambda
       (,world-signature ,body)))
(define-macro (transform . <l>)
  `(with-world-signature
    (let* ,<l>
      (recreate-world))))

(define (to-int n) (inexact->exact (floor n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the i/o crap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand 
 (slayer-audio (use-modules (slayer audio)) (display "using audio\n"))
 (else (define load-sound noop)
       (define play-sound! noop)
       (display "dismissing audio\n")))

(define *samples* (list->array 1 `(,(load-sound "art/pandora/click.wav")
				   ,(load-sound "art/pandora/beep.wav"))))

(set-window-title! "P A N D O R A")
(set-screen-size! 640 480)

(define *sprites* (list->array 1 `(,(load-image "art/pandora/komputr.png") ; 0
				   ,(load-image "art/pandora/robot-ur.png") ; 1
				   ,(load-image "art/pandora/robot-dr.png") ; 2
				   ,(load-image "art/pandora/robot-ul.png") ; 3
				   ,(load-image "art/pandora/robot-dl.png") ; 4
				   ,(load-image "art/pandora/kostka-1.png") ; 5
				   ,(load-image "art/pandora/kostka-2.png") ; 6
				   ,(load-image "art/pandora/kostka-3.png") ; 7
				   ,(load-image "art/pandora/kostka-4.png") ; 8
				   ,(load-image "art/pandora/kostka-5.png") ; 9
				   )))

(define *font* (load-font "art/VeraMono.ttf" 11))

(define *status-line-text* "")
(define *voltage-status* 0)
(define *display* '())

(define (mk-display-message msg x y)
  (lambda ()    
   (draw-image! (render-text msg *font*) x y)))

(define (mk-display-messages msgs)
  (lambda ()
    (draw-image! (array-ref *sprites* 0) 0 0)
    (for-each (match-lambda ((msg x y)
			     (draw-image! (render-text msg *font*) x y)))
	      msgs)))



(define (mk-voltage-bar-image)
  (rectangle (min 600 (to-int (/ (+ *voltage-status* (random 9)) 2.0))) 6 #xddFF00))

(define display-world
 (lambda()
   (draw-image! (render-text *status-line-text* *font*) 66 420)
   (draw-image! (mk-voltage-bar-image) 66 366)
   (for-each
    (match-lambda ((x y sprite-index)
		   (draw-image! (array-ref *sprites* sprite-index) x y)))
    *display*)))

(define *joystick* 0)

(keydn 'space (lambda () (set! *joystick* 'A)))
(keydn 'up    (lambda () (set! *joystick* 'N)))
(keydn 'right (lambda () (set! *joystick* 'E)))
(keydn 'left  (lambda () (set! *joystick* 'W)))
(keydn 'down  (lambda () (set! *joystick* 'S)))

(keydn 'esc quit)
(keydn 'q quit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the isometric gfx crap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (set-to-display! view)
  (set! *display*
	(append
	 (map (match-lambda ((map-x map-y . details)
			     (let* ((tile-half-width 32)
				    (tile-half-height 16)
				    (center-x 320)
				    (top-y 0)
				    (disp-x
				    (+ (- center-x tile-half-width)
				       (* tile-half-width (- map-x map-y))))
				    (disp-y
				     (+ top-y
					(* tile-half-height (+ map-x map-y))))
				    (sprite-index
				     ((match-lambda (('wall 'TYPE-1) 5)
						    (('wall 'TYPE-2) 6)
						    (('wall 'TYPE-3) 7)
						    (('hero 'W) 1)
						    (('hero 'S) 2)
						    (('hero 'N) 3)
						    (('hero 'E) 4)
						    (('object 'BATTERY) 9)
						    (('object 'RADIATOR) 8)
						    (('object 'DOOR) 8)
						    (('object 'KEY) 9)
						    (('object 'LASER-GORDIAN-KNOT) 8)
						    (('object 'LASER-SABRE) 9)
						    (('object 'LASER-TRAP) 8)
						    (('object 'MIRROR) 9)
						    (('object 'REACTOR) 8)
						    (('object 'GRAPHITE) 9)
						    (('object 'VOLTAGE-CONTROLLED-LOCK) 8)
					            ; ...
						    (otherwise 9) ;;; muka?!
						    ) details)))
			       `(,(to-int disp-x) ,(to-int disp-y) ,sprite-index))))
	      (sort-current-view view))
	 '((0 0 0))))) ;; +komputr!


(define current-view
  (with-world-signature
   (let* ((cur-sector
	   (list-ref sectors sec))
	  (obstacles ; don't display portals ziom
	   (filter (match-lambda ((x y 'portal . _) #f)
				 (otherwise #t))
		   cur-sector))
	  (hero-facing
	   (cond ((= dx -1) 'W)
		 ((= dx 1) 'E)
		 ((= dy -1) 'N)
		 ((= dy 1) 'S))))
     (cons `(,x ,y hero ,hero-facing)
	   obstacles))))

(define (sort-current-view visibles-list)
  (let ((dist-from-origin ;; sq.rt. is monotone anyway, and the observer stands at (13,13).
	 (match-lambda ((x y . _)
			(+ (* (- 13.0 x) (- 13.0 x))
			   (* (- 13.0 y) (- 13.0 y)))))))
     (sort visibles-list
	   (lambda (a b)
	     (> (dist-from-origin a)
		(dist-from-origin b))))))

	  
(define (mk-path x1 y1 x2 y2 steps)
  (map (lambda (step)
	 (let* ((rat (/ (+ step 1) (+ 1.0 steps)))
		(x (+ x1 (* rat (- x2 x1))))
		(y (+ y1 (* rat (- y2 y1)))))
	   `(,x ,y)))
       (iota steps)))

;;; animacja: ruch bohatera lub zniknięcie obiektu; world x world -> [visibles-list]
(define (mk-animation-sequence before after)
  (match-let* ((anim-steps 4)
	       (walk-sound (array-ref *samples* 0))
	       (action-sound (array-ref *samples* 1))
	       ((hero-before . obstacles-before) (current-view before))
	       ((hero-after . obstacles-after) (current-view after))
	       ((xb yb 'hero fb) hero-before)
	       ((xa ya 'hero fa) hero-after))
    (cond ;;; zmiana sektora:
	  ((not (= ((with-world-signature sec) before)
		   ((with-world-signature sec) after)))
	   `((() ,walk-sound)))
	  ;;; zwykły ruch bohatera:
	  ((not (and (= xb xa)
		     (= yb ya)))
	   (map (match-lambda ((x y)
			       `(((,x ,y hero ,fa) . ,obstacles-after) ,walk-sound)))
		(mk-path xb yb xa ya anim-steps)))
	  ;;; a znikanie obiektu?
	  ((not (equals? obstacles-before obstacles-after))
	   `((,(current-view after) ,action-sound)))
	  (else '()))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the divine game mechanics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yup, a monoid!
(define (identity world) world)
(define (compose f g) (lambda (world) (g (f world))))
;; what every cycle does
(define std-step
  (transform (hot-room?
	      (not
	       (null? (filter
		       (match-lambda ((x y 'object 'RADIATOR) #t)
				     ((x y 'object 'REACTOR) #t)
				     (otherwise #f))
		       (list-ref sectors sec)))))
	     (voltage-dec (+ (if hot-room? 6 1)
			     (if carrying 3 0)))
	     (voltage (- voltage voltage-dec))
	     (alive? (and alive?
			  (> voltage 0)))))


(define (list-subst index what where) ;; się nie będę prosił. nondefensywny!
  (if (= index 0)
      (cons what (cdr where))
      (cons (car where)
	    (list-subst (- index 1) what (cdr where)))))

(define (mk-remover x1 y1 sec)
  (transform (sector
	      (list-ref sectors sec))
	     (sector1
	      (filter (match-lambda ((x0 y0 . details)
				     (or (not (= x0 x1))
					 (not (= y0 y1)))))
		      sector))
	     (sectors
	      (list-subst sec sector1 sectors))))

(define (mk-face dx1 dy1)
  (transform (dx dx1)
	     (dy dy1)))

(define (mk-walk dx1 dy1)
  (compose (mk-face dx1 dy1)
	   (with-world-signature
	    (match (lookup-in-front-of-hero (recreate-world))
	      ('#f
	       (let* ((x (+ x dx))
		      (y (+ y dy)))
		 (recreate-world)))
	      (('portal x1 y1 sec1)
	       (let* ((x x1)
		      (y y1)		      		     
		      (sec sec1))
		 (recreate-world)))
	      (otherwise
	       (recreate-world))))))

(define (mk-pickup object)
  (transform (carrying object)))

(define (mk-drop) (mk-pickup #f))

(define (mk-eat val)
  (transform (voltage (+ voltage val))))

(define (mk-kill)
  (transform (alive? #f)))

(define (mk-progress)
  (transform (progress (cons '* progress))))

(define provide-action ;; object-to-act-on x carrying
  (match-lambda	(('#f 'BATTERY x y sec)
		 (compose (mk-drop) (mk-eat 242)))
		(('WALL 'BATTERY x y sec)
		 (compose (mk-drop) (mk-eat 242)))
		(('BATTERY '#f x y sec)
		 (compose (mk-remover x y sec) (mk-pickup 'BATTERY)))
		(('BATTERY sth x y sec)
		 (compose (mk-remover x y sec) (mk-eat 242)))

		(('KEY sth x y sec)
		 (compose (mk-remover x y sec) (mk-pickup 'KEY)))

		(('GRAPHITE sth x y sec)
		 (compose (mk-remover x y sec) (mk-pickup 'GRAPHITE)))

		(('MIRROR sth x y sec)
		 (compose (mk-remover x y sec) (mk-pickup 'MIRROR)))

		(('LASER-SABRE sth x y sec)
		 (compose (mk-remover x y sec) (mk-pickup 'LASER-SABRE)))

		(('DOOR 'KEY x y sec)
		 (compose (mk-remover x y sec)
			  (mk-drop)))
		(('DOOR sth x y sec)
		 identity)

		(('VOLTAGE-CONTROLLED-LOCK 'BATTERY x y sec)
		 (mk-remover x y sec))
		(('VOLTAGE-CONTROLLED-LOCK sth x y sec)
		 (mk-kill))

		(('LASER-TRAP 'MIRROR x y sec)
		 (compose (mk-drop) (mk-remover x y sec)))
		(('LASER-TRAP sth x y sec)
		 (mk-kill))

		(('LASER-GORDIAN-KNOT 'LASER-SABRE x y sec)
		 (compose (mk-drop) (mk-remover x y sec)))
		(('LASER-GORDIAN-KNOT sth x y sec)
		 (mk-kill))

		(('REACTOR 'GRAPHITE x y sec)
		 (compose (mk-remover x y sec)
			  (compose (mk-drop)
				   (mk-progress))))
		(('REACTOR sth x y sec)
		 (mk-kill))

		(('RADIATOR sth x y sec)
		 (mk-kill))
		; ...?
		(otherwise identity)))

(define try-action
  (with-world-signature
   (let* ((what-sits-there
	   (lookup-in-front-of-hero (recreate-world)))
	  (object-name (if (and what-sits-there 
				(eq? (car what-sits-there) 'object))
			   (cadr what-sits-there)
			   #f))
	  (action
	   (provide-action `(,object-name ,carrying ,(+ x dx) ,(+ y dy) ,sec))))
     (action (recreate-world)))))
		   
(define (mk-game-step action)
  (compose
   (match action
     ('0 identity)
     ('N (mk-walk 0 -1))
     ('E (mk-walk 1 0)) 
     ('W (mk-walk -1 0))
     ('S (mk-walk 0 1)) 
     ('A try-action))
   std-step))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lookup-world x1 y1 sec sectors)
  (let* ((sector
	  (list-ref sectors sec))
	 (records
	  (filter-map (match-lambda ((x y . details)
				     (if (and (= x x1)
					      (= y y1))
					 details
					 #f)))
		      sector)))
    (if (null? records)
	#f
	(car records))))

(define lookup-in-front-of-hero
  (with-world-signature
   (let* ((x0 (+ x dx))
	  (y0 (+ y dy)))
     (lookup-world x0 y0 sec sectors))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the generated-elsewhere crap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 
(define (initiate-game-world)
'((4 3 0 1 0 #f 666 #t ())
 (((2 9 portal 2 1 2)
   (3 8 wall TYPE-1)
   (1 8 wall TYPE-2)
   (8 7 wall TYPE-2)
   (7 7 wall TYPE-1)
   (6 7 wall TYPE-1)
   (5 7 wall TYPE-1)
   (4 7 wall TYPE-1)
   (3 7 wall TYPE-1)
   (1 7 wall TYPE-2)
   (9 6 portal 1 4 1)
   (6 6 object DOOR)
;   (3 6 object BATTERY)
   (1 6 wall TYPE-2)
   (8 5 wall TYPE-2)
   (7 5 wall TYPE-2)
   (6 5 wall TYPE-2)
   (1 5 wall TYPE-2)
   (6 4 wall TYPE-2)
   (5 4 object BATTERY)
   (1 4 wall TYPE-2)
   (6 3 wall TYPE-2)
   (2 3 wall TYPE-2)
   (6 2 wall TYPE-2)
   (3 2 wall TYPE-2)
   (6 1 wall TYPE-2)
   (5 1 wall TYPE-2)
   (4 1 wall TYPE-2))
  ((6 8 wall TYPE-3)
   (5 8 wall TYPE-3)
   (4 8 wall TYPE-3)
   (7 7 wall TYPE-3)
   (6 7 wall TYPE-3)
   (5 7 wall TYPE-3)
   (4 7 wall TYPE-3)
   (3 7 wall TYPE-3)
   (8 6 wall TYPE-3)
   (7 6 wall TYPE-3)
   (3 6 wall TYPE-3)
   (2 6 wall TYPE-3)
   (1 6 wall TYPE-3)
   (8 5 wall TYPE-3)
   (7 5 wall TYPE-3)
   (5 5 object REACTOR)
   (0 5 portal 8 6 0)
   (8 4 wall TYPE-3)
   (7 4 wall TYPE-3)
   (5 4 object REACTOR)
   (0 4 portal 8 6 0)
   (8 3 wall TYPE-3)
   (7 3 wall TYPE-3)
   (3 3 wall TYPE-3)
   (2 3 wall TYPE-3)
   (1 3 wall TYPE-3)
   (7 2 wall TYPE-3)
   (6 2 wall TYPE-3)
   (5 2 wall TYPE-3)
   (4 2 wall TYPE-3)
   (3 2 wall TYPE-3)
   (6 1 wall TYPE-3)
   (5 1 wall TYPE-3)
   (4 1 wall TYPE-3))
  ((2 9 portal 7 1 4)
   (8 8 wall TYPE-1)
   (7 8 wall TYPE-1)
   (6 8 wall TYPE-1)
   (3 8 wall TYPE-1)
   (1 8 wall TYPE-2)
   (8 7 wall TYPE-1)
   (7 7 object KEY)
   (6 7 wall TYPE-1)
   (5 7 wall TYPE-2)
   (4 7 wall TYPE-2)
   (3 7 wall TYPE-1)
   (1 7 wall TYPE-2)
   (8 6 wall TYPE-1)
   (1 6 wall TYPE-2)
   (8 5 wall TYPE-1)
   (1 5 wall TYPE-2)
   (8 4 wall TYPE-1)
   (5 4 object BATTERY)
   (1 4 wall TYPE-2)
   (8 3 wall TYPE-1)
   (7 3 object LASER-TRAP)
   (6 3 wall TYPE-2)
   (5 3 wall TYPE-1)
   (4 3 wall TYPE-1)
   (3 3 wall TYPE-2)
   (1 3 wall TYPE-2)
   (8 2 wall TYPE-1)
   (6 2 wall TYPE-2)
   (3 2 wall TYPE-2)
   (1 2 wall TYPE-2)
   (8 1 wall TYPE-2)
   (6 1 wall TYPE-2)
   (3 1 wall TYPE-2)
   (1 1 wall TYPE-2)
   (7 0 portal 7 8 3)
   (2 0 portal 2 8 0))
  ((7 9 portal 7 1 2)
   (8 8 wall TYPE-1)
   (6 8 wall TYPE-1)
   (8 7 wall TYPE-1)
   (7 7 object BATTERY)
   (6 7 wall TYPE-1)
   (5 7 wall TYPE-1)
   (4 7 wall TYPE-1)
   (8 6 wall TYPE-1)
   (4 6 wall TYPE-2)
   (8 5 wall TYPE-1)
   (6 5 object LASER-SABRE)
   (4 5 wall TYPE-2)
   (8 4 wall TYPE-1)
   (4 4 wall TYPE-2)
   (8 3 wall TYPE-2)
   (7 3 wall TYPE-2)
   (6 3 wall TYPE-2)
   (5 3 wall TYPE-2)
   (4 3 wall TYPE-2))
  ((8 8 wall TYPE-2)
   (7 8 wall TYPE-1)
   (6 8 wall TYPE-1)
   (5 8 wall TYPE-1)
   (4 8 wall TYPE-1)
   (3 8 wall TYPE-1)
   (9 7 portal 1 4 5)
   (8 7 object LASER-GORDIAN-KNOT)
   (6 7 wall TYPE-1)
   (4 7 object RADIATOR)
   (2 7 wall TYPE-1)
   (8 6 wall TYPE-2)
   (4 6 object MIRROR)
   (1 6 wall TYPE-1)
   (8 5 wall TYPE-1)
   (6 5 wall TYPE-1)
   (4 5 object RADIATOR)
   (1 5 wall TYPE-1)
   (8 4 wall TYPE-1)
   (4 4 object RADIATOR)
   (2 4 wall TYPE-1)
   (8 3 wall TYPE-1)
   (6 3 wall TYPE-1)
   (5 3 wall TYPE-1)
   (4 3 wall TYPE-1)
   (3 3 wall TYPE-1)
   (8 2 wall TYPE-1)
   (6 2 wall TYPE-2)
   (8 1 wall TYPE-1)
   (6 1 wall TYPE-2)
   (7 0 portal 2 8 2))
  ((8 7 wall TYPE-1)
   (7 7 wall TYPE-1)
   (6 7 wall TYPE-1)
   (5 7 wall TYPE-1)
   (4 7 wall TYPE-1)
   (3 7 wall TYPE-1)
   (2 7 wall TYPE-2)
   (8 6 wall TYPE-1)
   (7 6 object BATTERY)
   (6 6 object GRAPHITE)
   (5 6 object GRAPHITE)
   (2 6 wall TYPE-1)
   (8 5 wall TYPE-1)
   (4 5 wall TYPE-1)
   (2 5 wall TYPE-1)
   (1 5 wall TYPE-1)
   (8 4 wall TYPE-1)
   (7 4 wall TYPE-2)
   (6 4 wall TYPE-1)
   (5 4 wall TYPE-1)
   (4 4 wall TYPE-2)
   (0 4 portal 8 7 4)
   (7 3 wall TYPE-1)
   (2 3 wall TYPE-2)
   (1 3 wall TYPE-2)
   (7 2 wall TYPE-2)
   (6 2 object VOLTAGE-CONTROLLED-LOCK)
   (5 2 wall TYPE-2)
   (4 2 wall TYPE-2)
   (3 2 wall TYPE-2)
   (2 2 wall TYPE-2)
   (6 1 portal 6 3 1)))))


(define mk-status-line
  (with-world-signature
   (let* ((in-front-object (lookup-in-front-of-hero (recreate-world)))
	  (in-front-name
	   (match in-front-object
	     (#f "nothing")
	     (('portal . _) "a passage") ; ?
	     (('wall _) "a wall")
	     (('object 'RADIATOR . _) "a deadly radiator")
	     (('object 'REACTOR . _) "a reactor")
	     (('object 'BATTERY . _) "a battery")
	     (('object 'DOOR . _) "a locked door")
	     (('object 'KEY . _) "a key")
	     (('object 'LASER-GORDIAN-KNOT . _) "the laser gordian knot")
	     (('object 'LASER-SABRE . _) "a laser sabre")
	     (('object 'LASER-TRAP . _) "a laser trap")
	     (('object 'MIRROR . _) "a mirror")
	     (('object 'GRAPHITE . _) "a graphite rod")
	     (('object 'VOLTAGE-CONTROLLED-LOCK . _) "a voltage-controlled lock")
	     (('object name . _) (symbol->string name))))
	  (carrying-name
	   (match carrying
	     (#f "nothing")
	     ('BATTERY "a battery")
	     ('KEY "a key")
	     ('GRAPHITE "a graphite rod")
	     ('MIRROR "a mirror")
	     ('LASER-SABRE "a laser sabre")
	     (otherwise (symbol->string carrying)))))
     (string-append ;"voltage "
                    ;(number->string voltage)
                    " you are carrying "
		    carrying-name
  		    " | "
		    "in front of you there is "
		    in-front-name))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the main loop crap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *game-state* 'TITLE)

(define *state* '())

(add-timer! 66
	    (lambda()
	      (match *game-state*
		('TITLE
		 (begin
		   (set! *state* (initiate-game-world)) ;; i tak i nie.
		   (set-display-procedure!
		    (mk-display-messages '(("-- P A N D O R A --" 240 66)
					   ("copyleft 2014-02-15 by drcz" 210 86)
					   ("sprites by johann-c, http://opengameart.org/users/johann-c" 120 150)
					   ("PRESS FIRE TO PLAY PANDORA." 210 230))))
		   (if (eq? *joystick* 'A)
		       (set! *game-state* 'STORY))
		   (set! *joystick* 0)))

		('STORY ;; sorry?
		 (begin
		   (set-display-procedure!
		    (mk-display-messages '(;("-- P A N D O R A --" 240 66)
					   ("WE HAVE A PARTIAL MELTDOWN CONDITION IN BOTH OUR REACTORS." 130 86)
					   ("THE RISK OF MASSIVE CONTAMINATION IS HIGH. WE HAVE INTRODUCED" 130 96)
					   ("A REMOTE CONTROLLED ROBOT TO ONE OF POWER PLANT'S SHAFTS." 130 106)
					   ("YOUR TASK IS SIMPLE -- FIND THE GRAPHITE RODS AND SHUT THE" 130 116)
					   ("REACTORS. GOOD LUCK." 130 126)
					   ("PRESS FIRE." 290 230))))
		   (if (eq? *joystick* 'A)
		       (set! *game-state* 'PLAY))
		   (set! *joystick* 0)))

		('OVER
		 (begin
		   (set-display-procedure!
		    (mk-display-messages '(("GAME OVER." 280 66)
					   ("YOU FAILED, AND SINCE WE CANNOT AFFORD ANOTHER ROBOT," 130 116)
					   ("WE ARE ALL GOING TO DIE FROM THYROID CANCER." 130 136)
					   ("PRESS FIRE TO PLAY AGAIN." 210 230))))
		   (if (eq? *joystick* 'A)
		       (set! *game-state* 'TITLE))
		   (set! *joystick* 0)))

		('FIRST-REACTOR
		 (begin
		   (set-display-procedure!
		    (mk-display-messages '(("VERY WELL, YOU HAVE MANAGED TO SHUT THE FIRST REACTOR!" 90 166)
					   ("PRESS FIRE." 210 230))))
		   (if (eq? *joystick* 'A)
		       (begin (set! *state* ((mk-progress) *state*))
			      (set! *game-state* 'PLAY)))
		   (set! *joystick* 0)))

		('WON
		 (begin
		   (set-display-procedure!
		    (mk-display-messages '(("CONGRATULATIONS!" 260 66)
					   ("YOU HAVE MANAGED TO SHUT BOTH REACTORS." 180 116)
					   ("WE CAN NOW FORGET THE WHOLE INCIDENT." 180 126)
					   ("PRESS FIRE TO PLAY AGAIN." 210 230))))
		   (if (eq? *joystick* 'A)
		       (set! *game-state* 'TITLE))
		   (set! *joystick* 0)))

		('PLAY 
		  (let ((old-state *state*))
		    (set-display-procedure! display-world)
		    (set-to-display! (current-view *state*))
		    (set! *status-line-text* (mk-status-line *state*))
		    (set! *voltage-status* ((with-world-signature voltage) *state*))
		    (set! *state*
			  ((mk-game-step *joystick*)
			   *state*))		    
		    (set! *game-state* `(ANIMATION ,(mk-animation-sequence old-state *state*)))
		    (cond (((with-world-signature (not alive?)) *state*)
			   (set! *game-state* 'OVER))
			  (((with-world-signature (= (length progress) 1)) *state*)
			   (set! *game-state* 'FIRST-REACTOR))
			  (((with-world-signature (= (length progress) 3)) *state*)
			   (set! *game-state* 'WON)))
		    (set! *joystick* 0)))

		(('ANIMATION frames)
		    (if (null? frames)
			(set! *game-state* 'PLAY)
			(match-let* ((((cur-frame sound) . frames) frames))
			  (set-display-procedure! display-world)
			  (set-to-display! cur-frame)
			  (play-sound! sound)
			  (set! *game-state* `(ANIMATION ,frames)))))
		;; ...?
		)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; koniec bomba a kto czytał ten ssie gały eurocwelom.
