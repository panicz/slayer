#!../src/slayer
!#
(use-modules (slayer) (slayer image)
	     (extra common))
(cond-expand 
 (slayer-audio (use-modules (slayer audio)) (display "using audio\n"))
 (else (define load-sound noop)
       (define play-sound! noop)
       (display "dismissing audio\n")))

(keydn 'esc quit)
(define board-width 64)
(define board-height 48)
(define ball-width (quotient (screen-width) board-width))
(define ball-height (quotient (screen-height) board-height))

(define ball (array->image ;; a white square (ball-width x ball-height)
	      (make-typed-array 'u32 #xffffffff ball-width ball-height)))

;(define guard (make-mutex))

(define ball-x #f)
(define ball-y #f)
(define ball-vx #f)
(define ball-vy #f)

(seed->random-state (vector-ref (times) 0))

(define (reset-ball!)
  (set! ball-x (quotient board-width 2))
  (set! ball-y (quotient board-height 2))
  (set! ball-vx (1- (* 2 (random 2))))
  (set! ball-vy (1- (* 2 (random 2)))))

(reset-ball!)

(define paddle-a-size 5)
(define paddle-a-x 1)
(define paddle-a-y (quotient (- board-height paddle-a-size) 2))
(define paddle-a-v 0)

(define paddle-b-size 5)
(define paddle-b-x (- board-width 2))
(define paddle-b-y (quotient (+ board-height paddle-b-size) 2))
(define paddle-b-v 0)

(define digits
  (list->vector
   (map 
    (lambda(l)(list->array 2 l))
    (map 
     list
     '((8 8 8)(_ _ 8)(8 8 8)(8 8 8)(8 _ 8)(8 8 8)(8 8 8)(8 8 8)(8 8 8)(8 8 8))
     '((8 _ 8)(_ _ 8)(_ _ 8)(_ _ 8)(8 _ 8)(8 _ _)(8 _ _)(_ _ 8)(8 _ 8)(8 _ 8))
     '((8 _ 8)(_ _ 8)(8 8 8)(8 8 8)(8 8 8)(8 8 8)(8 8 8)(_ _ 8)(8 8 8)(8 8 8))
     '((8 _ 8)(_ _ 8)(8 _ _)(_ _ 8)(_ _ 8)(_ _ 8)(8 _ 8)(_ _ 8)(8 _ 8)(_ _ 8))
     '((8 8 8)(_ _ 8)(8 8 8)(8 8 8)(_ _ 8)(8 8 8)(8 8 8)(_ _ 8)(8 8 8)(8 8 8))))))

(define glyph-width 3)
(define glyph-height 5)

(define (draw-digit d x y)
  (match-let* ((glyph (vector-ref digits (modulo d 10)))
	       ((rows cols) (array-dimensions glyph)))
    (for j in 0 .. (1- rows)
	 (for i in 0 .. (1- cols)
	      (if (not (eq? (array-ref glyph j i) '_))
		  (draw-image! ball (+ x (* i ball-width)) 
			       (+ y (* j ball-height))))))))

(define points-a 0)
(define points-b 0)

(set-display-procedure! 
 (lambda()
   ;; draw board
   (for i in 0 .. board-width
	(draw-image! ball (* i ball-width) 0)
	(draw-image! ball (* i ball-width) (* (1- board-height) ball-height)))
   (for i in 0 .. (/ board-height 2)
	(draw-image! ball (* ball-width (/ board-width 2)) (* 2 i ball-height)))
   ;; draw ball
   (draw-image! ball (* ball-width ball-x) (* ball-height ball-y))
   ;; draw paddles
   (for i in 0 .. paddle-a-size
	(draw-image! ball (* ball-width paddle-a-x) 
		    (* (+ i paddle-a-y) ball-height)))

   (for i in 0 .. paddle-b-size
	(draw-image! ball (* ball-width paddle-b-x) 
		    (* (+ i paddle-b-y) ball-height)))
   ;; draw score
   (draw-digit points-a (* ball-width
			   (- (floor (/ board-width 2)) (* 2 glyph-width)))
	       (* 3 ball-height))
   (draw-digit points-b (* ball-width
			   (+ (floor (/ board-width 2)) 
			      (inexact->exact (floor (* 1.5 glyph-width)))))
	       (* 3 ball-height))))

(define bounce-sound (load-sound "art/sounds/alert.wav"))
(define score-sound (load-sound "art/sounds/explode.wav"))

(define (wait usecs)
  (let loop ((usecs (usleep usecs)))
    (if (> usecs 0) 
	(loop (usleep usecs)))))

#;(define-syntax-rule (utimer usecs action ...)
  (let ((tick (register-userevent! (lambda () action ...))))
    (call-with-new-thread (lambda () (while #t
				       (generate-userevent! tick)
				       (wait usecs))))))

(add-timer!
 30
 (lambda ()
   (increase! ball-x ball-vx)
   (increase! ball-y ball-vy)
   (increase! paddle-a-y paddle-a-v)
   (increase! paddle-b-y paddle-b-v)
   (if (or (<= ball-y 1)
	   (>= ball-y (- board-height 2)))
       (begin 
	 (play-sound! bounce-sound)
	 (multiply! ball-vy -1)))
   (if (or (and (= ball-x (1- paddle-b-x))
		(> ball-vx 0)
		(<= paddle-b-y ball-y (+ paddle-b-y paddle-b-size)))
	   (and (= ball-x (1+ paddle-a-x))
		(< ball-vx 0)
		(<= paddle-a-y ball-y (+ paddle-a-y paddle-a-size))))
       (begin 
	 (play-sound! bounce-sound)
	 (multiply! ball-vx -1)))
   (if (> ball-x board-width)
       (begin
	 (play-sound! score-sound)
	 (increase! points-a 1)
	 (reset-ball!)))
   (if (< ball-x 0)
       (begin
	 (play-sound! score-sound)
	 (increase! points-b 1)
	 (reset-ball!)))
   (if (or (> points-a 9)
	   (> points-b 9))
       (quit))))

(keydn 'a (lambda()(decrease! paddle-a-v 1)))
(keyup 'a (lambda()(increase! paddle-a-v 1)))
(keydn 'z (lambda()(increase! paddle-a-v 1)))
(keyup 'z (lambda()(decrease! paddle-a-v 1)))

(keydn ";" (lambda()(decrease! paddle-b-v 1)))
(keyup ";" (lambda()(increase! paddle-b-v 1)))
(keydn "." (lambda()(increase! paddle-b-v 1)))
(keyup "." (lambda()(decrease! paddle-b-v 1)))

(keydn 'g gc)