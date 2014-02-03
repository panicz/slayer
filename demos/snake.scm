#!../src/slayer -d3d
exit
!#
;; Functional snake game from http://soft.vub.ac.be/~jnicolay/code/snake.scm
;; Copyright (C) Jens Nicolay <jens.nicolay@vub.ac.be>
;; The code has been adjusted by Maciek Godek <godek.maciek@gmail.com>
;; to work with the SLAYER framework https://puszcza.gnu.org.ua/projects/slayer
(use-modules (slayer) (slayer image))

(define dir-up 1)

(define dir-down 2)

(define dir-left 3)

(define dir-right 4)

(define (dir-inverse dir)
  (cond ((= dir dir-up) dir-down)
        ((= dir dir-down) dir-up)
        ((= dir dir-left) dir-right)
        ((= dir dir-right) dir-left)))

(define (xy x y) (cons x y))

(define (xy-x p) (car p))

(define (xy-y p) (cdr p))

(define (xy-in-range? p x1 y1 x2 y2)
  (let ((x (xy-x p))
        (y (xy-y p)))
    (and (>= x 1) (>= y y1) (<= x x2) (<= y y2))))

(define (xy-move p dir)
  (let ((x (xy-x p))
	(y (xy-y p)))
    (cond ((= dir dir-up) (xy x (- y 1)))
          ((= dir dir-down) (xy x (+ y 1)))
          ((= dir dir-left) (xy (- x 1) y))
          ((= dir dir-right) (xy (+ x 1) y)))))

(define (snake p segs) (cons p segs))

(define (snake-xy s) (car s))

(define (snake-segs s) (cdr s))

(define (snake-add s dir)
  (let ((p (snake-xy s))
        (segs (snake-segs s)))
    (let ((newP (xy-move p dir))
          (newSegs (cons dir segs)))
      (snake newP newSegs))))

(define (snake-move s dir)
  (let ((p (snake-xy s))
        (segs (snake-segs s)))
    (let ((newP (xy-move p dir))
          (newSegs (cons dir (reverse (cdr (reverse segs))))))
      (snake newP newSegs))))

(define (snake-xys s)
  (let loop ((p (snake-xy s)) (dirs (snake-segs s)) (xys (list (snake-xy s))))
    (if (null? dirs)
	(reverse xys)
	(let* ((dir (car dirs))
	       (newP (xy-move p (dir-inverse dir))))
	  (loop newP (cdr dirs) (cons newP xys))))))

(define (snake-on-snake? s p)
  (let ((xys (snake-xys s)))
    (member p xys)))

(define (board n running s score)
  (vector n running s score))

(define (board-size b) (vector-ref b 0))

(define (board-running? b) (vector-ref b 1))

(define (board-snake b) (vector-ref b 2))

(define (board-score b) (vector-ref b 3))

(define (board-step b dir)
  (if (not (board-running? b))
      b
      (let ((score (board-score b)))
        (let* ((s (board-snake b))
               (p (snake-xy s))
               (newP (xy-move p dir)))
          (if (and (board-in-range? b newP) 
		   (not (snake-on-snake? s newP)))
              (let ((newS (snake-move s dir))
		    (newScore (+ score 1)))
                (board (board-size b) #t newS newScore))
              (board (board-size b) #f s score))))))

(define (board-in-range? b p)
  (let ((n (board-size b)))
    (xy-in-range? p 0 0 (- n 1) (- n 1))))

;;;;;

(define (setup-board n)
  (let ((snake (snake (xy (- (quotient n 2) 4) (quotient n 2)) 
		      (list dir-right))))
    (board n #t (snake-add 
		 (snake-add 
		  (snake-add 
		   (snake-add snake dir-right) 
		   dir-right) 
		  dir-right) 
		 dir-right) 
	   0)))

;;;;;

(define b (setup-board 16))

(define current-dir dir-right)

(define period 100)

(define timer (add-timer! 100 (lambda()(set! b (board-step b current-dir)))))

(keydn 'esc quit)
(keydn 'left (lambda () (set! current-dir dir-left)))
(keydn 'right (lambda () (set! current-dir dir-right)))
(keydn 'up (lambda () (set! current-dir dir-up)))
(keydn 'down (lambda () (set! current-dir dir-down)))

(define tile-width 20)

(define tile-height 20)

(define tile (array->image (make-typed-array 'u32 #xffffffff 
					     tile-width tile-height)))

(set-display-procedure!
 (lambda ()
   (for-each (lambda (xy)
	       (let ((x (* tile-width (xy-x xy)))
		     (y (* tile-height (xy-y xy))))
		 (draw-image! tile x y)))
       (snake-xys (board-snake b)))))

(set-screen-size! (* tile-width (board-size b)) (* tile-height (board-size b)))
(set-window-title! "SNAKE")
