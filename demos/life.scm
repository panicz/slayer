#!../src/slayer
!#
(use-modules (slayer) (slayer image) (extra slayer)
	     (extra common))

(keydn 'esc quit)

(define board-width 32)
(define board-height 24)

(define fragment-width (quotient (screen-width) board-width))
(define fragment-height (quotient (screen-height) board-height))

(define fragment 
  (array->image ;; a white square (fragment-width x fragment-height)
   (make-typed-array 'u32 #xffffffff fragment-width fragment-height)))

(define (new-board) (make-array #f board-height board-width))

(define board (new-board))
(define copy (new-board))

(keydn 'space (lambda()(array-map! board (lambda _ (> (random 2) 0)))))

(define (neighbours #;on board #;at x y)
  (match-let (((w h) (array-dimensions board))
	      (at (lambda(x y)
		    (array-ref board x y))))
    `(,(at (modulo (- x 1) w) y)
      ,(at x (modulo (+ y 1) h))
      ,(at (modulo (+ x 1) w) y)
      ,(at x (modulo (- y 1) h)))))

(let ((board #2((a b c d)
		(e f g h)
		(i j k l))))
  (array-ref board 2 1))

(define (evolve!)
  (for i in 0 .. (1- board-width)
       (for j in 0 .. (1- board-height)
	    (let*-values (((current neighbours)
			   (values (array-ref board j i)
				   (neighbours #;on board #;at j i)))
			  ((neighbours-alive)
			   (filter (lambda(x)x) neighbours))
			  ((alive) (length neighbours-alive)))
	      (array-set! 
	       copy
	       (cond 
		;; Any live cell with fewer than two live neighbours dies,
		;; as if caused by under-population.
		((and current (< alive 2))
		 #f)
		;; Any live cell with two or three live neighbours
		;; lives on to the next generation.
		((and current (or (= alive 2) (= alive 3)))
		 current)
		;; Any live cell with more than three live neighbours 
		;; dies, as if by overcrowding.
		((and current (> alive 3))
		 #f)
		;; Any dead cell with exactly three live neighbours
		;; becomes a live cell, as if by reproduction.
		((and (not current) (= alive 3))
		 #t)
		(else
		 #f))
	       j i))))
  (let ((temp board))
    (set! board copy)
    (set! copy temp))
  (force-redisplay!))
    
(set-display-procedure!
 (lambda ()
   (for i in 0 .. (1- board-width)
	(for j in 0 .. (1- board-height)
	     (if (array-ref board j i)
		 (draw-image! fragment 
			      (* i fragment-width)
			      (* j fragment-height)))))))

(keydn 'mouse-left
  (lambda (x y)
    (let ((i (quotient x fragment-width))
	  (j (quotient y fragment-height)))
      (array-set! board (not (array-ref board j i)) j i))))

(define timer 
  (add-timer! 300 evolve!))
