#!../src/slayer -r -d3d
!#
(use-modules (ice-9 nice-9)
	     (ice-9 q)
	     (slayer)
	     (slayer image)
	     (slayer drawing)
	     (extra slayer)
	     (extra threads)
	     (extra drawing)
	     (extra attributes)
	     (lib))

#;(set-display-procedure!
 (lambda ()
   (draw! '((shape #:contour-thickness 1.0 #:contour-color (1 0 0)
		   (0 0) (0 1) (1 1) (1 0))
	    (space #:position (5 5) #:size (2 2))
	    (caption #:position (10 10) #:text "dupa" #:font "cairo:monospace"
		     #:size 12)))))

(define EVENT-QUEUE (make-channel))

(set-reaction!
 (lambda (event)
   (send! event #;through EVENT-QUEUE)))

(set-input-mode! 'reactive)

(set-window-title! "butterfly")

;; chcilibyśmy powiedzieć raczej, że aktualizacja przebiega
;; poprzez fold-leftowanie 

(define initial-state
  (list
   #:document '((let ((x 5) (y 10)) (+ x y)))
   #:cursor '()
   #:selection '()
   ))

(define evolution
  (call-with-new-thread
   (lambda ()
     (visible initial-state)
     (channel-fold
      (lambda (state action)
	(let ((state* (or (update state #;with action)
			  state)))
	  (visible state*)))
      initial-state
      EVENT-QUEUE))))

;; state -> drawing
(define (visualization state)
  (or (and-let* ((cursor ((from state) #:cursor))
		 (document ((from state) #:document)))
	(map (lambda (expression . index)
	       (edited expression #;under-cursor))
	     document (iota (length document))))
      '()))

(set-display-procedure!
 (lambda ()
   (let ((current-state (snapshot evolution)))
     (draw! (visualization current-state)))))

(define+ (edited s-exp #;cursor)
  (caption (->string s-exp) #:at '(10 10)))

(define (indentation)
  (space #:width 10))

(define+ (edited ('let (bindings ...) body ...) #;cursor)
  (below
   (beside (caption "let" #:at '(10 10))
	   (apply above (map edited bindings)))
   (beside (indentation)
	   (apply above (map edited body)))))

(define+ (edited ('expt x y))
  (beside x (superscript y)))

;; update state action -> maybe state
(define (update state #;with action)
  state)

#|
(define+ (update state #;with '(key-down right))
aqg  ;; move the cursor right (at the current nesting level)
  ...)

(define+ (update state #;with '(key-down left))
  ;; move the cursor left (at the current nesting level)
  ...)

(define+ (update state #;with ('key-down key))
  ;; move the cursor to the lower nesting level
  ...)

(define+ (update state #;with '(key-down mouse-left))
  ;; select the most specific s-expression under cursor
  ;; (
  (and-let* ((position (current-mouse-position))
	     (selected (object-at position #;from state)))
    (extend state #;with #:selected selected)))

(define+ (update state #;with '(key-up mouse-left))
  (remove #:selected #;from state))

(define+ (update state #;with ('mouse-move dx dy))
  (and-let* ((selected ((from state) #:selected)))
    ;; trzeba 
    ...))

|#
