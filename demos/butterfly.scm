#!../src/slayer -r -d3d
!#
(use-modules (ice-9 q)
	     (slayer)
	     (slayer image)
	     (slayer drawing)
	     (extra slayer)
	     (extra threads)
	     (extra drawing)
	     (extra attributes)
	     (butterfly render)
	     (butterfly cursor)
	     (butterfly selection)
	     (butterfly categories)
	     (butterfly parameters)
	     (grand scheme))

(define EVENT-QUEUE (make-channel))

(set-reaction!
 (lambda (event)
   (send! event #;through EVENT-QUEUE)))

(set-input-mode! 'reactive)

(set-window-title! "butterfly")

(define initial-state
  (editor-state-category
   #:document '((let ((x 5)
		      (y 10))
		  (+ x y z))
		(let ((a 'x)
		      (b 'y)
		      (c (+ 2 2)))
		  `(,x y))
		)))

(define evolution
  (call-with-new-thread
   (lambda ()
     (visible initial-state)
     (channel-fold
      (lambda (state action)
	(parameterize ((current-editor-state state))
	  (let ((state* (or (update state #;with action)
			    state)))
	    (visible state*))))
      initial-state
      EVENT-QUEUE))))


;; state -> drawing
(define (visualization state)
  (below
   (space #:height 20)
   (beside
    (space #:width 20)
    (or (and-let* ((cursor (current #:cursor))
		   (document (current #:document))
		   (selection (current #:selection))
		   (document-with-cursor (insert-cursor #;into document
							       #;at cursor)))
	  (apply below
		 (map (lambda (expression . index)
			(below
			 (parameterize ((current-font "Courier New"))
			   (render expression #;at index))
			 (space #:height 5)))
		      document-with-cursor
		      (iota (length document-with-cursor)))))
	'()))))

(set-display-procedure!
 (lambda ()
   (let ((current-state (snapshot evolution)))
     (parameterize ((current-editor-state current-state))
       (draw! (visualization current-state))))))

;; update state action -> maybe state
(define+ (update state #;with action)
  state)

(define+ (update state #;with '(key-down right))
  (let ((cursor* (cursor-next #;to (current #:cursor)
				   #;in (current #:document))))
    (display cursor*)(display (focus (current #:document) cursor*))(newline)
    (merge-attributes state `(#:cursor ,cursor*))))

(define+ (update state #;with '(key-down left))
  (let ((cursor* (cursor-previous #;to (current #:cursor)
				       #;in (current #:document))))
    (display cursor*)(display (focus (current #:document) cursor*))(newline)
    (merge-attributes state `(#:cursor ,cursor*))))

(define+ (update state #;with '(key-down c))
  (display (current #:cursor))
  (display (cursor-next #;to (current #:cursor) #;in (current #:document)))
  (display (focus (current #:document) #;on (current #:cursor) ))
  (display (focus (current #:document) #;on (cursor-next
					     #;to (current #:cursor)
						  #;in (current #:document))))
  (display (insert-cursor (current #:document) (current #:cursor)))
  (newline)
  
  state)

#|
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
		 
