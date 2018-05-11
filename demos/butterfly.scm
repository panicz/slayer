#!../src/slayer -r -d3d
!#
(use-modules (ice-9 q)
	     (ice-9 pretty-print)
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
	  (let ((state* (or (update+ state #;with action)
			    state)))
	    (visible state*))))
      initial-state
      EVENT-QUEUE))))


(define (b size)
  (box
   (space #:size `(,size ,size))
   #:contour-thickness 2.0
   #:contour-color '(1.0 1.0 1.0)))


(define (agency+ expression selector)
  expression)

(define (render reagent)
  (render-s-expression reagent))

;; state -> drawing
(define (visualization state)
  (below
   (space #:height 100)
   (beside
    (space #:width 50)
    (or (and-let* ((cursor (current #:cursor))
		   (document (with-cursor cursor (current #:document)))
		   (reagent (agency+ document '())))
	  
	  (apply below
		 (map (lambda (expression . prefix)
			(below
			 (parameterize ((current-font "Courier New")
					(current-font-size 26)
					(current-font-slant 'normal))
			   (render (agency+ expression prefix)))
			 (space #:height 5)))
		      document
		      (iota (length document)))))
	(space)))))

(set-display-procedure!
 (lambda ()
   (let ((current-state (snapshot evolution)))
     (parameterize ((current-editor-state current-state))
       (draw! (visualization current-state))))))

(define+ (update+ state #;with `(mouse-move . ,_))
  state)

;; update+ state action -> maybe state
(define+ (update+ state #;with action)
  (display action) (newline)
  state)

(define+ (update+ state #;with `(window-resize ,width ,height))
  (available-width width)
  (update-attributes state #:window-size `(,width ,height)))

(define (display-expression-at cursor)
  (display cursor)
  (display (focus (current #:document) cursor))
  (newline))

(define+ (update+ state #;with '(key-down right))
  (let* ((cursor (current #:cursor))
	 (cursor* (cursor-next #;to cursor #;in (current #:document))))
    (display-expression-at cursor*)
    (update-attributes state #:cursor cursor*)))

(define+ (update+ state #;with '(key-down left))
  (let* ((cursor (current #:cursor))
	 (cursor* (cursor-previous #;to cursor #;in (current #:document))))
    (display-expression-at cursor*)
    (update-attributes state #:cursor cursor*)))

(define+ (update+ state #;with '(key-down up))
  (let* ((cursor (current #:cursor))
	 (cursor* (cursor-previous/same-level
		   #;to cursor #;in (current #:document))))
    (display-expression-at cursor*)
    (update-attributes state #:cursor cursor*)))

(define+ (update+ state #;with '(key-down down))
  (let* ((cursor (current #:cursor))
	 (cursor* (cursor-next/same-level
		   #;to cursor #;in (current #:document))))
    (display-expression-at cursor*)
    (update-attributes state #:cursor cursor*)))

(define-syntax (debug datum)
  (begin
    (display 'datum)
    (display ":\t")
    (display datum)
    (newline)))

(define+ (update+ state #;with '(key-down c))
  (let ((cursor (current #:cursor))
	(document (current #:document)))
    (debug cursor)
    (debug (cursor-next #;to cursor #;in document))
    (debug (focus document #;on cursor))
    (debug (focus document #;on (cursor-next #;to cursor #;in document)))
    (debug (with-cursor cursor document))
    state))




(define+ (update+ state #;with '(key-down mouse-left mouse-position))
  (or (and-let* ((selected (reagent-at position #;from state)))
	(update-attributes state #:dragged selected))))


 #|

(define+ (update+ state #;with '(key-down left))
  (or (and-let* ((document ((from state) #:document))
		 (reagent (agency+ document '())))
	)
      state))

(define+ (update+ state #;with ('key-down key))
  ;; move the cursor to the lower nesting level
  ...)

(define+ (update+ state #;with '(key-up mouse-left))
  (remove #:selected #;from state))

(define+ (update+ state #;with ('mouse-move dx dy))
  (and-let* ((dragged ((from state) #:selected)))
    ;; trzeba 
    ...))

|#		 
