(define-module (widgets base)
  #:use-module (oop goops)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (slayer)
  #:use-module (slayer font)
  #:export (
	    draw
	    area
	    absolute-area
	    in-area?
	    add-child!
	    remove-child!
	    ancestors

	    <widget>
	    <extended-widget>
	    <stage>

	    *stdout*
	    *stdin*
	    *stderr*

	    *stage*
	    *input-widget*
	    *active-widget*
	    *nearby-widget*

	    *default-font*
	    )
  #:export-syntax (default-slot-values)
  )

(define-syntax default-slot-values
  ;; this macro is intended to be used within an "initialize" method.
  (syntax-rules ()
    ((_ object args (name value) ...)
     (let-keywords args #t ((name value) ...)
       (set! #[object 'name] name)
       ...))))

(define *stdout* (current-output-port))
(define *stdin* (current-input-port))
(define *stderr* (current-error-port))

(define *default-font* (load-font "./art/VeraMono.ttf" 10))

(define-generic draw)
(define-generic area)
(define-generic add-child!)
(define-generic remove-child!)

(define-class <widget> ()
  (parent #:init-value #f #:init-keyword #:parent)
  (children #:allocation #:virtual #:slot-ref (lambda _ '())
	    #:slot-set! noop)
  (left-mouse-down #:init-value noop #:init-keyword #:left-mouse-down)
  (left-mouse-up #:init-value noop #:init-keyword #:left-mouse-up)
  (left-click #:init-value noop #:init-keyword #:left-click)
  (right-mouse-down #:init-value noop #:init-keyword #:right-mouse-down)
  (right-mouse-up #:init-value noop #:init-keyword #:right-mouse-up)
  (right-click #:init-value noop #:init-keyword #:right-click)

  (mouse-move                           ; issued whenever a mouse cursor
   #:init-value noop                    ; is moved over a widget
   #:init-keyword #:mouse-move)
  (drag-over                            ; issued when the active widget
   #:init-value noop                    ; becomes visible to some nearby
   #:init-keyword #:mouse-over)         ; widget

  (mouse-out #:init-value noop #:init-keyword #:mouse-out)
  (drag #:init-value noop #:init-keyword #:drag)
  (activate #:init-value noop #:init-keyword #:activate)
  (deactivate #:init-value noop #:init-keyword #:deactivate)
  (resize #:init-value noop #:init-keyword #:resize); new-w new-h old-w old-h
  (x #:init-value 0 #:init-keyword #:x)
  (y #:init-value 0 #:init-keyword #:y)
  (%w #:init-value 0 #:init-keyword #:w)
  (%h #:init-value 0 #:init-keyword #:h)
  (w #:allocation #:virtual
     #:slot-ref (lambda(self)#[self '%w])
     #:slot-set! (lambda(self w)
		   (#[self 'resize] w #[self '%h])
		   (set! #[self '%w] w)))
  (h #:allocation #:virtual
     #:slot-ref (lambda(self)#[self '%h])
     #:slot-set! (lambda(self h)
		   (#[self 'resize] #[self '%w] h)
		   (set! #[self '%h] h))))

(define-class <stage> (<widget>)
  (children #:init-value '()))

;; the main stage widget
(define *stage* (make <stage> #:w (screen-width) #:h (screen-height)))

;; a widget that collects text input
(define *input-widget* #f)

;; *active-widget* is a widget that is currently being dragged
(define *active-widget* *stage*)

;; *nearby-widget* is a widget that the active widget is being
;; dragged over
(define *nearby-widget* #f)

(define-class <extended-widget> (<widget>)
  (data #:init-thunk make-hash-table))

(define-method (ancestors (w <widget>))
  (or (and-let* ((parent #[w 'parent]))
	(cons parent (ancestors parent)))
      '()))

(define-method (widget-depth (w <widget>))
  (length (ancestors w)))

(define-method (area (w <widget>))
 (list #[ w 'x ] #[ w 'y ] #[ w 'w ] #[ w 'h ]))

(define-method (absolute-area (w <widget>))
  (let ((widgets (cons w (ancestors w))))
    (list (apply + (map #[_ 'x] widgets))
	  (apply + (map #[_ 'y] widgets))
	  #[w 'w] #[w 'h])))

(define-method (draw (w <widget>))
  (for-each draw (reverse #[ w 'children ])))

(define-method (add-child! (child <widget>) #;to (parent <widget>))
  (set! #[ parent 'children ] (cons child #[ parent 'children ]))
  (set! #[ parent 'w ] (max #[ parent 'w ] (+ #[ child 'x ] #[ child 'w ])))
  (set! #[ parent 'h ] (max #[ parent 'h ] (+ #[ child 'y ] #[ child 'h ])))
  (set! #[ child 'parent ] parent))
  
(define-method (remove-child! (child <widget>) #;from (parent <widget>))
  ;; the parent widget size should be recalculated
  (set! #[ child 'parent ] #f)
  (set! #[ parent 'children ] (delete child #[ parent 'children ])))

(define (in-area? point area)
  (match-let (((x y w h) area)
	      ((px py) point))
   (and (<= x px (+ x w)) (<= y py (+ y h)))))

(define (all-widgets-under position #;from root)
  (if (in-area? position (absolute-area root))
      (cons root (append-map (lambda(w)
			       (all-widgets-under position #;from w))
			     #[root 'children]))
      '()))

(define (most-nested-widget-under position #;from root)
  (let ((candidates (all-widgets-under position #;from root)))
    (if (null? candidates)
	#f
	(argmax widget-depth candidates))))

(set-display-procedure! (lambda()(draw *stage*)))

(set-resize-procedure! (lambda (w h)
			 (set! #[*stage* 'w] w)
			 (set! #[*stage* 'h] h)))

(define left-click-time #f)

(define (left-mouse-down x y)
  (set! *nearby-widget* #f)
  (and-let* ((w (most-nested-widget-under `(,x ,y) *stage*)))
    (set! *active-widget* w))
  (when *active-widget*
    (#[*active-widget* 'left-mouse-down ] x y))
  (drag-over x y 0 0)
  (set! left-click-time (current-microtime)))

(define (left-mouse-up x y)
  (when *active-widget* 
    (#[*active-widget* 'left-mouse-up] x y)
    (if left-click-time
	(#[*active-widget* 'left-click] x y)))
  (set! *active-widget* *stage*))

(define (right-mouse-down x y)
  (and-let* ((w (most-nested-widget-under `(,x ,y) *stage*)))
    (#[ w 'right-mouse-down ] x y)))

(define (drag-over x y xrel yrel)
  (let* ((widgets-below (all-widgets-under `(,x ,y) *stage*))
	 (non-active-widgets-below 
	  (filter (lambda (x)
		    (not (in? *active-widget* `(,x ,@(ancestors x)))))
		  widgets-below)))
    (if (not (null? non-active-widgets-below))
	(let ((dragover-widget (argmax widget-depth non-active-widgets-below)))
	  (when (not (equal? dragover-widget *nearby-widget*))
	    (if *nearby-widget* 
		(#[ *nearby-widget* 'mouse-out ] x y xrel yrel))
	    (set! *nearby-widget* dragover-widget)
	    (#[ *nearby-widget* 'drag-over ] x y xrel yrel))))
    (if (not (null? widgets-below))
	(let ((mousemove-widget (argmax widget-depth widgets-below)))
	  (#[mousemove-widget 'mouse-move] x y xrel yrel)))
    (#[ *active-widget* 'drag ] x y xrel yrel)
    (set! left-click-time #f)))

(keydn 'mouse-left left-mouse-down)
(keyup 'mouse-left left-mouse-up)
(keydn 'mouse-right right-mouse-down)
(mousemove drag-over)
