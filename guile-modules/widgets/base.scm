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
	    exit

	    left-click-widget

	    <widget>
	    <extended-widget>
	    <stage>
	    <container-widget>

	    *stdout*
	    *stdin*
	    *stderr*

	    *stage*
	    *input-widget*
	    *active-widget*
	    *nearby-widget*

	    *default-font*
	    )
  #:export-syntax (default-slot-values with-slots)
  )

(define-syntax-rule (default-slot-values object args (name value) ...)
  (let-keywords args #t ((name value) ...)
    (set! #[object 'name] name)
    ...))

(define-syntax with-slots
  (syntax-rules ()
    ((_ object ((name value) ...) actions ...)
     (let ((name (slot-ref object 'name)) ...)
       (dynamic-wind (lambda ()
		       (slot-set! object 'name value)
		       ...)
		     (lambda ()
		       actions ...)
		     (lambda ()
		       (slot-set! object 'name name)
		       ...))))))

(define *stdout* (current-output-port))
(define *stdin* (current-input-port))
(define *stderr* (current-error-port))

(define *default-font* (load-font "./art/VeraMono.ttf" 10))

(define-generic draw)
(define-generic area)
(define-generic add-child!)
(define-generic remove-child!)

(define ((getter property) object)
  #[object property])

(define ((hook-adder hook-name) self value)
  (add-hook! (slot-ref self hook-name) value #t))

(define (((hook-caller hook-name) self) . args)
  (apply run-hook (slot-ref self hook-name) args))

(define ((hook . args))
  (make-hook (length args)))

(define-class <widget> ()
  (parent #:init-value #f #:init-keyword #:parent)
  (children #:allocation #:virtual #:slot-ref (lambda _ '())
	    #:slot-set! noop)
  (left-mouse-down-hook #:init-thunk (hook 'x 'y))
  (left-mouse-up-hook #:init-thunk (hook 'x 'y))
  (left-click-hook #:init-thunk (hook 'x 'y))
  (right-mouse-down-hook #:init-thunk (hook 'x 'y))
  (right-mouse-up-hook #:init-thunk (hook 'x 'y))
  (right-click-hook #:init-thunk (hook 'x 'y))

  (left-mouse-down 
   #:allocation #:virtual
   #:slot-ref (hook-caller 'left-mouse-down-hook)
   #:slot-set! (hook-adder 'left-mouse-down-hook))

  (left-mouse-up 
   #:allocation #:virtual
   #:slot-ref (hook-caller 'left-mouse-up-hook)
   #:slot-set! (hook-adder 'left-mouse-up-hook))
  (left-click 
   #:allocation #:virtual
   #:slot-ref (hook-caller 'left-click-hook)
   #:slot-set! (hook-adder 'left-click-hook))

  (right-mouse-down 
   #:allocation #:virtual
   #:slot-ref (hook-caller 'right-mouse-down-hook)
   #:slot-set! (hook-adder 'right-mouse-down-hook))
  (right-mouse-up 
   #:allocation #:virtual
   #:slot-ref (hook-caller 'right-mouse-up-hook)
   #:slot-set! (hook-adder 'right-mouse-up-hook))
  (right-click 
   #:allocation #:virtual
   #:slot-ref (hook-caller 'right-click-hook)
   #:slot-set! (hook-adder 'right-click-hook))
#|
  (left-mouse-down #:init-value noop #:init-keyword #:left-mouse-down)
  (left-mouse-up #:init-value noop #:init-keyword #:left-mouse-up)
  (left-click #:init-value noop #:init-keyword #:left-click)
  (right-mouse-down #:init-value noop #:init-keyword #:right-mouse-down)
  (right-mouse-up #:init-value noop #:init-keyword #:right-mouse-up)
  (right-click #:init-value noop #:init-keyword #:right-click)
|#
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
  (resize                               ; new-w new-h, issued before
   #:init-value noop                    ; the actual resize, so the old
   #:init-keyword #:resize)             ; values can be accessed
  (move                                 ; new-x, new-y, issued before
   #:init-value noop                    ; the position is changed
   #:init-keyword #:move)
  (on-create
   #:init-value noop
   #:init-keyword #:on-create)
  (on-exit
   #:init-value noop
   #:init-keyword #:on-exit)
  (%x #:init-value 0 #:init-keyword #:x) (%y #:init-value 0 #:init-keyword #:y)
  (%w #:init-value 0 #:init-keyword #:w) (%h #:init-value 0 #:init-keyword #:h)
  (x #:allocation #:virtual
     #:slot-ref (getter '%x)
     #:slot-set! (lambda(self x)
		   (unless (= #[self '%x] x)
		     (#[self 'move] x #[self '%y])
		     (set! #[self '%x] x))))
  (y #:allocation #:virtual
     #:slot-ref (getter '%y)
     #:slot-set! (lambda(self y)
		   (unless (= #[self '%y] y)
		     (#[self 'move] #[self '%x] y)
		     (set! #[self '%y] y))))
  (w #:allocation #:virtual
     #:slot-ref (getter '%w)
     #:slot-set! (lambda(self w)
		   (unless (= #[self '%w] w)
		     (#[self 'resize] w #[self '%h])
		     (set! #[self '%w] w))))
  (h #:allocation #:virtual
     #:slot-ref (getter '%h)
     #:slot-set! (lambda(self h)
		   (unless (= #[self '%h] h)
		     (#[self 'resize] #[self '%w] h)
		     (set! #[self '%h] h)))))

(define-method (initialize (self <widget>) args)
  (next-method)
  (#[self 'on-create] self))

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

(define-method (draw (none <boolean>))
  (noop))

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
	(apply argmax widget-depth candidates))))

(set-display-procedure! (lambda()(draw *stage*)))

(define-generic exit)

(define-method (exit (self <widget>))
  (for child in #[self 'children]
    (exit child))
  (#[self 'on-exit] self))

(set-exit-procedure! (lambda (_) (exit *stage*)))

(set-resize-procedure! (lambda (w h)
			 (set! #[*stage* 'w] w)
			 (set! #[*stage* 'h] h)))

(define left-click-widget #f)

(define (left-mouse-down x y)
  (set! *nearby-widget* #f)
  (and-let* ((w (most-nested-widget-under `(,x ,y) #;on *stage*)))
    (set! *active-widget* w))
  (when *active-widget*
    (set! left-click-widget *active-widget*)
    (#[*active-widget* 'left-mouse-down ] x y))
  (drag-over x y 0 0))

(define (left-mouse-up x y)
  (when *active-widget* 
    (#[*active-widget* 'left-mouse-up] x y)
    (when (and (eq? left-click-widget *active-widget*)
	       (in? *active-widget* (all-widgets-under `(,x ,y) #;on *stage*)))
      (#[*active-widget* 'left-click] x y)))
  (set! left-click-widget #f)
  (set! *active-widget* *stage*))

(define (right-mouse-down x y)
  (and-let* ((w (most-nested-widget-under `(,x ,y) #;on *stage*)))
    (#[ w 'right-mouse-down ] x y)))

(define (drag-over x y xrel yrel)
  (let* ((widgets-below (all-widgets-under `(,x ,y) #;on *stage*))
	 (non-active-widgets-below 
	  (filter (lambda (x) (or (eq? *active-widget* *stage*)
			     (not (in? *active-widget* `(,x ,@(ancestors x))))))
		  widgets-below)))
    (unless (null? non-active-widgets-below)
      (let ((dragover-widget (apply argmax widget-depth 
				    non-active-widgets-below)))
	(unless (equal? dragover-widget *nearby-widget*)
	  (when *nearby-widget*
	    (#[ *nearby-widget* 'mouse-out ] x y xrel yrel))
	  (set! *nearby-widget* dragover-widget)
	  (#[ *nearby-widget* 'drag-over ] x y xrel yrel))))
    (unless (null? widgets-below)
      (let ((mousemove-widget (apply argmax widget-depth widgets-below)))
	(#[mousemove-widget 'mouse-move] x y xrel yrel)))
    (#[ *active-widget* 'drag ] x y xrel yrel)))

(keydn 'mouse-left left-mouse-down)
(keyup 'mouse-left left-mouse-up)
(keydn 'mouse-right right-mouse-down)
(mousemove drag-over)

(define-class <container-widget> (<widget>)
  (%content #:init-value #f)
  (content
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     #[self '%content])
   #:slot-set!
   (lambda (self value)
     (if #[self '%content]
	 (set! #[self : '%content : 'parent] #f))
     (set! #[self '%content] value)
     (set! #[value 'parent] self)))
  (children
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (or (and-let* ((content #[self 'content]))
	   `(,content))
	 '()))
   #:slot-set!
   noop)
  (min-w #:init-value 0 #:init-keyword #:min-w)
  (min-h #:init-value 0 #:init-keyword #:min-h)
  (w
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (if #[self 'content]
	 #[self : 'content : 'w]
	 #[self 'min-w]))
   #:slot-set!
   noop)
  (h
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (if #[self 'content]
	 #[self : 'content : 'h]
	 #[self 'min-h]))
   #:slot-set!
   noop))

(define-method (draw (c <container-widget>))
  (if #[c 'content]
      (draw #[c 'content])))
