(define-module (widgets base)
  #:use-module (oop goops)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (slayer)
  #:use-module (slayer font)
  #:export (
	    update!
	    draw
	    area
	    absolute-area
	    in-area?
	    add-child!
	    remove-child!
	    ancestors
	    widget-nested-find

	    <widget>
	    <extended-widget>

	    *stdout*
	    *stdin*
	    *stderr*

	    *stage*
	    *input-widget*
	    *active-widget*
	    *nearby-widget*
	    select-widget-at
	    unselect-widget-at
	    right-click-widget-at
	    drag-over-widget

	    *default-font*
	    ))

(define *stdout* (current-output-port))
(define *stdin* (current-input-port))
(define *stderr* (current-error-port))

(define *default-font* (load-font "./art/VeraMono.ttf" 12))

(define-generic update!)
(define-generic draw)
(define-generic area)
(define-generic add-child!)
(define-generic remove-child!)

(define-class <widget> ()
  (parent #:init-value #f #:init-keyword #:parent)
  (children #:init-value '() #:init-keyword #:children)
  (left-mouse-down #:init-value noop #:init-keyword #:left-mouse-down)
  (left-mouse-up #:init-value noop #:init-keyword #:left-mouse-up)
  (right-mouse-down #:init-value noop #:init-keyword #:right-mouse-down)
  (right-mouse-up #:init-value noop #:init-keyword #:right-mouse-up)
  (mouse-over #:init-value noop #:init-keyword #:mouse-over)
  (mouse-out #:init-value noop #:init-keyword #:mouse-out)
  (drag #:init-value noop #:init-keyword #:drag)
  (update!  #:init-value noop #:init-keyword #:update)
  (activate #:init-value noop #:init-keyword #:activate)
  (deactivate #:init-value noop #:init-keyword #:deactivate)
  (resize #:init-value noop #:init-keyword #:resize)
  (x #:init-value 0 #:init-keyword #:x)
  (y #:init-value 0 #:init-keyword #:y)
  (w #:init-value 0 #:init-keyword #:w)
  (h #:init-value 0 #:init-keyword #:h))

(define-class <extended-widget> (<widget>)
  (data #:init-thunk make-hash-table))

(define-method (ancestors (w <widget>))
  (or (and-let* ((parent #[w 'parent]))
	(cons parent (ancestors parent)))
      '()))

(define-method (area (w <widget>))
  (list #[ w 'x ] #[ w 'y ] #[ w 'w ] #[ w 'h ]))

(define-method (absolute-area (w <widget>))
  (let ((widgets (cons w (ancestors w))))
    (list (apply + (map #[_ 'x] widgets))
	  (apply + (map #[_ 'y] widgets))
	  #[w 'w] #[w 'h])))

(define-method (draw (w <widget>))
  (for-each draw (reverse #[ w 'children ])))

(define-method (add-child! (parent <widget>) (child <widget>))
  (set! #[ parent 'children ] (cons child #[ parent 'children ]))
  (set! #[ parent 'w ] (max #[ parent 'w ] (+ #[ child 'x ] #[ child 'w ])))
  (set! #[ parent 'h ] (max #[ parent 'h ] (+ #[ child 'y ] #[ child 'h ])))
  (set! #[ child 'parent ] parent))

(define-method (remove-child! (parent <widget>) (child <widget>))
  (set! #[ child 'parent ] #f)
  (set! #[ parent 'children ] (delete child #[ parent 'children ])))

(define (in-area? point area)
  (match-let
   (((x y w h) area)
    ((px py) point))
   (and (<= x px (+ x w)) (<= y py (+ y h)))))

(define (widget-nested-find condition widget)
  (if (not (condition widget))
      #f
      (let ((w (find condition (slot-ref widget 'children))))
	(if (not w)
	    widget
	    (let ((c (widget-nested-find condition w)))
	      (if (not c) w c))))))

(define *stage* (make <widget> #:w (screen-width) #:h (screen-height)))

(define *input-widget* #f)
(define *active-widget* *stage*)
(define *nearby-widget* #f)
(set-display-procedure! (\ draw *stage*))
(set-resize-procedure! (lambda (w h)
			 (set! #[*stage* 'w] w)
			 (set! #[*stage* 'h] h)))
(define (select-widget-at x y)
  (and-let* ((w (widget-nested-find 
		 (lambda(w)
		   (in-area? (list x y)
			     (absolute-area w)))
		 *stage*)))
    (set! *active-widget* w))
  (if *active-widget* (#[*active-widget* 'left-mouse-down ] x y)))


(define (unselect-widget-at x y)
  (if *active-widget* (#[*active-widget* 'right-mouse-up] x y))
  (set! *active-widget* *stage*))

(define (right-click-widget-at x y)
  (and-let* ((w (widget-nested-find 
		 (lambda(w)
		   (in-area? (list x y)
			     (absolute-area w)))
		 *stage*)))
    (#[ w 'right-mouse-down ] x y)))

(define (drag-over-widget x y xrel yrel)
   (let ((mouseover-widget 
	  (widget-nested-find 
	   (lambda (w) 
	     (in-area? (list x y) (absolute-area w))) 
	   *stage*)))
     (if (and mouseover-widget (not (equal? mouseover-widget *nearby-widget*)))
	 (begin
	   (if *nearby-widget* (#[ *nearby-widget* 'mouse-out ] x y xrel yrel))
	   (set! *nearby-widget* mouseover-widget)
	   (#[ *nearby-widget* 'mouse-over ] x y xrel yrel)))
     (#[ *active-widget* 'drag ] x y xrel yrel)))

(keydn 'mouse-left select-widget-at)
(keyup 'mouse-left unselect-widget-at)
(keydn 'mouse-right right-click-widget-at)
(mousemove drag-over-widget)
