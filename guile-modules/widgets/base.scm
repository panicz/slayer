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
	    <stage>

	    *stdout*
	    *stdin*
	    *stderr*

	    *stage*
	    *input-widget*
	    *active-widget*
	    *nearby-widget*

	    *default-font*
	    ))

(define *stdout* (current-output-port))
(define *stdin* (current-input-port))
(define *stderr* (current-error-port))

(define *default-font* (load-font "./art/VeraMono.ttf" 10))

(define-generic update!)
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
  (mouse-over #:init-value noop #:init-keyword #:mouse-over)
  (mouse-out #:init-value noop #:init-keyword #:mouse-out)
  (drag #:init-value noop #:init-keyword #:drag)
  (update!  #:init-value noop #:init-keyword #:update)
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
		   (set! #[self '%w] w)
		   (#[self 'resize] w #[self '%h])))
  (h #:allocation #:virtual
     #:slot-ref (lambda(self)#[self '%h])
     #:slot-set! (lambda(self h)
		   (set! #[self '%h] h)
		   (#[self 'resize] #[self '%w] h))))

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
  ;; tutaj trzeba by było jeszcze zmienić rozmiar widgetu
  (set! #[ child 'parent ] #f)
  (set! #[ parent 'children ] (delete child #[ parent 'children ])))

(define (in-area? point area)
  (match-let (((x y w h) area)
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

(define-class <stage> (<widget>)
  (children #:init-value '()))

(define *stage* (make <stage> #:w (screen-width) #:h (screen-height)))

(define *input-widget* #f)
(define *active-widget* *stage*)
(define *nearby-widget* #f)

(set-display-procedure! (lambda()(draw *stage*)))

(set-resize-procedure! (lambda (w h)
			 (set! #[*stage* 'w] w)
			 (set! #[*stage* 'h] h)))


(define left-click-position #f)

(define (left-mouse-down x y)
  (set! *nearby-widget* #f)
  (and-let* ((w (widget-nested-find 
		 (lambda(w)
		   (in-area? (list x y)
			     (absolute-area w)))
		 *stage*)))
    (set! *active-widget* w))
  (when *active-widget*
    (#[*active-widget* 'left-mouse-down ] x y))
  (drag-over x y 0 0)
  (set! left-click-position `(,x ,y)))

(define (left-mouse-up x y)
  (when *active-widget* 
    (#[*active-widget* 'left-mouse-up] x y)
    (if (equal? left-click-position `(,x ,y))
	(#[*active-widget* 'left-click] x y)))
  (set! *active-widget* *stage*))

(define (right-mouse-down x y)
  (and-let* ((w (widget-nested-find 
		 (lambda(w)
		   (in-area? (list x y)
			     (absolute-area w)))
		 *stage*)))
    (#[ w 'right-mouse-down ] x y)))

(define (drag-over x y xrel yrel)
  (let ((mouseover-widget 
	 (widget-nested-find 
	  (lambda (w) 
	    (and (not (eq? w *active-widget*))
		 (in-area? (list x y) (absolute-area w))))
	  *stage*)))
    (when (and mouseover-widget 
	       (not (equal? mouseover-widget *nearby-widget*)))
      (if *nearby-widget* 
	  (#[ *nearby-widget* 'mouse-out ] x y xrel yrel))
      (set! *nearby-widget* mouseover-widget)
      (#[ *nearby-widget* 'mouse-over ] x y xrel yrel))
    (#[ *active-widget* 'drag ] x y xrel yrel)
    (set! left-click-position #f)))

(keydn 'mouse-left left-mouse-down)
(keyup 'mouse-left left-mouse-up)
(keydn 'mouse-right right-mouse-down)
(mousemove drag-over)
