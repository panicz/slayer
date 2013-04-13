(use-modules (slayer input)
	     (slayer video)
	     (slayer image)
	     (slayer)
	     (widgets widgets)
	     (oop goops)
	     (extra ref)
	     (extra common))

(define *stage* (make <widget> #:w (screen-width) #:h (screen-height)))

(define *input-widget* #f)
(define *active-widget* *stage*)
(define *nearby-widget* #f)

(keydn 'esc quit)

(set-display-procedure! (\ draw *stage*))
(set-resize-procedure! (lambda (w h)
			 (set! #[*stage* 'w] w)
			 (set! #[*stage* 'h] h)))

(define ku (load-image "./art/ku.png"))

(add-child! *stage* (make-image ku 75 25))

(add-child! *stage* (make-text-area))

(keydn 'mouse1 
       (lambda (x y)
	 (and-let* ((w (widget-nested-find 
			(lambda(w)
			  (in-area? (list x y)
				    (absolute-area w)))
			*stage*)))
	   (set! *active-widget* w))
	 (if *active-widget* (#[ *active-widget* 'click ] x y))))

(keyup 'mouse1 
	(lambda (x y) 
	  (if *active-widget* (#[ *active-widget* 'unclick ] x y))
	  (set! *active-widget* *stage*)))

(keydn 'mouse2 
       (lambda (x y)
	 (and-let* ((w (widget-nested-find 
			(lambda(w)
			  (in-area? (list x y)
				    (absolute-area w)))
			*stage*)))
	   (#[ w 'right-click ] x y))))

(mousemove 
 (lambda (x y xrel yrel)
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
     (#[ *active-widget* 'drag ] x y xrel yrel))))
