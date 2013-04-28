(use-modules (slayer)
	     (slayer 3d)
	     (widgets base)
	     (widgets ode-view)
	     (oop goops)
	     (extra ref)
	     (extra common)
	     (extra 3d)
	     (extra math)
	     (libs physics))

(keydn 'esc quit)

(define *stage* (make <widget> #:w (screen-width) #:h (screen-height)))

(define *input-widget* #f)
(define *active-widget* *stage*)
(define *nearby-widget* #f)

(set-display-procedure! (\ draw *stage*))
(set-resize-procedure! (lambda (w h)
			 (set! #[*stage* 'w] w)
			 (set! #[*stage* 'h] h)))

(define *sim* (primitive-make-simulation))

(add-child! *stage* (make <ode-view> #:x 10 #:y 10 
			  #:w (- (screen-width) 10)
			  #:h (- (screen-height) 10)
			  #:simulation *sim*))

