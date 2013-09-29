(define-module (widgets bitmap)
  #:use-module (oop goops)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (slayer font)
  #:use-module (widgets base)
  #:export (<bitmap>
	    make-button
	    make-container
	    make-image))

(define-class <bitmap> (<widget>)
  (%image #:init-value #f)
  (image #:allocation #:virtual
	 #:slot-ref (lambda(self)
		      #[self '%image])
	 #:slot-set! (lambda (self image)
		       (set! #[self '%image] image)
		       (match-let (((w h) (image-size image)))
			 (set! #[self 'w] w)
			 (set! #[self 'h] h)))
	 #:init-keyword #:image))

#;(define-method (initialize (bitmap <bitmap>) init-args)
  (next-method)
  (let-keywords init-args #t ((image #f))
    (if image
	(set! #[bitmap 'image] image))))

(define-method (draw (i <bitmap>))
  (draw-image! #[ i 'image ]
	       (+ (or #[i : 'parent : 'x] 0) #[ i 'x ]) 
	       (+ (or #[i : 'parent : 'y] 0) #[ i 'y ])))

(define* (make-button #:key (text "button") (x 0) (y 0) (w #f) (h #f))
  (let ((normal (render-text text *default-font* #xffffff #xff0000))
	(over (render-text text *default-font* #xffffff #x00ff00))
	(clicked (render-text text *default-font* #xffffff #x0000ff)))
    (let ((button (make <bitmap> #:image normal #:x x #:y y 
			#:w (or w (image-width normal))
			#:h (or h (image-height normal)))))
      (set! #[ button 'mouse-over ] 
	    (lambda e (set! #[ button 'image ] over )))
      (set! #[ button 'mouse-out ] 
	    (lambda e (set! #[ button 'image ] normal )))
      (set! #[ button 'left-mouse-down ] 
	    (lambda e (set! #[ button 'image ] clicked )))
      (set! #[ button 'right-mouse-down ] #[ button 'mouse-over ])
      (set! #[ button 'drag ] noop)
      button)))

(define* (make-container #:key x y (name "menu") (content '()))
  (let ((container (make <widget>))
	(label (make-button #:text name)))
    (set! #[label 'drag] (lambda (x y xrel yrel)		 
			   (increase! #[ container 'x ] xrel)
			   (increase! #[ container 'y ] yrel)))
    (for child in (append content `(,label))
	 (add-child! container child))
    ;(add-child! container (make-button #:x 0 #:y 0 #:text name))
    (let ((top 0))
      (for w in #[container 'children]
	   (set! #[container 'w] (max #[w 'w] #[container 'w]))
	   (set! #[w 'y] top)
	   (set! top (+ top #[w 'h])))
      (set! #[container 'h] top))
    container))

(define (make-image image x y)
  (let ((image (make <bitmap> #:image image #:x x #:y y 
		     #:w (image-width image) 
		     #:h (image-height image))))
    (set! #[ image 'drag ]
	  (lambda (x y xrel yrel)		 
	    (increase! #[ image 'x ] xrel)
	    (increase! #[ image 'y ] yrel)))
    #;(set! #[ image 'mouse-over ]
	  (lambda (x y xrel yrel)
	    (format #t "now mouse is over ~s\n" image)))
    #;(set! #[ image 'mouse-out ]
	  (lambda (x y xrel yrel)
	    (format #t "mouse is no longer over ~s\n" image)))
    image))
