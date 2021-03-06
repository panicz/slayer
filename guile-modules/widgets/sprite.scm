(define-module (widgets sprite)
  #:use-module (oop goops)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (extra slayer)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (slayer font)
  #:use-module (widgets base)
  #:export (<sprite>
	    <label>
	    <layout>
	    layout
	    lay-out-horizontally
	    lay-out-vertically
	    make-button button
	    make-container
	    make-sprite
	    label)
  #:re-export (add-child! draw))

(define-class <sprite> (<widget>)
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

(define-method (initialize (sprite <sprite>) args)
  (next-method)
  (let-keywords args #t ((image #f))
    (cond ((string? image)
	   (set! #[sprite 'image] (load-image image)))
	  ((image? image)
	   (set! #[sprite 'image] image)))))

(define-method (draw (i <sprite>))
  (specify ((first identity)
	    (rest #[_ 'parent])
	    (empty? (?not #[_ 'parent])))
    (let ((x (apply + (map* #[_ 'x] i)))
	  (y (apply + (map* #[_ 'y] i))))
      (draw-image! #[ i 'image ] x y))))

(define* (make-button #:key (text "button") (x 0) (y 0) (w #f) (h #f))
  (let ((normal (render-text text *default-font* #xffffff #x777777))
	(over (render-text text *default-font* #xffffff #x999999))
	(clicked (render-text text *default-font* #xffffff #xcccccc)))
    (let ((button (make <sprite> #:image normal #:x x #:y y 
			#:w (or w (image-width normal))
			#:h (or h (image-height normal)))))
      (set! #[ button 'mouse-move ] 
	    (lambda _ (unless (eq? #[button 'image] clicked)
		   (set! #[ button 'image ] over ))))
      (set! #[ button 'mouse-out ] 
	    (lambda _ (set! #[ button 'image ] normal )))
      (set! #[ button 'left-mouse-down ] 
	    (lambda _ (set! #[ button 'image ] clicked )))
      (set! #[ button 'left-mouse-up ]
	    (lambda _ (set! #[ button 'image ] over )))
      (set! #[ button 'drag ] noop)
      button)))

(define* (button #:key (text "button") (action (lambda (x y) (if #f #f))))
  (let ((button (make-button #:text text)))
    (set! #[button 'left-click] action)
    button))
  
(define* (make-container #:key x y (name "menu") (content '()))
  (let ((container (make <widget>))
	(label (make-button #:text name)))
    (set! #[label 'drag] (lambda (x y xrel yrel)		 
			   (increase! #[ container 'x ] xrel)
			   (increase! #[ container 'y ] yrel)))
    (for child in (append content `(,label))
	 (add-child! container child))
    (let ((top 0))
      (for w in #[container 'children]
	   (set! #[container 'w] (max #[w 'w] #[container 'w]))
	   (set! #[w 'y] top)
	   (set! top (+ top #[w 'h])))
      (set! #[container 'h] top))
    container))

(define (make-sprite image #;at x y)
  (let ((image (make <sprite> #:image image #:x x #:y y 
		     #:w (image-width image) 
		     #:h (image-height image))))
    (set! #[ image 'drag ]
	  (lambda (x y xrel yrel)
	    (increase! #[ image 'x ] xrel)
	    (increase! #[ image 'y ] yrel)))
    image))

(define-class <label> (<sprite>)
  (%text #:init-value #f)
  (font #:init-value *default-font* #:init-keyword #:font)
  (text-color #:init-value #xffffff #:init-keyword #:text-color)
  (background-color #:init-value #x777777 #:init-keyword #:background-color)
  (text 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     #[self '%text])
   #:slot-set!
   (lambda (self value)
     (set! #[self '%text] value)
     (set! #[self 'image] (render-text value #[self 'font] 
				       #[self 'text-color]
				       #[self 'background-color])))))

(define-method (initialize (self <label>) args)
  (next-method)
  (default-slot-values self args (text "label")))

(define-class <layout> (<widget>)
  (children #:init-value '())
  (lay-out #:init-value noop #:init-keyword #:lay-out))

(define-method (lay-out-vertically (child <widget>) #;on (layout <layout>))
  (set! #[child 'y] (+ #;[layout 'y] 1 #[layout 'h])))

(define-method (lay-out-horizontally (child <widget>) #;on (layout <layout>))
  (set! #[child 'x] (+ #;[layout 'x] 1 #[layout 'w])))

(define-method (initialize (self <layout>) args)
  (next-method)
  (default-slot-values self args (lay-out lay-out-vertically)))

(define-method (add-child! (child <widget>) #;to (layout <layout>))
  (#[layout 'lay-out] child #;on layout)
  (next-method))

(define (layout . args)
  (let ((layout (apply make <layout> args)))
    (lambda future-children
      (for child in future-children
	   (add-child! child #;to layout))
      layout)))

(define (label text)
  (make <sprite> #:image (render-text text *default-font* #xffffff #x555555)))
