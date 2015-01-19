(define-module (widgets tab)
  #:use-module (slayer image)
  #:use-module (slayer font)
  #:use-module (oop goops)
  #:use-module (widgets base)
  #:use-module (widgets sprite)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:export (
	    <tab-widget>
	    add-tab!
	    ))

(define-class <tab-button> (<sprite>)
  (target #:init-keyword #:target)
  (pressed? #:init-value #t)
  (label #:init-keyword #:label)
  (%image/pressed #:init-value #f)
  (image 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (if #[self 'pressed?]
	 #[self '%image/pressed]
	 #[self '%image]))
   #:slot-set! noop))

(define-method (initialize (self <tab-button>) args)
  (next-method)
  (let ((label (string-append " " #[self 'label] " ")))
    (set! #[self '%image] (render-text label *default-font* #x000000 #xffffff))
    (set! #[self '%image/pressed]
      (render-text label *default-font* #xdd0000 #xffffff)))
  (let* (((w h) (image-size #[self '%image])))
    (set! #[self 'w] w)
    (set! #[self 'h] h))
  (set! #[self 'left-click]
    (lambda _
      (unless #[self 'pressed?]
	(let ((parent #[self 'parent]))
	  (for button in #[parent 'tab-buttons]
	    (set! #[button 'pressed?] #f))
	  (set! #[self 'pressed?] #t)
	  (assert (in? #[self 'target] #[parent 'tab-views]))
	  (set! #[parent 'current-view] #[self 'target]))))))

(define-class <tab-widget> (<widget>)
  (w #:allocation #:virtual
     #:slot-ref (lambda (self)		  
		  (max (apply + (map #[_ 'w] #[self 'tab-buttons]))
		       (or (and-let* ((view #[self 'current-view]))
			     (+ #[view 'x] #[view 'w]))
			   0)))
     #:slot-set! noop)

  (h #:allocation #:virtual
     #:slot-ref (lambda (self)		  
		  (+ (apply max 0 (map #[_ 'h] #[self 'tab-buttons]))
		     (or (and-let* ((view #[self 'current-view]))
			   (+ #[view 'y] #[view 'h]))
			 0)))
     #:slot-set! noop)

  (tab-buttons #:init-value '())
  (tab-views #:init-value '())
  (current-view #:init-value #f)
  (children 
   #:allocation #:virtual
   #:slot-ref (lambda (self) `(,#[self 'current-view] ,@#[self 'tab-buttons]))
   #:slot-set! noop))

(define-generic add-tab!)

(define-method (add-tab! (child <widget>) #;under (name <string>)
			 #;to (manager <tab-widget>))
  (let ((buttons-before #[manager 'tab-buttons]))
    (for button in buttons-before
      (set! #[button 'pressed?] #f))
    (set! #[manager 'tab-buttons]
      `(,(make <tab-button>
	   #:label name
	   #:parent manager
	   #:target child
	   #:x (fold + (length buttons-before) 
		     (map #[_ 'w] buttons-before))) 
	,@buttons-before)))
  (set! #[manager 'tab-views] `(,child ,@#[manager 'tab-views]))
  (set! #[child 'y] (+ #[child 'y] 1
		       (apply max (map #[_ 'h] #[manager 'tab-buttons]))))
  (set! #[child 'parent] manager)
  (set! #[manager 'current-view] child))
