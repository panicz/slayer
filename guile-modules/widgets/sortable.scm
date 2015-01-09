(define-module (widgets sortable)
  #:use-module (oop goops)
  #:use-module (widgets base)
  #:use-module (widgets sprite)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:export (
	    <sortable-container>
	    <sortable-box>
	    )
  #:re-export (add-child!))

(define-class <sortable-container> (<widget>)
  (children #:init-value '())
  (margin #:init-value 1 #:init-keyword #:margin)
  (w #:allocation #:virtual
     #:slot-ref (lambda (self)
		  (apply max 0 (map #[_ 'w] #[self 'children])))
     #:slot-set! noop)
  (h #:allocation #:virtual
     #:slot-ref (lambda (self)
		  (fold + 0 (map #[_ 'h] #[self 'children])))
     #:slot-set! noop))

(define ((target-getter property) self)
  #[self : 'target : property])

(define ((target-setter property) self value)
  (set! #[self : 'target : property] value))

(define-class <sortable-box> (<widget>)
  (original-parent #:init-value #f)
  (original-index #:init-value #f)
  (target #:init-value #f)
  (dx #:init-value 0)
  (dy #:init-value 0)

  (w #:allocation #:virtual
     #:slot-ref (target-getter 'w)
     #:slot-set! (target-setter 'w))
  (h #:allocation #:virtual
     #:slot-ref (target-getter 'h)
     #:slot-set! (target-setter 'h))

  (x #:allocation #:virtual
     #:slot-ref (lambda (self) 
		  (+ #[self 'dx]
		     (if (is-a? #[self 'parent] <sortable-container>)
			 #[self : 'parent : 'x]
			 0)))
     #:slot-set! (lambda (self value)
		   (set! #[self 'dx] ((pick 1) - value #[self 'x]))))
  (y #:allocation #:virtual
     #:slot-ref 
     (lambda (self)
       (+ #[self 'dy]
	  (if (and (is-a? #[self 'parent] <sortable-container>)
		   (in? self #[self : 'parent : 'children]))
	      (let* ((parent #[self 'parent])
		     (margin #[parent 'margin]))
		(let next (((some . siblings) #[parent 'children])
			   (vertical-position 0))
		  (if (eq? some self)
		      vertical-position
		      (next siblings (+ vertical-position #[some 'h] margin)))))
	      #;else 0)))
     #:slot-set! (lambda (self value)
		   (set! #[self 'dy] ((pick 1) - value #[self 'y])))))

(define-method (reset! (self <sortable-box>))
  (set! #[self 'original-index] #f)
  (set! #[self 'original-parent] #f)
  (set! #[self 'dx] 0)
  (set! #[self 'dy] 0))

(define-class <sortable-placeholder> (<widget>)
  (accepts-widget?
   #:init-value
   (lambda (box)
     #t))
  (x #:allocation #:virtual
     #:slot-ref (lambda (self) #[self : 'parent : 'x])
     #:slot-set! noop)
  (y #:allocation #:virtual
     #:slot-ref 
     (lambda (self)
       (let* ((parent #[self 'parent])
	      (margin #[parent 'margin]))
	 (let next (((some . siblings) #[parent 'children])
		    (vertical-position 0))
	   (if (eq? some self)
	       vertical-position
	       (next siblings (+ vertical-position #[some 'h] margin))))))
     #:slot-set! noop)) 

(define-method (draw (self <sortable-box>))
  (draw #[self 'target]))

(define ((box-left-mouse-down self) x y)
  (check (eq? self *active-widget*))
  (let* ((parent #[self 'parent])
	 (siblings #[parent 'children])
	 (n (order #;of self #;in siblings))
	 (placeholder (make <sortable-placeholder>
			#:w #[self 'w] #:h #[self 'h]
			#:parent parent #:at n)))
    (set! #[self 'dx] (+ #[self : 'parent : 'x] #[self 'x]))
    (set! #[self 'dy] (+ #[self : 'parent : 'y] #[self 'y]))
    (set! #[parent 'children]
      (alter #;element-number n #;in siblings #;with placeholder))
    (set! #[self 'original-parent] parent)
    (set! #[self 'original-index] n)
    (add-child! self #;to #[parent 'parent])))

(define ((box-left-mouse-up self) x y)
  (when (eq? self *active-widget*)
    (cond ((and (is-a? *nearby-widget* <sortable-placeholder>)
		(#[*nearby-widget* 'accepts-widget?] self))
	   (let* ((parent #[*nearby-widget* 'parent])
		  (n (order #;of *nearby-widget* 
				 #;in #[parent 'children])))
	     (reset! self)
	     (set! #[self 'parent] parent)
	     (set! #[parent 'children]
	       (alter #;element-number n #;in #[parent 'children] #;with self))
	     (set! #[*nearby-widget* 'parent] #f)))
	  (else
	   ;; putting back the piece
	   (let* ((grandpa #[self 'parent])
		  (parent #[self 'original-parent])
		  (siblings (remove (lambda (child)
				      (is-a? child <sortable-placeholder>))
				    #[parent 'children]))
		  (younger older (split-at siblings #[self 'original-index])))
	     (set! #[grandpa 'children] (delete self #[grandpa 'children]))
	     (set! #[self 'parent] parent)
	     (set! #[parent 'children] `(,@younger ,self ,@older))
	     (reset! self))))))

(define ((box-drag-over self) x y dx dy)
  (check (eq? self *nearby-widget*))
  (when (is-a? *active-widget* <sortable-box>)
    (and-let* ((parent #[*nearby-widget* 'parent])
	       (placeholder (make <sortable-placeholder>
			      #:w #[*active-widget* 'w]
			      #:h #[*active-widget* 'h]
			      #:parent parent)))
      (let* ((n (order #;of *nearby-widget* #;in #[parent 'children]))
	     (skim (remove (lambda (child) (is-a? child <sortable-placeholder>))
			   #;from #[parent 'children]))
	     (earlier later (split-at skim n)))
	(set! #[parent 'children] `(,@earlier ,placeholder ,@later))))))

(define-method (initialize (self <sortable-box>) args)
  (next-method)
  (let-keywords args #t ((target #f))
    (unless target 
      (error "#:target <widget> must be provided on <sortable-box>\
 initialization"))
    (set! #[target 'parent] self)
    (set! #[self 'target] target)

    (set! #[self 'left-mouse-down] (box-left-mouse-down self))

    (set! #[self 'left-mouse-up] (box-left-mouse-up self))

    (set! #[self 'drag-over] (box-drag-over self))

    (set! #[self 'drag] (lambda (x y xrel yrel)
			  (increase! #[ self 'x] xrel)
			  (increase! #[ self 'y] yrel)))
    ))

(define-method (add-child! (child <widget>) #;to  (parent <sortable-container>))
  (let ((placeholder (if (or (is-a? child <sortable-box>)
			     (is-a? child <sortable-placeholder>))
			 child
			 (make <sortable-box> #:target child))))
    (set! #[parent 'children] (cons placeholder #[parent 'children]))
    (set! #[placeholder 'parent] parent)))
