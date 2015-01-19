(define-module (widgets sortable)
  #:use-module (oop goops)
  #:use-module (slayer image)
  #:use-module (widgets base)
  #:use-module (widgets sprite)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:export (
	    <sortable-container>
	    <widget-distributor>
	    <sortable-box>
	    add/overwrite!
	    )
  #:re-export (add-child!))

(define-class <sortable-container> (<widget>)
  (instances #:allocation #:class #:init-value '())
  (accepts-widget? #:init-value noop)
  (blocked? #:init-value #f #:init-keyword #:blocked?)
  (children #:init-value '())
  (margin #:init-value 1 #:init-keyword #:margin)
  (min-w #:init-value 0 #:init-keyword #:min-w)
  (min-h #:init-value 0 #:init-keyword #:min-h)
  (w #:allocation #:virtual
     #:slot-ref (lambda (self)
		  (max #[self 'min-w]
		       (apply max 0 (map #[_ 'w] #[self 'children]))))
     #:slot-set! noop)
  (h #:allocation #:virtual
     #:slot-ref (lambda (self)
		  (max #[self 'min-h]
		       (fold + 0 (map #[_ 'h] #[self 'children]))))
     #:slot-set! noop))

(define ((sortable-container-drag-over self) x y dx dy)
  (when (and (not #[self 'blocked?])
	     (is-a? *active-widget* <sortable-box>)
	     (#[self 'accepts-widget?] *active-widget* #;in #f)
	     (not (exists x in #[self 'children]
		    (is-a? x <sortable-placeholder>))))
    (let ((placeholder (make <sortable-placeholder>
			 #:w #[*active-widget* 'w]
			 #:h #[*active-widget* 'h])))
      (set! #[self 'children] `(,placeholder ,@#[self 'children]))
      (set! #[placeholder 'parent] self))))

(define-method (initialize (self <sortable-container>) args)
  (next-method)
  (set! #[self 'instances] `(,self ,@#[self 'instances]))
  (set! #[self 'drag-over] (sortable-container-drag-over self)))

(define-class <widget-distributor> (<sortable-container>))

(define-method (initialize (self <widget-distributor>) args)
  (next-method)
  (set! #[self 'accepts-widget?] (lambda (box #;in placeholder) #f))
  (set! #[self 'drag-over] noop))

(define ((target-getter property) self)
  #[self : 'target : property])

(define ((target-setter property) self value)
  (set! #[self : 'target : property] value))

(define-class <sortable-box> (<widget>)
  (original-parent #:init-value #f)
  (original-index #:init-value #f)
  (target #:init-value #f)
  (dx #:init-value 0 #:init-keyword #:dx)
  (dy #:init-value 0 #:init-keyword #:dy)
  
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
     #:slot-set! noop)
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
		      (next siblings (+ vertical-position #[some 'h] 
					margin)))))
	      #;else
	      0)))
     #:slot-set! noop))

(define-method (reset! (self <sortable-box>))
  (set! #[self 'original-index] #f)
  (set! #[self 'original-parent] #f)
  (set! #[self 'dx] 0)
  (set! #[self 'dy] 0))

(define-class <sortable-placeholder> (<sprite>)
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

(define-method (initialize (self <sortable-placeholder>) args)
  (next-method)
  (set! #[self 'image] (rectangle #[self 'w] #[self 'h]
				  #xff4422)))

(define-method (draw (self <sortable-box>))
  (draw #[self 'target]))

(define-generic take-box-from-container)

(define-method (take-box-from-container (box <sortable-box>)
					(container <sortable-container>))
  (assert (eq? container #[box 'parent]))
  (let* ((siblings (filter (lambda (x) (is-a? x <sortable-box>))
			   #[container 'children]))
	 (n (order #;of box #;in siblings))
	 (placeholder (make <sortable-placeholder>
			#:w #[box 'w] #:h #[box 'h]
			#:parent container #:at n)))
    (set! #[box 'dx] (+ #[box 'x] #[container 'x]))
    (set! #[box 'dy] (+ #[box 'y] #[container 'y]))
    (set! #[container 'children]
      (alter #;element-number n #;in siblings #;with placeholder))
    (set! #[box 'original-parent] container)
    (set! #[box 'original-index] n)
    (add-child! box #;to #[container 'parent])))

(define-method (take-box-from-container (box <sortable-box>)
					(container <widget-distributor>))
  (assert (eq? container #[box 'parent]))
  (let ((clone (make <sortable-box> 
		 #:target (shallow-clone #[box 'target])
		 #:dx (+ #[box 'x] #[container 'x])
		 #:dy (+ #[box 'y] #[container 'y]))))
    (set! #[clone 'original-parent] container)
    (add-child! clone #;to #[container 'parent])
    (set! *active-widget* clone)))

(define ((box-left-mouse-down self) x y)
  (check (eq? self *active-widget*))
  (and-let* ((parent #[self 'parent])
	     ((is-a? parent <sortable-container>))
	     ((not #[parent 'blocked?])))
    (take-box-from-container self parent))
  (#[self : 'target : 'activate] x y))

(define ((box-left-mouse-up self) x y)
  (assert (eq? self *active-widget*))
  (cond 
   ((and-let* (((is-a? *nearby-widget* <sortable-placeholder>))
	       (parent #[*nearby-widget* 'parent])
	       ((is-a? parent <sortable-container>))
	       ((#[parent 'accepts-widget?] self #;in *nearby-widget*)))
      parent)
    => (lambda (parent)
	 (let ((n (order #;of *nearby-widget* 
			      #;in #[parent 'children])))
	   (set! #[self 'parent] parent)
	   (set! #[parent 'children]
	     (alter #;element-number n #;in #[parent 'children]
				     #;with self))
	   (set! #[*nearby-widget* 'parent] #f))))
   ((and-let* ((original-parent #[self 'original-parent])
	       ((#[original-parent 'accepts-widget?] self #;in #f)))
      original-parent)
    ;; putting back the piece
    => (lambda (original-parent)
	 (let* ((grandpa #[self 'parent])
		(siblings (remove (lambda (child)
				    (is-a? child <sortable-placeholder>))
				  #[original-parent 'children]))
		(younger older (split-at siblings 
					 (min (length siblings)
					      #[self 'original-index]))))
	   (set! #[grandpa 'children] (delete self #[grandpa 'children]))
	   (set! #[self 'parent] original-parent)
	   (set! #[original-parent 'children] `(,@younger ,self ,@older)))))
   ((not (is-a? #[self 'parent] <sortable-container>))
    ;; remove from the stage
    (let ((parent #[self 'parent]))
      (set! #[parent 'children] (delete self #[parent 'children])))))
  
  (and-let* ((container #[self 'original-parent]))
    (for container in #[container 'instances]
      (set! #[container 'children]
	(filter (lambda (x) (is-a? x <sortable-box>))
		#[container 'children]))))
  (reset! self))

(define ((box-drag-over self) x y dx dy)
  (check (eq? self *nearby-widget*))
  (and-let* (((is-a? *active-widget* <sortable-box>))
	     (parent #[self 'parent])
	     ((is-a? parent <sortable-container>))
	     ((#[parent 'accepts-widget?] *active-widget* #;in self))
	     (placeholder (make <sortable-placeholder>
			    #:w #[self 'w]
			    #:h #[self 'h]
			    #:parent parent))
	     (n (order #;of self #;in #[parent 'children]))
	     (skim (remove (lambda (child) (is-a? child <sortable-placeholder>))
			   #;from #[parent 'children]))
	     (earlier later (split-at skim (min (length skim) n))))
    (set! #[parent 'children] `(,@earlier ,placeholder ,@later))))

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

    (set! #[self 'drag] 
      (lambda (x y xrel yrel)
	(and-let* ((parent (or #[self 'original-parent] #[self 'parent]))
		   ((is-a? parent <sortable-container>))
		   ((not #[parent 'blocked?])))
	  (increase! #[ self 'dx ] xrel)
	  (increase! #[ self 'dy ] yrel))))
    ))

(define-method (add-child! (child <widget>) #;to  (parent <sortable-container>))
  (let ((placeholder (if (or (is-a? child <sortable-box>)
			     (is-a? child <sortable-placeholder>))
			 child
			 (make <sortable-box> #:target child))))
    (set! #[parent 'children] (cons placeholder #[parent 'children]))
    (set! #[placeholder 'parent] parent)))

(define-method (add/overwrite! (element <widget>) 
			       #;to (container <sortable-container>) 
				    #;overwriting-if condition?)
  (set! #[container 'children] (filter (lambda (x) (is-a? x <sortable-box>))
				       #[container 'children]))
  (let ((existing (find (lambda (box) (condition? #[box 'target]))
			#[container 'children])))
    (if existing
	(begin
	  (set! #[existing 'target] element)
	  (set! #[element 'parent] existing))
	(add-child! element #;to container))))
