(define-module (schess widgets)
  ;; a set of widgets+operations for displaying a graphical interface
  ;; for board games.
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (widgets base)
  #:use-module (widgets sprite)
  #:use-module (extra slayer)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra time)
  #:use-module (oop goops)
  #:use-module (schess elements)
  #:export (
	    <board> 
	    load-images
	    reset!
	    setup-fields!
	    synchronize-fields!
	    board-state
	    <field> 
	    allow!
	    <checker>
	    move!
	    )
  #:re-export (make)
  )

(define (init-value class slot)
  (keyword-ref (class-slot-definition class slot) #:init-value))

(define-class <checker> (<sprite>)
  (origin #:init-value #f) ;; if non-false, contains a value
  (type #:init-value #f #:init-keyword #:type)
  (on-pick #:init-value 
	   (lambda(checker field board)
	     (format #t "~a has been picked from ~a\n" #[checker 'type]
		     #[field 'position]))
	   #:init-keyword #:on-pick)
  (on-drop #:init-value
	   (lambda(checker field board)
	     (move! checker #;to #[checker 'origin] #;on board)
	     (format #t "~a has been dropped above ~a\n" 
		     #[checker 'type] #[field 'position]))
	   #:init-keyword #:on-drop))

(define-class <field> (<sprite>)
  (%content #:init-value #f)
  (content #:allocation #:virtual
	   #:slot-ref (lambda (self)
			#[self '%content])
	   #:slot-set! (lambda (self object)
			 (set! #[self '%content] object)
			 (set! #[object 'parent] self)))
  (position #:init-keyword #:position #;(x y))
  (allowed #:init-value #f)
  (chosen-move #:init-value #f)
  (last-move #:init-value #f)
  (children #:allocation #:virtual
	    #:slot-ref (lambda (self)
			 (if #[self 'content] 
			     (list #[self 'content]) 
			     '()))
	    #:slot-set! (lambda (self value)
			  (when (list? value)
			    (set! #[self '%content] #f)
			    (for x in value
				 (set! #[self 'content] x))))))

(define-class <board> (<widget>)
  (fields #:init-value #())
  (on-pick-checker #:init-thunk (lambda()(init-value <checker> 'on-pick))
		   #:init-keyword #:on-pick-checker)
  (on-drop-checker #:init-thunk (lambda()(init-value <checker> 'on-drop))
		   #:init-keyword #:on-drop-checker)
  (above-fields #:init-value '())
  (field-decorator #:init-value (let ((field (rectangle 32 32 #xadff2f)))
				  (lambda (x y) field))
		   #:init-keyword #:field-decorator)
  (image-transformer #:init-value identity
		     #:init-keyword #:image-transformer)
  ;; (images #: <symbol> -> <image>)
  (images #:init-keyword #:images #:init-thunk make-hash-table)
  (children #:allocation #:virtual
	    #:slot-ref (lambda (self)
			 (append #[self 'above-fields]
				 (concatenate (array->list #[self 'fields]))))
	    #:slot-set! noop))

(define-method (board-state (board <board>))
  (array->list (array-map (lambda (field)
			    (or (and-let* ((checker #[field 'content]))
				  #[checker 'type])
				'_))
			  #[board 'fields])))

(define ((pick! checker) . _)
  (and-let* ((field #[checker 'parent])
	     ((is-a? field <field>))
	     (figure #[ checker 'type ])
	     (board #[field 'parent])
	     ((is-a? board <board>))
	     (position #[field 'position]))
    (#[checker 'on-pick] checker #;from field #;on board)
    (take! checker #;from field #;on board)))

(define ((put-to-nearby-field! checker) . _)
  (and-let* ((field (cond ((is-a? *nearby-widget* <field>)
			   *nearby-widget*)
			  ((and-let* (((is-a? *nearby-widget* <checker>))
				      (parent #[*nearby-widget* 'parent]) 
				      ((is-a? parent <field>)))
			     parent))
			  (else
			   #f)))
	     (board #[checker 'parent])
	     ((is-a? board <board>)))
    (#[checker 'on-drop] checker #;at field #;on board)))

(define-method (initialize (self <checker>) init-args)
  (next-method)
  (set! #[self 'left-mouse-down] 
	(pick! self))
  (set! #[self 'left-mouse-up] 
	(put-to-nearby-field! self))
  (set! #[self 'drag]
	(lambda (x y dx dy)
	  (increase! #[self 'x] dx)
	  (increase! #[self 'y] dy))))

(define-method (draw (field <field>))
  (next-method)
  (if #[field 'content]
      (draw #[field 'content])))

(define* (load-images image-names #:optional (transformer identity))
  (let ((images #[]))
    (for (type name) in image-names
	 (set! #[images type] (transformer (load-image name))))
    images))

(define-method (synchronize-fields! (board <board>) #;with rect)
  (match-let* ((fields #[board 'fields])
	       ((h w) (array-dimensions fields)))
    (for x in 0 .. (- w 1)
	 (for y in 0 .. (- h 1)
	      (let* ((checker-type (take-from-rect rect x y))
		     (field #[fields y x])
		     (image #[board : 'images : checker-type]))
		(if (not image)                   ; we treat types that have 
		    (set! #[field 'children] '()) ; no images as empty
	        #;else
		    (if (or (null? #[field 'children])
			    (not (eq? checker-type 
				      #[(first #[field 'children]) 
					'type])))
			(let ((checker (make <checker>
					 #:on-pick #[board 'on-pick-checker]
					 #:on-drop #[board 'on-drop-checker]
					 #:image image #:type checker-type)))
			  (set! #[field 'children] '())
			  (add-child! checker #;to field)))))))))

(define-method (setup-fields! (board <board>) initial-state)
  (match-let (((w h) (rect-size initial-state))
	      (empty-field (#[board 'field-decorator] 0 0)))
    (set! #[board 'w] (* w (image-width empty-field)))
    (set! #[board 'h] (* h (image-height empty-field)))
    (set! #[board 'fields] (make-array #f h w))
    (for x in 0 .. (- w 1)
	 (for y in 0 .. (- h 1)
	      (match-let* ((field (make <field>
				    #:position `(,x ,y)
				    #:image (#[board 'field-decorator] x y)))
			   (checker (take-from-rect initial-state x y))
			   ((w h) (image-size #[field 'image])))
		(set! #[field 'parent] board)
		(set! #[field 'x] (* x w))
		(set! #[field 'y] (* y h))
		(set! #[#[board 'fields] y x] field))))
    (synchronize-fields! board initial-state)))

(define-method (initialize (self <board>) init-args)
  (next-method)
  (let-keywords init-args #t ((initial-state '((_)))
			      (image-names '()))
    (set! #[self 'images] (load-images image-names #[self 'image-transformer]))
    (setup-fields! self initial-state)
    ))

(define-method (take! (checker <checker>) #;from (field <field>) 
		      #;on (board <board>))
  (set! #[checker 'x] #[field 'x])
  (set! #[checker 'y] #[field 'y])
  (set! #[field 'content] #f)
  (set! #[checker 'parent] board)
  (set! #[checker 'origin] field)
  (push! #[board 'above-fields] checker))

(define-method (move! (checker <checker>) #;to (field <field>) 
		      #;on (board <board>))
  (set! #[board 'above-fields] (delete checker #[board 'above-fields]))
  (set! #[field 'content] checker)
  (set! #[checker 'x] 0)
  (set! #[checker 'y] 0)
  (set! #[checker 'parent] field)
  (set! #[checker 'origin] #f)
  #;(reset! board))

(define-method (allow! (field <field>) move)
  (set! #[field 'allowed] #t)
  (set! #[field 'chosen-move] move)
  (set! #[field 'image] (highlighted #[field 'image] #:green +50)))

(define-method (reset! (board <board>))
  (match-let (((h w) (array-dimensions #[board 'fields])))
    (for x in 0 .. (- w 1)
	 (for y in 0 .. (- h 1)
	      (set! #[ #[ #[board 'fields] y x ] 'allowed] #f)
	      (set! #[ #[ #[board 'fields] y x ] 'chosen-move] #f)
	      (set! #[ #[ #[board 'fields] y x ] 'image ]
		    (#[board 'field-decorator] x y))))))
