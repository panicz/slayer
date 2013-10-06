(define-module (schess interface)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (widgets base)
  #:use-module (widgets bitmap)
  #:use-module (schess elements)
  #:use-module (extra slayer)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra threads)
  #:use-module (oop goops)
  #:export (load-board-game
	    gameplay
	    current-board-state/array
	    <board> <field> <checker>
	    ))

(define (gameplay board)
  (let turn ((n 0) (player #[board 'current-player]))
    (let ((checker (choose-checker #;from board)))
      (move! checker #;to (choose-destination #;on board) #;on board)
      (if (final? board)
	  (wins! player #;in-round n)
       #;else
	  (turn (1+ n) (next-player! #;on board))))))

(define (choose-checker #;from board)
  ;; czekamy, aż na planszy pojawi się jakiś wybór
  (get! #;from #[board 'selected-checker]))

(define (choose-destination #;on board)
  (get! #;from #[board 'chosen-destination]))

(define (select-destination! #;of checker #;as field #;on board)
  (give! field #;via #[board 'chosen-destination])
  (give! checker #;via #[board 'selected-checker]))

(define ((put-to-nearby-widget! checker) . _)
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
    (cond (#[field 'allowed]
	   (select-destination! #;of checker #;as field #;on board))
	  (else #;(eq? *nearby-widget* #[checker 'origin])
	   (move! checker #;to #[checker 'origin] #;on board)))))

(define-method (move! checker #;to field #;on board)
  (set! #[board 'above-fields] (delete checker #[board 'above-fields]))
  (set! #[field 'content] checker)
  (set! #[checker 'x] 0)
  (set! #[checker 'y] 0)
  (set! #[checker 'parent] field)
  (set! #[checker 'origin] #f)
  (reset! board))

(define-method (next-player! #;on board)
  (set! #[board 'order-of-play] (rest #[board 'order-of-play]))
  #[board 'current-player])

(define empty-field (load-image "art/chess/b.png"))

(define empty-field* (load-image "art/chess/w.png"))

(define-class <field> (<bitmap>)
  (%content #:init-value #f)
  (content #:allocation #:virtual
	   #:slot-ref (lambda (self)
			#[self '%content])
	   #:slot-set! (lambda (self object)
			 (set! #[self '%content] object)
			 (set! #[object 'parent] self)))
  (position #:init-keyword #:position #;(x y))
  (allowed #:init-value #f)
  (children #:allocation #:virtual
	    #:slot-ref (lambda (self)
			 (if #[self 'content] 
			     (list #[self 'content]) 
			     '()))
	    #:slot-set! (lambda (self value)
			  (if (list? value)
			      (for x in value
				   (set! #[self 'content] x))))))

(define-method (draw (field <field>))
  (next-method)
  (if #[field 'content]
      (draw #[field 'content])))

(define-class <checker> (<bitmap>)
  (origin #:init-value #f)
  (type #:init-value #f #:init-keyword #:type))

(define ((pick! checker) . _)
  (and-let* ((field #[checker 'parent])
	     ((is-a? field <field>))
	     (board #[field 'parent])
	     ((is-a? board <board>)))
    ;; tutaj jakoś chcielibyśmy może sprawdzić, które ruchy
    ;; są dozwolone, i odpowiednio je oznaczyć
    (for (x y) in (possible-destinations #[field 'position] board)
	 (allow! #[ #[board 'fields] y x ]))
    ;;(take! checker #;from field #;to board)
    (set! #[checker 'x] #[field 'x])
    (set! #[checker 'y] #[field 'y])
    (set! #[field 'content] #f)
    (set! #[checker 'parent] board)
    (set! #[checker 'origin] field)
    (push! #[board 'above-fields] checker)))


(define-method (initialize (checker <checker>) init-args)
  (next-method)
  (set! #[checker 'left-mouse-down] ;; powinno być start-dragging
	(pick! checker))
  (set! #[checker 'left-mouse-up] ;; powinno być stop-dragging
	(put-to-nearby-widget! checker))

  (set! #[checker 'drag]
	(lambda (x y dx dy)
	  (increase! #[checker 'x] dx)
	  (increase! #[checker 'y] dy))))


(define-class <board> (<widget>)
  (fields #:init-value #f)
  (above-fields #:init-value '())
  (allowed-moves #:init-keyword #:allowed-moves)
  (order-of-play #:init-keyword #:order-of-play)
  (selected-checker #:init-thunk make-queue)
  (chosen-destination #:init-thunk make-queue)

  (current-player #:allocation #:virtual
		  #:slot-ref (lambda (self)
			       (first #[self 'order-of-play]))
		  #:slot-set! noop)
  (wildcards #:init-keyword #:wildcards)
  (children #:allocation #:virtual
	    #:slot-ref (lambda (self)
			 (append #[self 'above-fields] 
				 (flatten (array->list #[self 'fields]))))
	    #:slot-set! noop))

(define-method (allow! (field <field>))
  (set! #[field 'allowed] #t)
  (set! #[field 'image] (highlighted #[field 'image] #:green +50)))

(define-method (reset! (board <board>))
  (match-let (((h w) (array-dimensions #[board 'fields])))
    (for x in 0 .. (- w 1)
	 (for y in 0 .. (- h 1)
	      (set! #[ #[ #[ board 'fields ] y x ] 'allowed] #f)
	      (set! #[ #[ #[board 'fields] y x ] 'image ]
		    (if (= 0 (modulo (+ x y) 2))
			empty-field
			empty-field*))))))

(define-method (initialize (board <board>) init-args)
  (next-method)
  (let-keywords init-args #t ((initial-state '((_)))
			      (images #[]))
    (match-let (((w h) (rect-size initial-state)))
      (set-screen-size! (* w (image-width empty-field)) 
			(* h (image-height empty-field)))
      (set! #[board 'w] (screen-width))
      (set! #[board 'h] (screen-height))
      (set! #[board 'fields] (make-array 2 h w))
      (for x in 0 .. (- w 1)
	   (for y in 0 .. (- h 1)
		(match-let* ((field (make <field> #:position `(,x ,y)
					  #:image (if (= 0 (modulo (+ x y) 2))
						      empty-field
						      empty-field*)))
			     (checker (take-from-rect initial-state x y))
			     ((w h) (image-size #[field 'image])))
		  (set! #[field 'parent] board)
		  (set! #[field 'x] (* x w))
		  (set! #[field 'y] (* y h))
		  (set! #[#[board 'fields] y x] field)
		  (and-let* ((image #[images checker])
			     (checker (make <checker> #:image image 
					    #:type checker)))
		    (add-child! field checker))
		  ))))))

(define-method (current-board-state/array (board <board>))
  (array-map (lambda(field)
	       (or #[field : 'content : 'type]
		   '_))
	     #[board 'fields]))

(define-method (current-board-state/rect (board <board>))
  (array->list (current-board-state/array board)))

(define-generic possible-destinations)

(define-method (possible-destinations field-position (board <board>))
  (and-let* ((allowed-moves #[board : 'allowed-moves : 
				    #[board : 'current-player]])
	     (wildcards #[board 'wildcards])
	     (board-rect (current-board-state/rect board)))
    (specify ((fit? (lambda (field pattern)
		      (or (eq? pattern '?)
			  (eq? field pattern)
			  (and-let* ((possible-values #[wildcards pattern]))
			    (in? field possible-values))))))
	     (or (possible-destinations #;on board-rect #;from field-position 
					#;using allowed-moves)
		 '()))))

(publish 
 (define (load-board-game game-description)
   (let ((description (with-input-from-file game-description read)))
     (match description
       (('define-game-rules name . description)
	(set-window-title! (string-append "SCHESS: " (->string name)))
	(let ((initial-board (first #[description 'initial-board:]))
	      (wildcards #[description 'wildcards:])
	      (moves #[description 'moves:])
	      (images (map (match-lambda 
			       ((figure name)
				`(,figure . ,(subtract-image (load-image name)
							     empty-field))))
			   #[description 'images:])))
	  (let ((wildcards (expand-wildcards wildcards))
		(width (rect-width initial-board))
		(height (rect-height initial-board)))
	    (specify ((wildcards wildcards))
		     (let ((allowed-moves (extract-moves moves width height)))
		       (make <board> #:initial-state initial-board
			     #:images images
			     #:allowed-moves allowed-moves
			     #:order-of-play (apply 
					      circular-list
					      #[description 'order-of-play:])
			     #:wildcards wildcards
			     )))))))))
 where
 (define (expand-wildcards wildcards)
   (define (value term)
     (match (find (match-lambda ((name value)
				 (equal? name term))) 
		  wildcards)
       ((name (values ...))
	(append-map value values))
       ((name atomic-value)
	(value atomic-value))
       (else
	(list term))))
   (map (match-lambda ((name definition) 
		       `(,name ,@(value name))))
	wildcards))

 (define (extract-moves movement-description board-width board-height)
   (let ((player-moves (make-hash-table)))
     (for (player . description) in movement-description
	  (match description
	    ((('transform player-name . details))
	     (let ((moves #[player-moves player-name])
		   (opposite (let* ((opposites (or #[details 'opposites:] '()))
				    (reciprocal `(,@(map reverse opposites)
						  ,@opposites)))
			       (lambda(figure)
				 (first (or #[reciprocal figure]
					    (list figure))))))
		   (transformations (map (lambda(name)
					   #[board-transformations name])
					 (or #[details 'transformations:] '())))
		   (new-moves #[]))
	       (for (figure => move-description) in moves
		    (set! #[new-moves (opposite figure)]
			  (map (match-lambda 
				   ((initial-state final-state)
				    (map (lambda(state-description)
					   ((apply compose transformations) 
					    (tree-map opposite
						      state-description)))
					 (list initial-state final-state))))
			       move-description)))
	       (set! #[player-moves player] new-moves)))
	    (((figures . moves) ...)
	     (let ((figure-moves (make-hash-table)))
	       (set! #[player-moves player] figure-moves)
	       (for (figure moves) in (zip figures moves)
		    (set! #[figure-moves figure]
			  (expand-moves #;for figure #;from moves
					   #;at board-width board-height)))))))
     #;return player-moves))

 (define (expand-moves #;for figure #;from definitions 
			     #;at board-width #;and board-height)
   (append-map
    (match-lambda 
	((initial-state final-state . extra)
	 (let ((symmetries (or #[extra 'symmetries:]
			       '(identity)))
	       (conditions (or #[extra 'conditions:]
			       'none))) ;; na razie pana nie obsługujemy
	   (unique (zip 
		    (append-map (employ symmetries)
				(complements initial-state
					     board-width
					     board-height))
		    (append-map (employ symmetries)
				(complements final-state
					     board-width
					     board-height)))))))
    definitions))

 (define ((employ symmetries) state-description)
   (append-map
    (match-lambda 
     ((? symbol? symmetry-name)
      (#[known-symmetries symmetry-name] state-description))
     (((? symbol? symmetry-name) procedures ...)
      (#[known-symmetries symmetry-name]
       ((apply compose (map (lambda(p) #[board-transformations p]) procedures))
	state-description))))
    symmetries))

 (define board-transformations
   `((flip-horizontally . ,flip-horizontally)
     (flip-vertically . ,flip-vertically)))
 (define known-symmetries
   `((all-rotations . ,all-rotations)
     (identity . ,list)
     (horizontal . ,horizontal)
     (vertical . ,vertical))))

(define (wins! player round)
  (quit))

(define (final? board)
  #;(window #:name 'winner-announcement
	  (message #[board 'current-player]" wins!")
	  (button "Play again"))
  #f)
