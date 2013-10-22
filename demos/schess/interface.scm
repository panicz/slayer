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
	    set-board-state!
	    <board> <field> <checker>
	    ))

;; class definitions 
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
  (move #:init-value #f)
  (children #:allocation #:virtual
	    #:slot-ref (lambda (self)
			 (if #[self 'content] 
			     (list #[self 'content]) 
			     '()))
	    #:slot-set! (lambda (self value)
			  (if (list? value)
			      (for x in value
				   (set! #[self 'content] x))))))

(define-class <checker> (<bitmap>)
  (origin #:init-value #f)
  (type #:init-value #f #:init-keyword #:type))

(define-method (initialize (checker <checker>) init-args)
  (next-method)
  (set! #[checker 'left-mouse-down]
	(pick! checker))
  (set! #[checker 'left-mouse-up]
	(put-to-nearby-widget! checker))
  (set! #[checker 'drag]
	(lambda (x y dx dy)
	  (increase! #[checker 'x] dx)
	  (increase! #[checker 'y] dy))))

(define-class <board> (<widget>)
  (fields #:init-value #f)
  (above-fields #:init-value '())
  (allowed-moves #:init-keyword #:allowed-moves)
  (after-move-rules #:init-keyword #:after-move #:init-thunk make-hash-table)
  (order-of-play #:init-keyword #:order-of-play)
  (selected-checker #:init-thunk make-queue)
  (chosen-destination #:init-thunk make-queue)
  (chosen-move #:init-thunk make-queue)
  (images #:init-keyword #:images #:init-thunk make-hash-table)
  (current-player #:allocation #:virtual
		  #:slot-ref (lambda (self)
			       (first #[self 'order-of-play]))
		  #:slot-set! noop)
  (wildcards #:init-keyword #:wildcards)
  (children #:allocation #:virtual
	    #:slot-ref (lambda (self)
			 (append #[self 'above-fields] 
				 (concatenate (array->list #[self 'fields]))))
	    #:slot-set! noop))


(define-method (initialize (board <board>) init-args)
  (next-method)
  (let-keywords init-args #t ((initial-state '((_))))
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
		  (and-let* ((image #[board : 'images : checker])
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

(define (gameplay board)
  (let turn ((n 0) (player #[board 'current-player]))
    (let ((checker (choose-checker #;from board)))
      (apply-move! (choose-move #;on board) #;of checker #;to board)
      #;(move! checker #;to (choose-destination #;on board) #;on board)
      (if (final? board)
	  (wins! player #;in-round n)
       #;else
	  (turn (1+ n) (next-player! #;on board))))))

(define (choose-checker #;from board)
  ;; czekamy, aż na planszy pojawi się jakiś wybór
  (get! #;from #[board 'selected-checker]))

(define (choose-move #;from board)
  (get! #;from #[board 'chosen-move]))

(define (choose-destination #;on board)
  (get! #;from #[board 'chosen-destination]))

(define (select-destination! #;of checker #;as field #;on board)
  (give! field #;via #[board 'chosen-destination])
  (give! #[field 'move] #[board 'chosen-move])
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
	  (else ;; put the checker back to the origin
	   (move! checker #;to #[checker 'origin] #;on board)))))

(define-generic possible-moves)

(define-method (possible-moves field-position (board <board>))
  (and-let* ((allowed-moves #[board : 'allowed-moves : 
				    #[board : 'current-player]])
	     (wildcards #[board 'wildcards])
	     (board-rect (current-board-state/rect board)))
    (specify ((fit? (fit-wildcards wildcards)))
	     (or (possible-moves #;on board-rect #;from field-position 
				      #;using allowed-moves)
		 '()))))

(define-generic possible-destinations)

(define-method (possible-destinations field-position (board <board>))
  (and-let* ((allowed-moves #[board : 'allowed-moves : 
				    #[board : 'current-player]])
	     (wildcards #[board 'wildcards])
	     (board-rect (current-board-state/rect board)))
    (specify ((fit? (fit-wildcards wildcards)))
	     (or (possible-destinations #;on board-rect #;from field-position 
					#;using allowed-moves)
		 '()))))

(define ((pick! checker) . _)
  (and-let* ((field #[checker 'parent])
	     ((is-a? field <field>))
	     (board #[field 'parent])
	     ((is-a? board <board>))
	     (position #[field 'position]))
    ;; tutaj jakoś chcielibyśmy może sprawdzić, które ruchy
    ;; są dozwolone, i odpowiednio je oznaczyć
    (for (initial final figure place) in (possible-moves position board)
	 (match (map + position 
		     (displacement #;of figure #;from initial #;to final))
	   ((x y)
	   ;;(for (x y) in (possible-destinations #[field 'position] board)
	    (allow! #[ #[board 'fields] y x ]
		    `(,initial ,final ,figure ,place)))))
    (take! checker #;from field #;to board)))

(define-method (allowed-figures board)
  (let ((result #[]))
    (for (player => rules) in #[board 'allowed-moves]
	 (for (figure => moves) in rules
	      (set! #[result figure] #t)))
    (hash-keys result)))

(define* (set-board-state! board rect #:optional (x 0) (y 0))
  (match-let (((w h) (rect-size rect)))
    (for r in x .. (- (+ w x) 1)
	 (for c in y .. (- (+ h y) 1)
	      (let ((field #[#[ board 'fields] c r])
		    (checker (take-from-rect rect (- r x) (- c y))))
		(if (not (equal? checker '?))
		    (set! #[field 'content]
			  (and-let* ((image #[board : 'images : checker]))
			    (make <checker> #:image image 
				  #:type checker)))))))))

(publish
 (define-method (apply-move! move #;of checker #;on (board <board>))
   ;; stosowanie ruchu polega na tym, że bierzemy sobie dany fragment
   ;; planszy i zastępujemy go nowym
   (let ((figures (allowed-figures board))
	 (wildcards (map first #[board 'wildcards]))
	 (current-state (current-board-state/rect board)))
     (match-let* (((initial-state final-state figure (dx dy)) move)
		  ((x0 y0) #[checker : 'origin : 'position])
		  ((x y) `(,(- x0 dx) ,(- y0 dy)))
		  ((w h) (rect-size initial-state))
		  (current-substate (take-subrect current-state x y w h))
		  (final-state* (substitute #[board 'wildcards] 
					    #;from initial-state
						   #;within current-substate
							    #;in final-state)))
       (set-board-state! board final-state* x y)
       (set! #[board 'above-fields] (delete checker #[board 'above-fields]))
       (reset! board)
       (apply-after-move-rules! board)
       (force-redisplay!))))

 (define-method (apply-after-move-rules! (board <board>))
   (and-let* ((rules #[board : 'after-move-rules : #[board 'current-player]])
	      (fields (current-board-state/rect board)))
     (specify ((fit? (fit-wildcards #[board 'wildcards])))
       (for (initial-state final-state) in rules
	    (match-let (((w h) (rect-size initial-state)))
	      (for (x y) in (subrect-indices fields initial-state)
		   (let* ((current-substate (take-subrect fields x y w h))
			  (final-state* (substitute 
					 #[board 'wildcards] 
					 #;from initial-state
						#;within current-substate
							 #;in final-state)))
		     (set-board-state! board final-state* x y))))))))
 where
 (define (substitute wildcards #;from subrect #;within rect #;in state)
   (let ((substitutions (filter-map 
			 (lambda (state pattern)
			   (and (in? pattern 
				     (map first wildcards))
				(in? state #[wildcards pattern])
				`(,pattern . ,state)))
			 (concatenate rect)
			 (concatenate subrect))))
     (rect-map (lambda(x)
		 (or #[substitutions x] x))
	       state))))


(define-method (move! (checker <checker>) #;to (field <field>) 
		      #;on (board <board>))
  (set! #[board 'above-fields] (delete checker #[board 'above-fields]))
  (set! #[field 'content] checker)
  (set! #[checker 'x] 0)
  (set! #[checker 'y] 0)
  (set! #[checker 'parent] field)
  (set! #[checker 'origin] #f)
  (reset! board))

(define-method (take! (checker <checker>) #;from (field <field>) 
		      #;on (board <board>))
  (set! #[checker 'x] #[field 'x])
  (set! #[checker 'y] #[field 'y])
  (set! #[field 'content] #f)
  (set! #[checker 'parent] board)
  (set! #[checker 'origin] field)
  (push! #[board 'above-fields] checker))

(define-method (next-player! #;on board)
  (set! #[board 'order-of-play] (rest #[board 'order-of-play]))
  #[board 'current-player])

(define empty-field (load-image "art/chess/b.png"))

(define empty-field* (load-image "art/chess/w.png"))

(define-method (draw (field <field>))
  (next-method)
  (if #[field 'content]
      (draw #[field 'content])))

(define-method (allow! (field <field>) move)
  (set! #[field 'allowed] #t)
  (set! #[field 'move] move)
  (set! #[field 'image] (highlighted #[field 'image] #:green +50)))

(define-method (reset! (board <board>))
  (match-let (((h w) (array-dimensions #[board 'fields])))
    (for x in 0 .. (- w 1)
	 (for y in 0 .. (- h 1)
	      (set! #[ #[ #[board 'fields] y x ] 'allowed] #f)
	      (set! #[ #[ #[board 'fields] y x ] 'move] #f)
	      (set! #[ #[ #[board 'fields] y x ] 'image ]
		    (if (= 0 (modulo (+ x y) 2))
			empty-field
			empty-field*))))))

(publish 
 (define (load-board-game game-description)
   (let ((description (with-input-from-file game-description read)))
     (match description
       (('define-game-rules name . description)
	(set-window-title! (string-append "SCHESS: " (->string name)))
	(let ((initial-board (first #[description 'initial-board:]))
	      (wildcards #[description 'wildcards:])
	      (moves #[description 'moves:])
	      (after-move-rules (or #[description 'after-move:] '()))
	      (images (map (match-lambda 
			       ((figure name)
				`(,figure . ,(subtract-image (load-image name)
							     empty-field))))
			   #[description 'images:])))
	  (let ((wildcards (expand-wildcards wildcards))
		(width (rect-width initial-board))
		(height (rect-height initial-board)))
	    (specify ((wildcards wildcards))
		     (let* ((allowed-moves (extract-moves moves width height))
			    (order-of-play (or #[description 'order-of-play:]
					       (hash-keys allowed-moves))))
		       (make <board> #:initial-state initial-board
			     #:images images
			     #:allowed-moves allowed-moves
			     #:order-of-play (apply circular-list
						    order-of-play)
			     #:after-move (extract-after-move-rules
					   after-move-rules width height)
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
 
 (define (extract-after-move-rules description board-width board-height)
   (let ((result #[]))
     (for (player rules ...) in description
	  (set! #[result player]
		(expand-moves #;from rules #;up-to board-width
				     #;times board-height)))
     result))

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
			  (expand-moves 
			   #;from moves
				  #;up-to board-width 
					  #;times board-height)))))))
     #;return player-moves))

 (define (expand-moves #;from definitions 
			      #;up-to board-width #;times board-height)
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
     (vertical . ,vertical)))

 ) ;D publish load-board-game

(define (wins! player round)
  (quit))

(define (final? board)
  #;(window #:name 'winner-announcement
	  (message #[board 'current-player]" wins!")
	  (button "Play again"))
  #f)
