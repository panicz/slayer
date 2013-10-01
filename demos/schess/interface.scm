(define-module (schess interface)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (widgets base)
  #:use-module (widgets bitmap)
  #:use-module (schess elements)
  #:use-module (extra slayer)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (oop goops)
  #:export (load-board-game
	    current-board-state/array
	    ))

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
  (type #:init-value #f #:init-keyword #:type))

(define-method (initialize (checker <checker>) init-args)
  (next-method)
  (set! #[checker 'left-mouse-down] ;; powinno być start-dragging
	(lambda (x y)
	  (and-let* ((field #[checker 'parent])
		     ((is-a? field <field>))
		     (board #[field 'parent])
		     ((is-a? board <board>)))
	    ;; tutaj jakoś chcielibyśmy może sprawdzić, które ruchy
	    ;; są dozwolone, i odpowiednio je oznaczyć
	    ;;(<< (possible-destinations #[field 'position] board))
	    (for (x y) in (possible-destinations #[field 'position] board)
		 (highlight! #[ #[board 'fields] y x ]))
	    ;;(take! checker #;from field #;to board)
	    (set! #[checker 'x] #[field 'x])
	    (set! #[checker 'y] #[field 'y])
	    (set! #[field 'content] #f)
	    (set! #[checker 'parent] board)
	    (push! #[board 'above-fields] checker)
	    )))
  (set! #[checker 'left-mouse-up] ;; powinno być stop-dragging
	(lambda (x y)
	  (and-let* (((is-a? *nearby-widget* <field>))
		     (board #[checker 'parent])
		     ((is-a? board <board>)))
	    (attenuate! board)
	    ;;(put-back! checker #;from board #;to field)
	    (set! #[board 'above-fields] (delete checker
						 #[board 'above-fields]))
	    (set! #[*nearby-widget* 'content] checker)
	    (set! #[checker 'x] 0)
	    (set! #[checker 'y] 0)
	    (set! #[checker 'parent] *nearby-widget*))))
  (set! #[checker 'drag]
	(lambda (x y dx dy)
	  (increase! #[checker 'x] dx)
	  (increase! #[checker 'y] dy))))

(define-class <board> (<widget>)
  (fields #:init-value #f)
  (above-fields #:init-value '())
  (allowed-moves #:init-keyword #:allowed-moves)
  (wildcards #:init-keyword #:wildcards)
  (children #:allocation #:virtual
	    #:slot-ref (lambda (self)
			 (append #[self 'above-fields] 
				 (flatten (array->list #[self 'fields]))))
	    #:slot-set! noop))

(define-method (highlight! (field <field>))
  (set! #[field 'image] (highlighted #[field 'image] #:green +50)))

(define-method (attenuate! (board <board>))
  (match-let (((h w) (array-dimensions #[board 'fields])))
    (for x in 0 .. (- w 1)
	 (for y in 0 .. (- h 1)
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
  (and-let* (;;(current-player #[board 'current-player])
	     (allowed-moves #[board : 'allowed-moves : 'player-1])
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
			     #:wildcards wildcards
			     ;;#:dimensions `(,width ,height)
		       #;(rules allowed-moves))))))))))
 where
 (define (expand-wildcards wildcards)
   (define (value term)
     (match (find (match-lambda ((name value)(equal? name term))) wildcards)
       ((name (values ...))
	(append-map value values))
       ((name atomic-value)
	(value atomic-value))
       (else
	(list term))))
   (map (match-lambda ((name definition) `(,name ,@(value name)))) wildcards))

 (define (extract-moves movement-description board-width board-height)
   (let ((player-moves (make-hash-table)))
     (for (player . description) in movement-description
	  (match description
	    ((('transform player-name . details))
	     (<< description))
	    (((figures . moves) ...)
	     (let ((figure-moves (make-hash-table)))
	       (hash-set! player-moves player figure-moves)
	       (for (figure moves) in (zip figures moves)
		    (hash-set! figure-moves figure
			       (expand-moves #;for figure #;from moves #;at
						   board-width 
						   board-height)))))))
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
       ((apply compose (map (lambda(p) #[transformations p]) procedures))
	state-description))))
    symmetries))

 (define transformations
   `((flip-horizontally . ,flip-horizontally)
     (flip-vertically . ,flip-vertically)))

 (define known-symmetries
   `((all-rotations . ,all-rotations)
     (identity . ,list)
     (horizontal . ,horizontal)
     (vertical . ,vertical)))
 )
