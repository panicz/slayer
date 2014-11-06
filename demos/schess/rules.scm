(define-module (schess rules)
  ;; functions and classes related to loading, preprocessing and storing rules.
  ;; the application of rules happens in the (schess game) module, which is
  ;; responsible for maintaining game logic
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (oop goops)
  #:use-module (schess elements)
  #:export (<board-game-rules> load-board-game))

(define-class <board-game-rules> ()
  (game-name #:init-value "" #:init-keyword #:game-name)
  (initial-state ;type: ((figure ...) ...)
   #:init-keyword #:initial-state)
  (allowed-moves ;type: (player => (figure => ((initial final . _) ...)))
   #:init-keyword #:allowed-moves)
  (immobile ;type: (figure ...)
   #:init-keyword #:immobile)
  (players ;type: (player ...)
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (hash-keys #[self 'allowed-moves]))
   #:slot-set! noop)
  (allowed-figures ;type: (figure ...)
   #:allocation #:virtual
   #:slot-ref ;; tutaj można by było dorobić cache'owanie
   (lambda (self)
     (let ((result #[]))
       (for (player => rules) in #[self 'allowed-moves]
	    (for (figure => moves) in rules
	      (set! #[result figure] #t)))
       (hash-keys result)))
   #:slot-set! noop)
  (figures ;type: (((or '*immobile* player) (figure ...)) ...)
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (let ((result #[]))
       (for (player => rules) in #[self 'allowed-moves]
	 (for (figure => moves) in rules
	   (set! #[result player] `(,figure . ,(or #[result player] '())))))
       `((*immobile* ,#[self 'immobile])
	 ,@(hash-map->list list result))))
   #:slot-set! noop)
  ;; we store image names here (instead of loading images) because
  ;; in some cases (e.g. server) 
  (image-names ;type: ((figure string) ...)
   #:init-keyword #:image-names #:init-value '())
  (post-move ;type: (player => ((initial final . _) ...))
   #:init-keyword #:post-move #:init-thunk make-hash-table)
  (order-of-play ;type: (player ...)
   #:init-keyword #:order-of-play)
  (final-condition ;type: expression
   #:init-keyword #:final-condition)
  (wildcards ;type: ((wildcard figures ...) ...)
   #:init-keyword #:wildcards))

(define (load-board-game game-description)
  (let ((description (with-input-from-file game-description read)))
    (match description
      (('define-game-rules name . description)
       (let ((initial-board (first #[description 'initial-board:]))
	     (wildcards (expand-wildcards (or #[description 'wildcards:] '())))
	     (moves #[description 'moves:])
	     (after-move-rules (or #[description 'after-move:] '()))
	     (final-condition (first (or #[description 'finish:] '(#f))))
	     (immobile (or #[description 'immobile:] '()))
	     (image-names #[description 'images:]))
	 (let ((width (rect-width initial-board))
	       (height (rect-height initial-board)))
	   (specify ((wildcards wildcards))
	     (let* ((allowed-moves (extract-moves moves width height))
		    (order-of-play (or #[description 'order-of-play:]
				       (hash-keys allowed-moves))))
	       (make <board-game-rules>
		 #:game-name name
		 #:initial-state initial-board
		 #:image-names image-names
		 #:allowed-moves allowed-moves
		 #:immobile immobile
		 #:order-of-play order-of-play
		 #:post-move (extract-after-move-rules
			      after-move-rules width height)
		 #:final-condition final-condition
		 #:wildcards wildcards
		 )))))))))

;; all the functions below are used internally by load-board-game

(define (expand-wildcards wildcards)
  (define (value term)
    (match (find (lambda ((name value)) 
		   (equal? name term))
		 wildcards)
      ((name (values ...))
       (append-map value values))
      ((name atomic-value)
       (value atomic-value))
      (else
       (list term))))
  (map (lambda ((name definition))
	 `(,name ,@(value name)))
       wildcards))

(e.g. (expand-wildcards '((■ (♟ ♜ ♞ ♝ ♛ ♚))
			  (□ (♙ ♖ ♘ ♗ ♕ ♔))
			  (■/_ (_ ■))
			  (□/_ (_ □)))) ===> ((■ ♟ ♜ ♞ ♝ ♛ ♚) 
					      (□ ♙ ♖ ♘ ♗ ♕ ♔) 
					      (■/_ _ ♟ ♜ ♞ ♝ ♛ ♚)
					      (□/_ _ ♙ ♖ ♘ ♗ ♕ ♔)))

(define (extract-after-move-rules description board-width board-height)
  (let ((result #[]))
    (for (player rules ...) in description
	 (set! #[result player]
	       (expand-moves #;from rules #;up-to board-width
				    #;times board-height)))
    #;return result))

(define (extract-moves movement-description board-width board-height)
  (let ((player-moves (make-hash-table)))
    (for (player . description) in movement-description
	 (match description
	   ((('transform player-name . details))
	    (if (eq? player-name '*immobile*)
		(error "The symbol *immobile* can't be used as player name"))
	    (let ((moves #[player-moves player-name])
		  (opposite (let* ((opposites (or #[details 'opposites:] '()))
				   (reciprocal `(,@(map reverse opposites)
						 ,@opposites)))
			      (lambda(figure)
				(first (or #[reciprocal figure]
					   `(,figure))))))
		  (transformations (map (lambda(name)
					  #[board-transformations name])
					(or #[details 'transformations:] '())))
		  (new-moves #[]))
	      (for (figure => move-description) in moves
		   (set! #[new-moves (opposite figure)]
			 (map (lambda ((initial-state final-state . extra))
				`(,@(map (lambda (state-description)
					   ((apply compose transformations)
					    (tree-map opposite
						      state-description)))
				     `(,initial-state ,final-state))
				  . ,extra))
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
   (lambda ((initial-state final-state . extra))
     (let ((symmetries (or #[extra 'symmetries:]
			   '(identity)))
	   (conditions (or #[extra 'conditions:]
			   '(#t))))
       (map (lambda ((initial final))
	      `(,initial ,final . ,conditions))
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
