(define-module (schess game)
  ;; functions and classes related to the gameplay process itself
  ;; (changing states, applying rules, verifying final condtions etc.)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (oop goops)
  #:use-module (ice-9 q)
  #:use-module (schess elements)
  #:use-module (schess rules)
  #:export (<board-game-interface>
	    <board-game> 
	    start-gameplay 
	    select-move! 
	    allowed-moves
	    next-player!
	    remember-move!
	    remember-state!
	    apply-move!
	    final?
	    allowed-moves
	    ))

(define-class <board-game-interface> ()
  (rules #:init-value #f)               ; <- instance of <board-game-rules>
  (board-state #:init-value #f)         ; <- rect initialized in constructor
  (order-of-play #:init-value '())      ; <- circular list copied from rules
					;    in constructor
  (past-moves #:init-value '())         ; <- list of previous moves
  (past-states #:init-value '())        ; <- previous states of the board
  (environment #:init-value #f)         ; <- module to evaluate conditions
  (turn #:init-value 0)
  (current-player #:init-value #f)
  (next-player #:init-value #f))

(define-generic next-player!)

(define-generic remember-move!)

(define-generic remember-state!)

(define-method (initialize (self <board-game-interface>) args)
  (next-method)
  (set! #[self 'environment] (schess-execution-module #;for self)))  

(define-class <board-game> (<board-game-interface>)
  (current-player 
   #:allocation #:virtual
   #:slot-ref (lambda (self)
		(first #[self 'order-of-play]))
   #:slot-set! noop)
  (next-player
   #:allocation #:virtual
   #:slot-ref (lambda (self)
		(first (rest #[self 'order-of-play])))
   #:slot-set! noop))

(define-method (next-player! #;in (game <board-game>))
  (set! #[game 'order-of-play] (rest #[game 'order-of-play]))
  #[game 'current-player])

(define-method (remember-move! #;from origin #;as move #;in (game <board-game>))
  (set! #[game 'past-moves] (cons `((origin . ,origin)
				    (player . ,#[game 'current-player])
				    (move . ,move))
				  #[game 'past-moves])))

(define-method (remember-state! state #;in (game <board-game>))
  (set! #[game 'past-states] (cons state #[game 'past-states])))

(define (schess-execution-module game)
  (let ((module (make-fresh-user-module)))
    (module-define! module 'the-game game)
    (within-module module
      (use-modules (extra common) (extra ref) (oop goops) (schess elements))
      (define* (board-state #:key (in-turn #f))
	(if (not in-turn)
	    #[the-game 'board-state]
	    (let* ((past-states #[the-game 'past-states])
		   (number-of-moves (length past-states)))
	      (if (< in-turn 0)
		  (let ((n (- 0 in-turn 1)))
		    (if (< n number-of-moves)
			#[past-states n]
			#[the-game : 'rules : 'initial-state]))
	      #;else
		  (if (< in-turn number-of-moves)
		      #[past-states (- number-of-moves in-turn 1)]
		      #f)))))
      (define (fits-somewhere? pattern board-state)
	(not (null? (subrect-indices #;of pattern #;in board-state))))
      (define (current-turn) #[the-game 'turn])
      (define (current-player) #[the-game 'current-player])
      (define field take-from-rect))
    module))

(define-method (initialize (self <board-game>) args)
  (next-method)
  (let-keywords args #t ((rule-book #f))
    (when rule-book
      (set! #[self 'rules] (load-board-game rule-book))
      (set! #[self 'board-state] #[self : 'rules : 'initial-state])
      (set! #[self 'order-of-play] 
	    (apply circular-list #[self : 'rules : 'order-of-play])))))

(define-method (gameplay (game <board-game>))
  (let turn ((n 0) (player #[game 'current-player]))
    (set! #[game 'turn] n)
    (let ((origin (choose-checker #;in game))
	  (move (choose-move #;in game)))
      (apply-move! move #;from origin #;to game)
      (if (final? game)
	  (wins! player #;in-round n)
       #;else
	  (turn (1+ n) (next-player! #;on game))))))

;; now this is interesting!
(publish
 (define-method (start-gameplay (game <board-game>))
   (call-with-prompt 
    schess-prompt-tag
    (lambda () (gameplay game))
    save-stage!))
 (define-method (choose-checker #;in (game <board-game>))
   (abort-to-prompt schess-prompt-tag))
 (define-method (choose-move #;in (game <board-game>))
   (abort-to-prompt schess-prompt-tag))
 (define-method (select-move! #;from origin #;as move #;in (game <board-game>))
   (remember-move! #;from origin #;as move #;in game)
   (yield origin)
   (yield move))
 where
 (define game-stage (make-q))
 (define (save-stage! stage)
   (enq! game-stage stage))
 (define ((next-stage! . values))
   (apply (deq! game-stage) values))
 (define schess-prompt-tag (make-prompt-tag "schess"))
 (define (yield . values)
   (call-with-prompt schess-prompt-tag
		     (apply next-stage! values)
		     save-stage!)))

(define (allowed-moves #;at field-position #;in game)
  (and-let* ((allowed-moves #[game : 'rules : 'allowed-moves 
				   : #[game 'current-player]])
	     (wildcards #[game : 'rules : 'wildcards]))
    (specify ((fit? (fit-wildcards wildcards)))
      (or (possible-moves #;on #[game 'board-state] #;from field-position 
			       #;using allowed-moves)
	  '()))))

(define-method (final? (game <board-game-interface>))
  (eval
   #[game : 'rules : 'final-condition]
   #[game 'environment]))

(define-method (satisfied? conditions #;with pattern #;at x y
			   #;in (game <board-game-interface>))
  (eval
   `(let ((positions 
	   (let ((pattern ',pattern))
	     (lambda figures
	       (map (lambda ((x y)) `(,(+ x ,x) ,(+ y ,y)))
		    (append-map (lambda(figure)
				  (subrect-indices 
				   #;of `((,figure)) #;in pattern))
				figures))))))
      ,@conditions)
   #;in #[game 'environment]))

(define (wins! player round)
  #;(window #:name 'winner-announcement
	  (message #[board 'current-player]" wins!")
	  (button "Play again"))
  (<< player " wins!")
  (quit))

(define (substitute wildcards #;from subrect #;within rect #;in state)
  ;;(format #t "(substitute ~a ~a ~a ~a)\n" wildcards subrect rect state)
  (let ((substitutions (filter-map 
			(lambda (state pattern)
			  (and (in? pattern (map first wildcards))
			       (in? state #[wildcards pattern])
			       `(,pattern . ,state)))
			(concatenate rect)
			(concatenate subrect))))
    (rect-map (lambda(x)
		(or #[substitutions x] x))
	      state)))

(publish
 (define-method (apply-move! move #;from origin 
			     #;to (game <board-game-interface>))
   (let*-values (((new-substate x&y) (apply-move move #;from origin #;to game))
		 ((x y) (apply values x&y))
		 ((old) #[game 'board-state])
		 ((new) (replace-subrect old #;with new-substate #;at x y)))
     (remember-state! old #;in game)
     (set! #[game 'board-state] new)
     (apply-after-move-rules! game)))
 where
 (define (apply-move (initial-state final-state . _) #;from (x0 y0) #;to game)
   (let ((figures #[game : 'rules : 'allowed-figures])
	 (figure (take-from-rect #[game 'board-state] x0 y0))
	 (wildcards (map first #[game : 'rules : 'wildcards])))
     (match-let* ((((dx dy)) (subrect-indices 
			      #;of `((,figure)) #;in initial-state))
		  ((x y) `(,(- x0 dx) ,(- y0 dy)))
		  ((w h) (rect-size initial-state))
		  (board-substate (take-subrect #[game 'board-state] x y w h)))
       (values
	(substitute #[game : 'rules : 'wildcards]
		    #;from initial-state
			   #;within board-substate
				    #;in final-state)
	`(,x ,y))))))

(define-method (apply-after-move-rules! (game <board-game-interface>))
  (and-let* ((rules #[game : 'rules : 'post-move : #[game 'current-player]])
	     (fields #[game 'board-state]))
    (specify ((fit? (fit-wildcards #[game : 'rules : 'wildcards])))
      (for (initial-state final-state . conditions) in rules
	   (match-let (((w h) (rect-size initial-state)))
	     (for (x y) in (subrect-indices #;of initial-state #;in fields)
		  (if (satisfied? conditions #;with initial-state #;at x y
				  #;in game)
		      (let* ((current-substate (take-subrect fields x y w h))
			     (final-state* (substitute 
					    #[game : 'rules : 'wildcards]
					    #;from initial-state
						   #;within current-substate
							    #;in final-state)))
			(set! #[game 'board-state]
			      (replace-subrect #[game 'board-state]
					       #;with final-state*
						      #;at x y))))))))
    (remember-state! fields #;in game)))
