(define-module (schess game)
  ;; functions and classes related to the gameplay process itself
  ;; (changing states, applying rules, verifying final condtions etc.)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra time)
  #:use-module (oop goops)
  #:use-module (ice-9 q)
  #:use-module (schess elements)
  #:use-module (schess rules)
  #:export (<board-game> start-gameplay select-move! allowed-moves)
  )

(define-class <board-game> ()
  (rules #:init-value #f)               ; <- instance of <board-game-rules>
  (board-state #:init-value #f)         ; <- rect initialized in constructor
  (order-of-play #:init-value #f)       ; <- circular list copied from rules
					;    in constructor
  (history #:init-value '())            ; <- list of all moves
  (turn #:init-value 0)
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

(define-method (initialize (self <board-game>) args)
  (next-method)
  (let-keywords args #t ((rule-book #f))
    (when rule-book
      (set! #[self 'rules] (load-board-game rule-book))
      (set! #[self 'board-state] #[self : 'rules : 'initial-state])
      (set! #[self 'order-of-play] #[self : 'rules : 'order-of-play]))
    ))

(define-method (gameplay (game <board-game>))
  (let turn ((n 0) (player #[game 'current-player]))
    (set! #[game 'turn] n)
    (let ((origin (choose-checker #;in game))
	  (move (choose-move #;in game)))
      (apply-move! move #;from origin #;to game #;in-turn n)
      (if (final? game)
	  (wins! player #;in-round n)
       #;else
	  (turn (1+ n) (next-player! #;on game))))))

(define-method (next-player! #;in (game <board-game>))
  (set! #[game 'order-of-play] (rest #[game 'order-of-play]))
  #[game 'current-player])

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
 (define (select-move! #;from origin #;as move #;in game)
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

(define-generic allowed-moves)

(define-method (allowed-moves #;at field-position #;in (game <board-game>))
  (and-let* ((allowed-moves #[game : 'rules : 'allowed-moves 
				   : #[game 'current-player]])
	     (wildcards #[game : 'rules : 'wildcards]))
    (specify ((fit? (fit-wildcards wildcards)))
      (or (possible-moves #;on #[game 'board-state] #;from field-position 
			       #;using allowed-moves)
	  '()))))

(define-method (final? game)
  (let ((state #[game 'board-state])
	(condition #[game : 'rules : 'final-condition]))
    (eval `(let ((current-turn ,#[game 'turn])
		 (current-player ',#[game 'current-player])
		 (next-player ',#[game 'next-player]))
	     ,condition)
	  (current-module))))

(define (wins! player round)
  #;(window #:name 'winner-announcement
	  (message #[board 'current-player]" wins!")
	  (button "Play again"))
  (<< player " wins!")
  (quit))

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
	      state)))

(define (apply-move (initial-state final-state . _) #;from (x0 y0) #;to game)
  ;; this is a functional (non-mutating) version of apply-move!
  (let ((figures #[game : 'rules : 'allowed-figures])
	(figure (take-from-rect #[game 'board-state] x0 y0))
	(wildcards (map first #[game : 'rules : 'wildcards])))
    (match-let* ((((dx dy)) (subrect-indices initial-state `((,figure))))
		 ((x y) `(,(- x0 dx) ,(- y0 dy)))
		 ((w h) (rect-size initial-state))
		 (board-substate (take-subrect #[game 'board-state] x y w h)))
      (values
       (substitute #[game : 'rules : 'wildcards]
		   #;from initial-state
			  #;within board-substate
				   #;in final-state)
       `(,x ,y)))))

(define-method (apply-move! move #;from origin
			    #;to (game <board-game>) #;in-turn n)
  (let*-values (((new-state x&y) (apply-move move #;from origin #;to game))
		((x y) (apply values x&y)))
    (let* ((old #[game 'board-state])
	   (new (replace-subrect #[game 'board-state] 
				 #;with new-state #;at x y)))
      (set! #[game 'board-state] 
	    (replace-subrect #[game 'board-state] #;with new-state #;at x y))
      (apply-after-move-rules! game #;in-turn n))))

(define-method (apply-after-move-rules! (game <board-game>) #;in-turn n)
  (and-let* ((rules #[game : 'rules : 'post-move : #[game 'current-player]])
	     (fields #[game 'board-state]))
    (specify ((fit? (fit-wildcards #[game : 'rules : 'wildcards])))
      (for (initial-state final-state) in rules
	   (match-let (((w h) (rect-size initial-state)))
	     (for (x y) in (subrect-indices fields initial-state)
		  #;(<< "post applying "`(,initial-state ,final-state)
		      " at "`(,x ,y)" in turn "n)
		  (let* ((current-substate (take-subrect fields x y w h))
			 (final-state* (substitute 
					#[game : 'rules : 'wildcards]
					#;from initial-state
					       #;within current-substate
							#;in final-state)))
		    (set! #[game 'board-state]
			  (replace-subrect #[game 'board-state]
					   #;with final-state*
						  #;at x y)))))))))

