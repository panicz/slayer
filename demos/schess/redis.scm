(define-module (schess redis)
  #:use-module (extra common) 
  #:use-module (extra ref) 
  #:use-module (extra network) 
  #:use-module (extra redis)
  #:use-module (oop goops) 
  #:use-module (schess game)
  #:use-module (schess rules)
  #:use-module ((redis commands hashes) #:prefix redis:)
  #:use-module ((redis commands lists) #:prefix redis:)
  #:use-module ((redis commands keys) #:prefix redis:)
  #:use-module ((redis commands sets) #:prefix redis:)
  #:use-module (redis main)
  #:export (<redis-board-game>
	    game-exists?
	    rules-exist?
	    create-game!
	    game
	    )
  #:re-export (select-move!))

(define-class <redis-board-game> (<board-game-interface> <redis-proxy>)
  (game-type  #:allocation #:virtual
	      #:slot-ref (redis-hash-getter 'game-state 'game-type)
	      #:slot-set! (redis-hash-setter 'game-state 'game-type))
  (board-state #:allocation #:virtual
	       #:slot-ref (redis-hash-getter 'game-state 'board-state)
	       #:slot-set! (redis-hash-setter 'game-state 'board-state))
  (order-of-play #:allocation #:virtual 
		 #:slot-ref (redis-list-retriever 'order-of-play)
		 #:slot-set! (redis-list-establisher 'order-of-play))
  (past-moves #:allocation #:virtual
	      #:slot-ref (redis-list-retriever 'past-moves)
	      #:slot-set! noop)
  (past-states #:allocation #:virtual
	       #:slot-ref (redis-list-retriever 'past-states)
	       #:slot-set! noop)
  (environment #:init-value #f)
  (turn #:allocation #:virtual
	#:slot-ref (redis-hash-getter 'game-state 'turn)
	#:slot-set! (redis-hash-setter 'game-state 'turn))
  (current-player #:allocation #:virtual 
		  #:slot-ref (redis-list-getter 'order-of-play 0)
		  #:slot-set! noop)
  (next-player  #:allocation #:virtual
		#:slot-ref (redis-list-getter 'order-of-play 1)
		#:slot-set! noop))

(define-method (initialize (self <redis-board-game>) args)
  (next-method)
  (if #[self 'game-type]
      (set! #[self 'rules] (make <redis-object-proxy> 
			     #:as #[self 'game-type]))))

(define-method (next-player! #;in (game <redis-board-game>))
  (let ((name (->string `(,@(slot-ref game 'redis-name) order-of-play))))
    (redis-send (slot-ref game 'redis) (redis:rpoplpush name name))))

(define-method (remember-move! #;from origin #;as move 
				      #;in (game <redis-board-game>))
  (let ((name (->string `(,@(slot-ref game 'redis-name) past-moves))))
    (redis-send 
     (slot-ref game 'redis)
     (redis:rpush name (->string `((origin . ,origin)
				   (player . ,#[game 'current-player])
				   (move . ,move)))))))

(define-method (remember-state! #;in (game <redis-board-game>))
  (let ((name (->string `(,@(slot-ref game 'redis-name) past-states))))
    (redis-send (slot-ref game 'redis)
		(redis:rpush name (->string #[game 'board-state])))))

#|
(define (select-move! #;from origin #;to destination #;in game)
  ;; jeszcze zobaczymy!
  (let* ((figure (apply take-from-rect #[game 'board-state] origin))
	 (possible-moves (allowed-moves #;at origin #;in game))
	 (move (find (lambda ((initial final . _))
		       (equal?
			destination
			(map + origin
			     (displacement #;of figure #;from initial
						#;to final))))
		     possible-moves)))
    (if (not move)
	(format #f "move from ~s to ~s not allowed in game ~s" origin
		destination game-id)
	(begin
	  (redis:select-move! move #;from origin 
			      #;to destination #;in game)
	  ;; powinniśmy zwrócić bieżącego gracza, albo
	  ;; informację o tym, że któraś ze stron wygrała,
	  ;; a najlepiej po prostu stan gry
	  'OK))))
|#

(define (rules-exist? type)
  (let ((connection (redis-connect))
	(essential-key (->string `(,type))))
    (not (zero? (redis-send connection (redis:exists essential-key))))))

(define (game-exists? game-id)
  (let ((connection (redis-connect))
	(essential-key (->string `(game ,game-id game-state))))
    (not (zero? (redis-send connection (redis:exists essential-key))))))

(define (game game-id)
  (if (game-exists? game-id)
      (make <redis-board-game> #:redis-name `(game ,game-id))
      #f))

(define (create-game! type id)
  (let ((game (make <redis-board-game> #:redis-name `(game ,id) 
		    #:game-type type)))
    (redis-send #[game 'redis] (redis:sadd "games" (list (->string id))))
    (set! #[game 'game-type] type)
    (set! #[game 'rules] (make <redis-object-proxy> #:as type))
    (set! #[game 'order-of-play] #[game : 'rules : 'order-of-play])
    (set! #[game 'board-state] #[game : 'rules : 'initial-state])
    (set! #[game 'turn] 0)
    game))
