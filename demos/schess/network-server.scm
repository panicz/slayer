(define-module (schess network-server)
  ;; this module defines the simplest setup, when all players play
  ;; together on one computer
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra network)
  #:use-module (oop goops)
  #:use-module (schess elements)
  #:use-module (schess rules)
  #:use-module (schess game)
  #:export (<board-network-server> handle-clients!)
  #:re-export (start-gameplay make)
  )

;; plan działania: najpierw piszemy mini-serwer, który obsługuje
;; tylko jedną grę, a następnie skalujemy go na dowolnie wiele gier

(define-class <board-network-server> (<board-game>)
  (gateway #:init-value #f) ;; socket
  (protocols #:init-thunk make-hash-table) ;; hashmap from sockets to protocols
  (available-players #:init-value #f) 
  (next-available-player!
   #:allocation #:virtual
   #:slot-ref (lambda (self)
		(match #[self 'available-players]
		  (() #f)
		  ((first . rest)
		   (set! #[self 'available-players] rest)
		   first)))
   #:slot-set! noop)
  (make-protocol
   #:allocation #:each-subclass
   #:init-value 
   (lambda (client game player)
     (protocol 
      this
      (define (request id call)
	(display (match call
		   (((? #[this _] proc) args ...)
		    (let ((result (apply #[this proc] args)))
		      (format #t "sending ~a as response to ~a\n"
			      result id)
		      `(response ,id ,result)))
		   (else
		    `(invalid-request-call: ,id ,call)))
		 client))
      (define (current-board-state)
	#[game 'board-state])
      (define (image-names)
	#[game : 'rules : 'image-names])
      (define (my-player)
	player)
      (define (moves position)
	(if (eq? #[game 'current-player] player)
	    (allowed-moves #;for position #;in game)
	    '()))
      (define (current-turn)
	#[game 'turn])
      (define (current-player)
	#[game 'current-player])
      (define (next-player)
	#[game 'next-player])
      (define (choose-move! #;from position #;as move)
	(when (eq? #[game 'current-player] player)
	  (select-move! #;from position #;as move #;in game)
	  (broadcast game `(set-board-state! ,#[game 'board-state]))))
      )))
  )

(define-method (broadcast (server <board-network-server>) message)
  (for client in (hash-keys #[server 'protocols])
       (display message client)))

(define-method (initialize (self <board-network-server>) args)
  (next-method)
  (set! #[self 'available-players]
	#[self : 'rules : 'players])
  (let-keywords args #t ((address INADDR_ANY)
			 (port 7777))
    (let ((gateway (socket PF_INET SOCK_STREAM 0)))
      (set! #[self 'gateway] gateway)
      (bind gateway AF_INET address port)
      (listen gateway (length #[self : 'rules : 'players])))))

(define-method (handle-clients! (server <board-network-server>))
  (while #t
    (let ((connection (apply first-available-input-port 
			     #[server 'gateway]
			     (hash-keys #[server 'protocols]))))
      (if (eq? connection #[server 'gateway])
	  (match-let (((client . address) (accept #[server 'gateway])))
	    (format #t "a new client connected: ~a\n"client)
	    (set! #[server : 'protocols : client]
		  (#[server 'make-protocol] 
		   client
		   server 
		   #[server 'next-available-player!])))
      #;else
	  (let ((message (read connection)))
	    (format #t "received message: ~a\n" message)
	    (match message
	      ((proc args ...)
	       (apply (or #[server : 'protocols : connection : proc] noop)
		      args))
	      ((? eof-object?)
	       (hash-remove! #[server 'protocols] connection))))))))
