(define-module (schess network-client)
  ;; this module defines the simplest setup, when all players play
  ;; together on one computer
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra network)
  #:use-module (oop goops)
  #:use-module (schess widgets)
  #:use-module (schess elements)
  #:use-module (slayer)
  #:export (<board-network-client> remote request)
  #:re-export (make)
  )

(define-class <network-client> ()
  (server #;socket #:init-value #f)
  (requests #:init-thunk make-hash-table)
  (reactions #:init-thunk make-hash-table) ;; initialized in constructor
  (thread #:init-value #f))

(define-method (remote (client <network-client>) message)
  (format #t "sending ~a to server\n" message)
  (write message #[client 'server]))

(define-method (request (client <network-client>) message (handler <procedure>))
  (let ((id (gensym "rq-")))
    (format "request: received ~a\n" message)
    (set! #[client : 'requests : id] handler)
    (remote client `(request ,id ,message))))

(define-method (handle-response (client <network-client>) id . args)
  (let ((handler #[client : 'requests : id]))
    (cond ((procedure? handler)
	   (format #t "handle-response for ~a: ~a\n" id args)
	   (hash-remove! #[client 'requests] id)
	   (apply handler args))
	  (else 
	   (format #t "Unsupported request: ~a\n" id)))))

(define-method (initialize (self <network-client>) args)
  (next-method)
  (let-keywords args #t ((address #f)
			 (port 7777))
    (if address
	(let ((socket (socket PF_INET SOCK_STREAM 0))
	      (address (make-socket-address AF_INET (inet-aton address) port))
	      (code (register-userevent!
		     (lambda (message)
		       (match message
			 ((action args ...)
			  (let ((handler #[self : 'reactions : action]))
			    (if handler
				(apply handler args)
				(format #t "No handler for request: ~a\n" 
					action))))
			 (else
			  (format #t "Malformed message: ~a\n" message)))))))
	  (connect socket address)
	  (merge-hashes! #[self 'reactions]
			  (protocol
			   (define (response id . args)
			     (apply handle-response self id args))))
	  (set! #[self 'server] socket)
	  (set! #[self 'thread]
		(call-with-new-thread
		 (lambda ()
		   (while #t
		     (let ((message (read #[self 'server])))
		       (format #t "received ~a\n"message)
		       (generate-userevent! code message))))))))))

(define-class <board-network-client> (<board> <network-client>)
  ;; tutaj zdarzenie on-pick-checker powinno być obsłużone w taki sposób,
  ;; żeby pytało serwer o dostępne ruchy, i w odpowiedzi podświetlało
  ;; /dopuszczało/ odpowiednie pola.
  (my-player #:init-value #f)
  (current-player #:init-value #f)
  (on-pick-checker ;; override
   #:init-value 
   (lambda (checker #;at field #;on board)
     (if (is-a? board <network-client>)
	 (request 
	  board `(moves ,#[field 'position])
	  (lambda (moves)
	    (match-let ((figure #[checker 'type])
			((x y) #[field 'position]))
	      (for (initial final . _) in moves
		   (match (displacement #;of figure #;from initial #;to final)
		     ((dx dy)
		      (allow! #[ #[board 'fields] (+ y dy) (+ x dx) ]
			      `(,initial ,final)))))))))))

  ;; przy upuszczeniu pionka (on-drop-checker) na pole uznane za dozwolone,
  ;; chcemy wysłać do serwera komunikat o wykonanym przez nas ruchu.
  (on-drop-checker ;; override
   #:init-value
   (lambda (checker #;at field #;on board)
     (cond (#[field 'allowed]
	    (remote
	     board
	     `(choose-move! #;from ,#[checker : 'origin : 'position]
				   #;as ,#[field 'move])))
	   (else
	    (move! checker #;to #[checker 'origin] #;on board)))
     (set! #[board 'above-fields] (delete checker #[board 'above-fields]))
     (reset! board)
  ;; ponadto chcielibyśmy mieć (być może w osobnym wątku) coś, co
  ;; odbiera sygnały od serwera i w określonych okolicznościach aktualizuje
  ;; planszę
  ;; można by też cyklicznie (np. co 3 sekundy) wysyłać żądanie z prośbą
  ;; o podanie aktualnego stanu, żeby nie stracić animuszu
  )))

(define-method (initialize (self <board-network-client>) args)
  (next-method)
  (merge-hashes! #[self 'reactions]
		 (protocol
		  (define (set-board-state! state)
		    (synchronize-fields! self state))
		  (define (set-current-player! player)
		    (set! #[self 'current-player] player))
		  ))
  (when #[self 'server]
    (request self '(my-player) 
	     (lambda (player)
	       (set! #[self 'my-player] player)))
    (request self '(image-names)
	     (lambda (images)
	       (set! #[self 'images] (load-images 
				      images
				      #[self 'image-transformer]))))
    (request self '(current-board-state)
	     (lambda (state)
	       (setup-fields! self state)
	       )
	     ))
  )
