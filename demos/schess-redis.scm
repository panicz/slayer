#!/usr/bin/guile \
-L ./guile-modules -L ../guile-modules -L . -e main -s
!#

(set-port-encoding! (current-input-port) "UTF-8")
(set-port-encoding! (current-output-port) "UTF-8")
(set-port-encoding! (current-error-port) "UTF-8")
(fluid-set! %default-port-encoding "UTF-8")

(use-modules (extra common) (extra ref) (extra redis)
	     (schess rules) (ice-9 threads) (oop goops)
	     ((schess redis) #:prefix redis:))

(define-syntax-rule (with-game (name id) action . *)
  (let ((name (redis:game id)))
    (if (not name)
	(format #f "game #~s does not exist" id)
	(begin action . *))))

(define-syntax-rule (match/command-line 
		     program-name/string command 
		     (pattern . actions) 
		     ...)
  (let ((program-name (string->symbol program-name/string)))
    (match (map read-string command)
      (pattern . actions)
      ...
      (else
       (pretty-print `(unrecognised command: ,else))
       (pretty-print `(usages: (,program-name . pattern) ...))))))

(define (write-if-specified x) ;; i.e. not unspecified
  (unless (unspecified? x)
    (write x)))

(define (main (program-name command ...))
  ;; note that we write strings in case of errors, and non-strings
  ;; in case of success (therefore no object should be represented
  ;; as string)
  (pretty-print ;write-if-specified
   (match/command-line
    program-name command

    (('load-rules game-name)
     (let ((filename (string-append (->string game-name) ".ss")))
       (if (not (file-exists? filename))
	   (format #f "rule file `~a' does not exist" filename)
	   (let ((rules (load-board-game filename)))
	     (make <redis-object-proxy> #:as game-name #:target rules)
	     'OK))))

    (('erase-rules game-type)
     (let ((rules (make <redis-object-proxy> #:as game-type)))
       (erase! rules)
       'OK))

    (('available-game-types)
     (filter (string-matches "\\.ss$") (list-directory ".")))

    (('all-games) #;=> (redis:all-games))

    (('create game-id type)
     (cond ((redis:game-exists? game-id)
	    (let ((game (redis:game game-id)))
	      (format #f "game #~s already exists with type `~s'"
		      game-id #[game 'game-type])))
	   ((not (redis:rules-exist? #;for type))
	    (format #f "the ruleset for game `~s' is not available" type))
	   (else
	    (redis:create-game! type game-id)
	    'OK)))

    (('players game-id)
     (with-game (game game-id) 
	 (unique #[game : 'rules : 'order-of-play])))

    (('game-type game-id)
     (with-game (game game-id) 
	 #[game 'game-type]))

    (('allowed-moves game-id x y)
     (with-game (game game-id) 
	 (redis:allowed-moves #;at `(,x ,y) #;in game)))

    (('allowed-destinations game-id x y)
     (with-game (game game-id) 
	 (redis:allowed-destinations #;at `(,x ,y) #;in game)))

    (('current-board game-id)
     (with-game (game game-id)
	 #[game 'board-state]))

    (('current-player game-id)
     (with-game (game game-id) 
	 #[game 'current-player]))

    (('current-turn game-id)
     (with-game (game game-id)
	 #[game 'turn]))

    (('current-state game-id)
     (with-game (game game-id)
	 (redis:game-state game)))

    (('make-move game-id from-x from-y to-x to-y)
     (with-game (game game-id)
	 (let ((origin `(,from-x ,from-y))
	       (destination `(,to-x ,to-y)))
	   (redis:make-move! #;from origin #;to destination #;in game))))

    )))
