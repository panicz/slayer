(define-module (schess simple-board-gameplay)
  ;; this module defines the simplest setup, when all players play
  ;; together on one computer
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (oop goops)
  #:use-module (schess elements)
  #:use-module (schess rules)
  #:use-module (schess game)
  #:use-module (schess widgets)
  #:export (<simple-board-gameplay>)
  #:re-export (start-gameplay make)
  )

(define-class <simple-board-gameplay> (<board-game> <board>)
  ;; <simple-board-gameplay> represents the simplest setup, namely
  ;; -- n human players playing a game on one computer.
  (on-pick-checker ;; override
   #:init-value 
   (lambda (checker #;at field #;on board)
     (and-let* ((position #[field 'position])
		(figure (apply take-from-rect #[board 'board-state] position))
		(moves (allowed-moves position board)))
       (match-let (((x y) position))
	 (for (initial final . _) in moves
	      (match (displacement #;of figure #;from initial #;to final)
		((dx dy)
		 (allow! #[ #[board 'fields] (+ y dy) (+ x dx) ]
			 `(,initial ,final))))))))
   )
  (on-drop-checker ;; override
   #:init-value
   (lambda (checker #;at field #;on board)
     (cond (#[field 'allowed]
	    (select-move! #;from #[checker : 'origin : 'position]
				 #;as #[field 'move] #;on board)
	    (synchronize-fields! board #[board 'board-state]))
	   (else
	    (move! checker #;to #[checker 'origin] #;on board)))
     (set! #[board 'above-fields] (delete checker #[board 'above-fields]))
     (reset! board)
     ))
  )

(define-method (initialize (self <simple-board-gameplay>) args)
  (next-method)
  (when #[self 'rules]
    (set! #[self 'images] (load-images #[self : 'rules : 'image-names] 
				       #[self 'image-transformer]))
    (setup-fields! self #[self : 'rules : 'initial-state])))
