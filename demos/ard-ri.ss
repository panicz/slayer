(define-game-rules ard-ri
  (initial-board:
   ((X _ i i i _ X)
    (_ _ _ i _ _ _)
    (i _ o o o _ i)
    (i i o Q o i i)
    (i _ o o o _ i)
    (_ _ _ i _ _ _)
    (X _ i i i _ X)))

  (wildcards: (O (o Q))
	      (i/_ (_ i))
	      (_/i i/_)
	      (_/X (_ X))
	      (X/_ _/X)
	      (_/X/T (_ X T))
	      (X/T (X T))
	      (O/_ (_ O))
	      (_/O O/_))

  (images:
   (i "art/chess/bpb.png")
   (o "art/chess/wpb.png")
   (Q "art/chess/wkb.png")
   (T "art/chess/wlw.png")
   (X "art/chess/blbp.png"))

  (moves:   
   (player-1
    (o
     (((o _ → _))
      ;;=========
      ((_ _ → o))
      (symmetries: all-rotations)))

    (Q
     (((Q _/X → _/X))
      ;;=========
      ((_ _/X → Q))
      (symmetries: all-rotations))
     (((Q _ → T))
      #;========
      ((_ _ → Q))
      (symmetries: all-rotations))
     (((Q _ → T _ → _))
      #;=============
      ((_ _ → T _ → Q))
      (symmetries: all-rotations))))

   (player-2
    (i 
     (((i _ → _))
      ;;=========
      ((_ _ → i))
      (symmetries: all-rotations)))))

  (order-of-play: player-2 player-1)

  (after-move:
   (player-1
    (((X ? ? ?)
      (? ? ? ?)
      (? ? ? ?)
      (? ? ? _))
     ;;========
     ((X ? ? ?)
      (? ? ? ?)
      (? ? ? ?)
      (? ? ? T)))

    (((o i O))
     ;;======
     ((o _ O))
     (conditions:
      (exists (x y) in (positions 'O 'o)
	(eq? (field (board-state #:in-turn -1) x y) '_)))
     (symmetries: all-rotations))

    (((O i X/T))
     ;;======
     ((O _ X/T))
     (conditions:
      (exists (x y) in (positions 'O)
	(eq? (field (board-state #:in-turn -1) x y) '_)))
     (symmetries: all-rotations)))

   (player-2
    (((i o i))
     ;;======
     ((i _ i))
     (conditions: 
      (exists (x y) in (positions 'i)
	(eq? (field (board-state #:in-turn -1) x y) '_)))
     (symmetries: all-rotations))

    (((i o X))
     ;;======
     ((i _ X))
     (conditions: 
      (exists (x y) in (positions 'i)
	(eq? (field (board-state #:in-turn -1) x y) '_)))
     (symmetries: all-rotations))))

  (finish:
   (case (current-player)
     ((player-1) 
      (exists pattern in (all-rotations '((Q ? ? ? ? ? X)))
	(fits-somewhere? pattern (board-state))))
     ((player-2)
      (fits-somewhere? '((? i ?)
			 (i Q i)
			 (? i ?)) (board-state)))))
) ;D define-game-rules

