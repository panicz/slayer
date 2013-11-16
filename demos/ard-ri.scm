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
     (((Q _/X/T → _/X/T))
      ;;=========
      ((_ _/X/T → Q))
      (symmetries: all-rotations))))

   (player-2
    (i 
     (((i _ → _))
      ;;=========
      ((_ _ → i))
      (symmetries: all-rotations)))))

  (order-of-play: player-1 player-2)

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
     (symmetries: all-rotations))

    (((O i X/T))
     ;;======
     ((O _ X/T))
     (symmetries: all-rotations)))

   (player-2
    (((i o i))
     ;;======
     ((i _ i))
     (symmetries: all-rotations))

    (((i o X))
     ;;======
     ((i _ X))
     (symmetries: all-rotations)))
   )
  (finish:
   

   )
) ;D define-game-rules
