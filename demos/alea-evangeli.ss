(define-game-rules hnefatafl

  (initial-board:
   ((X _ i _ _ i _ _ _ _ _ _ _ i _ _ i _ X)
    (_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
    (i _ _ _ _ i _ _ _ _ _ _ _ i _ _ _ _ i)
    (_ _ _ _ _ _ _ i _ i _ i _ _ _ _ _ _ _)
    (_ _ _ _ _ _ i _ o _ o _ i _ _ _ _ _ _)
    (i _ i _ _ i _ _ _ _ _ _ _ i _ _ i _ i)
    (_ _ _ _ i _ _ _ _ o _ _ _ _ i _ _ _ _)
    (_ _ _ i _ _ _ _ o _ o _ _ _ _ i _ _ _)
    (_ _ _ _ o _ _ o _ g _ o _ _ o _ _ _ _)
    (_ _ _ i _ _ o _ g Q g _ o _ _ i _ _ _)
    (_ _ _ _ o _ _ o _ g _ o _ _ o _ _ _ _)
    (_ _ _ i _ _ _ _ o _ o _ _ _ _ i _ _ _)
    (_ _ _ _ i _ _ _ _ o _ _ _ _ i _ _ _ _)
    (i _ i _ _ i _ _ _ _ _ _ _ i _ _ i _ i)
    (_ _ _ _ _ _ i _ o _ o _ i _ _ _ _ _ _)
    (_ _ _ _ _ _ _ i _ i _ i _ _ _ _ _ _ _)
    (i _ _ _ _ i _ _ _ _ _ _ _ i _ _ _ _ i)
    (_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
    (X _ i _ _ i _ _ _ _ _ _ _ i _ _ i _ X)
	))

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
   (i "art/chess/wpb.png")
   (o "art/chess/bpb.png")
   (g "art/chess/bpbp.png")
   (Q "art/chess/bkb.png")
   (T "art/chess/wlw.png")
   (X "art/chess/blbp.png"))

  (moves:   
   (player-1
    (o
     (((o _ → _))
      ;;=========
      ((_ _ → o))
      (symmetries: all-rotations)))
    (g
     (((g _ → _))
      ;;=========
      ((_ _ → g))
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

  (order-of-play: player-2 player-1)

  (after-move:
   (player-1
    (((X ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? _))
     ;;===================
     ((X ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? ?)
      (? ? ? ? ? ? ? ? ? T))
     )

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
     (symmetries: all-rotations))
)))