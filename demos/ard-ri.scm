(define-game-rules ard-ri

  (initial-board:
   ((X _ ♟ ♟ ♟ _ X)
    (_ _ _ ♟ _ _ _)
    (♟ _ ♙ ♙ ♙ _ ♟)
    (♟ ♟ ♙ ♔ ♙ ♟ ♟)
    (♟ _ ♙ ♙ ♙ _ ♟)
    (_ _ _ ♟ _ _ _)
    (X _ ♟ ♟ ♟ _ X)))

  (wildcards: (□ (♙ ♔))
	      (♟/_ (_ ♟))
	      (_/♟ ♟/_)
	      (_/X (_ X))
	      (X/_ _/X)
	      (□/_ (_ □))
	      (_/□ □/_))

  (images: 
   (♟ "art/chess/bpb.png")
   (♙ "art/chess/wpb.png")
   (♔ "art/chess/wkb.png")
   (X "art/chess/blbp.png"))

  (moves:   
   (player-1
    (♙
     (((♙ _ → _))
      ;;=========
      ((_ _ → ♙))
      (symmetries: all-rotations)))
    (♔
     (((♔ _ → _/X))
      ;;=========
      ((_ _ → ♔))
      (symmetries: all-rotations))))

   (player-2
    (♟ 
     (((♟ _ → _))
      ;;=========
      ((_ _ → ♟))
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
      (? ? ? X)))

    (((♙ ♟ □))
     ;;======
     ((♙ _ □))
     (symmetries: all-rotations))

    (((□ ♟ X))
     ;;======
     ((□ _ X))
     (symmetries: all-rotations)))

   (player-2
    (((♟ ♙ ♟))
     ;;======
     ((♟ _ ♟))
     (symmetries: all-rotations))
)))