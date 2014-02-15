(define-game-rules solitaire
  (initial-board:
   ((X X o o o X X)
    (X X o o o X X)
    (o o o o o o o)
    (o o o _ o o o)
    (o o o o o o o)
    (X X o o o X X)
    (X X o o o X X)))

  (wildcards: (O (o)))

  (images:
   (o "art/chess/wpb.png")
   (X "art/chess/blbp.png"))

  (moves:   
   (the-only-player
    (o
     (((o O _))
      ;;=========
      ((_ _ o))
      (symmetries: all-rotations))))))
