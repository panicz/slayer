(define-game-rules chess

  (initial-board:
   ((♜ ♟ _ _ _ _ ♙ ♖)
    (♞ ♟ _ _ _ _ ♙ ♘)
    (♝ ♟ _ _ _ _ ♙ ♗)
    (♛ ♟ _ _ _ _ ♙ ♕)
    (♚ ♟ _ _ _ _ ♙ ♔)
    (♝ ♟ _ _ _ _ ♙ ♗)
    (♞ ♟ _ _ _ _ ♙ ♘)
    (♜ ♟ _ _ _ _ ♙ ♖)))

  (wildcards: (■ (♟ ♜ ♞ ♝ ♛ ♚))
	      (□ (♙ ♖ ♘ ♗ ♕ ♔))
	      (■/_ (_ ■))
	      (_/■ ■/_)
	      (□/_ (_ □))
	      (_/□ □/_))

  ;; figury: (♟ ♜ ♞ ♝ ♛ ♚ ■ ♙ ♖ ♘ ♗ ♕ ♔ □)
  ;; symbole specjalne: ← ↑ → ↓ ↖ ↗ ↘ ↙ 

  (images: 
   (♟ "art/chess/bpb.png")
   (♜ "art/chess/brb.png")
   (♞ "art/chess/bnb.png")
   (♝ "art/chess/bbb.png")
   (♛ "art/chess/bqb.png")
   (♚ "art/chess/bkb.png")
   (♙ "art/chess/wpb.png")
   (♖ "art/chess/wrb.png")
   (♘ "art/chess/wnb.png")
   (♗ "art/chess/wbb.png")
   (♕ "art/chess/wqb.png")
   (♔ "art/chess/wkb.png"))
  
  (moves:
   (player-1
    (♟ ;; PIONEK
     (((♟ _)) ;; w dowolnym miejscu szachownicy pionek może ruszyć się 
      ;;====  ;; na wolne pole przed sobą
      ((_ ♟)))
     
     (((? ♟ _ _ ? ? ? ?))  ;; startując z pozycji początkowej, pionek może się
      ;;================   ;; się o dwa pola -- wówczas jednak może zostać
      ((? _ _ ♟ ? ? ? ?))) ;; "zbity w przelocie" przez pionka przeciwnika
     
     (((♟ ?)  ;; bicie pionkiem
       (? □))
      ;;====
      ((_ ?)
       (? ♟))
      (symmetries: horizontal))

     (((? ? ? ? ♟ _ ? ?)  ;; bicie w przelocie
       (? ? ? ? ♙ _ _ ?))
      ;;================
      ((? ? ? ? _ _ ? ?)
       (? ? ? ? _ ♟ _ ?))
      (symmetries: horizontal)
      (conditions: #;"ruch ♙ o dwa pola musiał mieć miejsce w poprzedniej turze")))
    
    (♜ ;; WIEŻA
     (((♜ _ → □/_)) ;; poruszanie się i bicie
      ;;=========
      ((_ _ → ♜))
      (symmetries: all-rotations)))

    (♝ ;; GONIEC
     (((? ? … □/_)
       (⁝ ⁝ ↗  ⁝ )
       (? _ …  ? )
       (♝ ? …  ? ))
      ;;=========
      ((? ? …  ♝ )
       (⁝ ⁝ ↗  ⁝ )
       (? _ …  ? )
       (_ ? …  ? ))
      (symmetries: all-rotations)))

    (♞ ;; SKOCZEK
     (((♞ ? ?  )
       (? ? □/_))
      ;;========
      ((_ ? ?)
       (? ? ♞))
      (symmetries: 
       all-rotations (all-rotations flip-horizontal))))

    (♛ ;; HETMAN
     (((♛ _ → □/_))
      ;;==========
      ((_ _ → ♛))
      (symmetries: all-rotations))

     (((? ? … □/_)
       (⁝ ⁝ ↗  ⁝ )
       (? _ …  ? )
       (♛ ? …  ? ))
      ;;==========
      ((? ? … ♛)
       (⁝ ⁝ ↗ ⁝)
       (? _ … ?)
       (_ ? … ?))
      (symmetries: all-rotations)))

    (♚ ;; KRÓL
     ;; może poruszać się o jedno pole, dokonując bicia
     (((♚ □/_))
      ;;=======
      ((_  ♚ ))
      (symmetries: all-rotations))
     
     (((? □/_)
       (♚  ? ))
      ;;====
      ((? ♚)
       (_ ?))
      (symmetries: all-rotations))

     ;; roszada
     (((♜)
       (_)
       (_)
       (_)
       (♚))
      ;;==
      ((_)
       (_)
       (♚)
       (♜)
       (_))
      (conditions: #;"król ani wieża nie mogły być ruszane"))
     
     (((♚)
       (_)
       (_)
       (♜))
      ;;==
      ((_)
       (♜)
       (♚)
       (_))
      (conditions: #;"król ani wieża nie mogły być ruszane")))
    );D player-1 
   (player-2 (transform player-1
			(opposites:
			 ((♟ ♙)
			  (♜ ♖)
			  (♞ ♘)
			  (♝ ♗)
			  (♛ ♕)
			  (♚ ♔)))
			(transformations:
			 flip-vertical)))
   ) ;D moves
  ) ;D define-game-rules
