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
   (♟ "art/chess/wpb.png")
   (♜ "art/chess/wrb.png")
   (♞ "art/chess/wnb.png")
   (♝ "art/chess/wbb.png")
   (♛ "art/chess/wqb.png")
   (♚ "art/chess/wkb.png")
   (♙ "art/chess/bpb.png")
   (♖ "art/chess/brb.png")
   (♘ "art/chess/bnb.png")
   (♗ "art/chess/bbb.png")
   (♕ "art/chess/bqb.png")
   (♔ "art/chess/bkb.png"))
  
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
       all-rotations (all-rotations flip-horizontally))))

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
      (conditions: #;"król ani wieża nie mogły być ruszane"
       ;; albo: nie istnieje

       ))
     
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
			 (♟ ♙) (♜ ♖) (♞ ♘) (♝ ♗) (♛ ♕) (♚ ♔) (■ □)
			 (■/_ □/_) (_/■ _/□))
			(transformations:
			 flip-vertically)))
   ) ;D moves
  (order-of-play: player-1 player-2)
 
  (finish: 
   ;; dla każdego ruchu przeciwnika istnieje taki ruch aktualnego
   ;; gracza, który bije króla przeciwnika
   ;; moglibyśmy chcieć zakodować regułę finalizującą następująco:
   (let ((opponent's-king (if (equal? current-player 'player-1) ♔ ♚)))
     (for-all x in (moves #;of next-player)
	      (let ((next-state (apply-move x #;to current-state)))
		(exists y in (moves #;of current-player)
			(let ((last-state (apply-move y #;to next-state)))
			  (not (in-rect? last-state `((opponent's-king)))))))))
   )
  ) ;D define-game-rules
