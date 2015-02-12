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
      (symmetries: vertical))

     (((? ? ? ? ♟ _ ? ?)  ;; bicie w przelocie
       (? ? ? ? ♙ _ _ ?))
      ;;================
      ((? ? ? ? _ _ ? ?)
       (? ? ? ? _ ♟ _ ?))
      (symmetries: vertical)
#|
      (conditions: 
       (let ((previous-move (last #;in history)))
	 (and (equal? (actor previous-move) ♙)
	      (matches (initial-state previous-move)
		       ((? ? ? ? _ _ ♙ ?)))))
       )
|#
      ))
    
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
      (conditions: 
       (and (not (last-move ♜)) (not (last-move ♚))))
      )
     
     (((♚)
       (_)
       (_)
       (♜))
      ;;==
      ((_)
       (♜)
       (♚)
       (_))
      (conditions: 
       (and (not (last-move ♜)) (not (last-move ♚))))
      ))
    );D player-1 
   (player-2 (transform player-1
			(opposites:
			 (♟ ♙) (♜ ♖) (♞ ♘) (♝ ♗) (♛ ♕) (♚ ♔) (■ □)
			 (■/_ □/_) (_/■ _/□))
			(transformations:
			 flip-horizontally)))
   ) ;D moves
  (order-of-play: player-1 player-2)
  
  #;(finish:
   (let ((opponents-king (if (equal? current-player 'player-1) '♔ '♚)))
     ;; jak to zgrabnie "ująć intelektualnie"?
     ;; w celach wyjaśnienia trzeba sobie co nieco podefiniować:
     ;; - abstrakcyjny ruch składa się z informacji o:
     ;;   * stane początkowym
     ;;   * stanie końcowym
     ;;   * tym, która figura wykonuje ruch
     ;;   * tym, ile wyniesie przesunięcie (x,y) figury po ruchu (nadmiarowo!)
     ;; - konkretny ruch = abstrakcyjny ruch + pozycja na planszy
     ;; 
     ;; jaka myśl stoi za poniższym kodem:
     ;; 
     
     (for-every (figure => move) in (moves #;of next-player)
       (for-every (x y) in (dimensions #;of board)
	 (let ((moves (possible-moves #;at `(,x ,y) #;on board)))
	   
	   ...)
	 ))))

  #;(finish: 
   ;; dla każdego ruchu przeciwnika istnieje taki ruch aktualnego
   ;; gracza, który bije króla przeciwnika
   ;; moglibyśmy chcieć zakodować regułę finalizującą następująco:
   (let ((opponent's-king (if (equal? current-player 'player-1) '♔ '♚)))
     (for-all x in (moves #;of next-player)
	      (let ((next-state (apply-move x #;to current-state)))
		(exists y in (moves #;of current-player)
			(let ((last-state (apply-move y #;to next-state)))
			  (not (in-rect? last-state `((,opponents-king)))))))))
   )
  ) ;D define-game-rules

