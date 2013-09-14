(define-game-rules chess

  (initial-board
   ((♜ ♟ _ _ _ _ ♙ ♖)
    (♞ ♟ _ _ _ _ ♙ ♘)
    (♝ ♟ _ _ _ _ ♙ ♗)
    (♛ ♟ _ _ _ _ ♙ ♕)
    (♚ ♟ _ _ _ _ ♙ ♔)
    (♝ ♟ _ _ _ _ ♙ ♗)
    (♞ ♟ _ _ _ _ ♙ ♘)
    (♜ ♟ _ _ _ _ ♙ ♖)))

  ;; figury: (♟ ♜ ♞ ♝ ♛ ♚ ■ ♙ ♖ ♘ ♗ ♕ ♔ □)
  ;; symbole specjalne: ← ↑ → ↓ ↖ ↗ ↘ ↙ ↔ ↕
  
  (♟ ;; PIONEK
   ;; w dowolnym miejscu szachownicy pionek może ruszyć się 
   ;; na wolne pole przed sobą
   (((♟ _))
    #;====
    ((_ ♟)))

   ;; startując z pozycji początkowej, pionek może ruszyć się
   ;; o dwa pola -- wówczas jednak może zostać "zbity w przelocie"
   ;; przez pionka przeciwnika
   (((? ♟ _ _ ? ? ? ?))
    #;================
    ((? _ _ ♟ ? ? ? ?)))

   ;; bicie pionkiem
   (((♟ ?)
     (? □))
    #;====
    ((_ ?)
     (? ♟))
    (symmetries: horizontal)
    )

  ;; bicie w przelocie
   (((? ? ? ? ♟ _ ? ?)
     (? ? ? ? ♙ _ _ ?))
    #;================
    ((? ? ? ? _ _ ? ?)
     (? ? ? ? _ ♟ _ ?))
    (symmetries: horizontal)
    (conditions: #;"ruch ♙ o dwa pola musiał mieć miejsce w poprzedniej turze"))
   )

  (♜ ;; WIEŻA
   ;; poruszanie się i bicie
   (((♜ _ → □/_))
    #;=========
    ((_ _ → ♜)))

    (symmetries: rotation4)))

  (♝ ;; GONIEC
   (((? ? … □/_)
     (⁝ ⁝ ↗  ⁝ )
     (? _ …  ? )
     (♝ ? …  ? ))
    #;=========
    ((? ? … ♝)
     (⁝ ⁝ ↗ ⁝)
     (? _ … ?)
     (_ ? … ?))
    (symmetries: rotation4)))

  (♞ ;; SKOCZEK
   (((♞ ? ?  )
     (? ? □/_))
    #;========
    ((_ ? ?)
     (? ? ♞))
    (symmetries: 
     rotation4 (rotation4 horizontal))))

  (♛ ;; HETMAN
   ;; hetman
   (((♛ _ → □/_))
    #;==========
    ((_ _ → ♛)))
    (symmetries: rotation4))

  (((? ? … □/_)
    (⁝ ⁝ ↗  ⁝ )
    (? _ …  ? )
    (♛ ? …  ? ))
   #;==========
   ((? ? … ♛)
    (⁝ ⁝ ↗ ⁝)
    (? _ … ?)
    (_ ? … ?))
   (symmetries: rotation4)))

  (♚ ;; KRÓL
   ;; może poruszać się o jedno pole, dokonując bicia
   (((♚ □/_))
    #;=======
    ((_  ♚ ))
    (symmetries: all))

   ;; roszada
   (((♜)
     (_)
     (_)
     (_)
     (♚))
    #;==
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
    #;==
    ((_)
     (♜)
     (♚)
     (_))
    (conditions: #;"król ani wieża nie mogły być ruszane")))
