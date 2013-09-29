
(displacement #;of '♞ #;from '((_ ?)
			       (? ?)
			       (? ♞))  #;to '((♞ ?)
					      (? ?)
					      (? _)))

(e.g. (lset= equal? (complements '((♜ _ → □/_)) 8 8)
	     '(((♜ □/_))
	       ((♜ _ □/_))
	       ((♜ _ _ □/_))
	       ((♜ _ _ _ □/_))
	       ((♜ _ _ _ _ □/_))
	       ((♜ _ _ _ _ _ □/_))
	       ((♜ _ _ _ _ _ _ □/_))))
 (lset= equal?
	(complements 
	 '((? ? … □/_)
	   (⁝ ⁝ ↗  ⁝ )
	   (? _ …  ? )
	   (♝ ? …  ? ))
	 8 8)
	'(((? □/_)
	   (♝  ? ))  ((? ? □/_)
		      (? _  ? )
		      (♝ ?  ? ))  ((? ? ? □/_)
				   (? ? _  ? )
				   (? _ ?  ? )
				   (♝ ? ?  ? ))  ((? ? ? ? □/_)
						  (? ? ? _  ? )
						  (? ? _ ?  ? )
						  (? _ ? ?  ? )
						  (♝ ? ? ?  ? ))
	  ((? ? ? ? ? □/_)
	   (? ? ? ? _  ? )
	   (? ? ? _ ?  ? )
	   (? ? _ ? ?  ? )
	   (? _ ? ? ?  ? )
	   (♝ ? ? ? ?  ? ))  ((? ? ? ? ? ? □/_)
			      (? ? ? ? ? _  ? )
			      (? ? ? ? _ ?  ? )
			      (? ? ? _ ? ?  ? )
			      (? ? _ ? ? ?  ? )
			      (? _ ? ? ? ?  ? )
			      (♝ ? ? ? ? ?  ? )) ((? ? ? ? ? ? ? □/_)
						  (? ? ? ? ? ? _  ? )
						  (? ? ? ? ? _ ?  ? )
						  (? ? ? ? _ ? ?  ? )
						  (? ? ? _ ? ? ?  ? )
						  (? ? _ ? ? ? ?  ? )
						  (? _ ? ? ? ? ?  ? )
						  (♝ ? ? ? ? ? ?  ? )))))

(complements '((a b)
	       (c d)) 8 8)

(e.g.
 (lset= equal?
	(possible-destinations
	 #;on
	 ;; 1 2 3 4 5 6 7 8  ;
	 '((♜ ♟ _ _ ♙ _ _ ♖) ;A
	   (_ ♟ _ _ _ _ ♙ ♘) ;B
	   (♝ ♟ _ _ _ _ ♙ ♗) ;C
	   (♛ ♟ _ _ _ _ ♙ ♕) ;D
	   (♚ ♟ _ ♞ _ _ ♙ ♔) ;E
	   (_ _ _ _ _ _ ♙ ♗) ;F
	   (♞ _ _ _ _ ♙ _ ♘) ;G
	   (_ _ _ _ _ _ ♙ ♖));H
	 #;for   
	 '(3 4)
	 #;from-set
	 `((♞
	    ,@(any-direction '((♞ ? ?)
			       (? ? _))
			     #;=======
			     '((_ ? ?)
			       (? ? ♞))))))
	'((2 2) (4 2) (5 3) (5 5) (4 6) (2 6) (1 5)))
 )


(rect-let
 ((TL … TR)
  (⁝  ↗  ⁝)
  (BL … BR))
 (match-let ((((_ ... lc*)) lc)
	     (((bc*) . _) bc))
   #;"I wish I could write something like this instead, so that the system 
would infer the appropreate code by itself. Maybe some day some brilliant 
hacker will figure it out, but until then we're doomed to analyse all those
lists, maps, appends and applies below:"

)))

(lety-srety-kotlety
 (sequence-of  
  (_)
  ((TL TR)
   (BL BR))
  
  ((TL tc TR)
   (lc re rc)
   (BL bc BR))
  
  ((TL tc  tc  TR)
   (lc lc* re  rc)
   (lc re  bc* rc)
   (BL bc  bc  BR))

  ((TL  tc  tc  tc  TR)
   (lc  lc* lc* re  rc)
   (lc  lc* re  bc* rc)
   (lc  re  bc* bc* rc)
   (BL  bc  bc  bc  BR))
  ...
  (until (or (= (rect-height _) board-height)
	     (= (rect-width _) board-width))))

	(complements	
	 '((TL   tc   …    TR)
	   ( ⁝    ⁝   ↗    ⁝)
	   (lc   __   …    rc)
	   (BL   bc   …     BR))
	 8 8)

	(complements	
	 '((TL   tc   …    tc   TR)
	   (lc   lc*  …    re   rc)
	   ( ⁝    ⁝   ↗    ⁝    ⁝)
	   (lc   __   …    bc*  rc)
	   (BL   bc   …    bc   BR))
	 8 8)

	(complements
	 '((TL   tc   tc    …   tc   tc   TR)
	   (lc   lc*  lc*   …   lc*  re   rc)
	   (lc   lc*  lc*   …   re   bc*  rc)
	   ( ⁝    ⁝    ⁝    ↗    ⁝    ⁝    ⁝)
	   (lc   lc*  re    …   bc*  bc*  rc)
	   (lc   re   bc*   …   bc*  bc*  rc)
	   (BL   bc   bc    …   bc   bc   BR))
	 8 8)


(e.g.
 (lset= equal?
  (expand-wildcards
   '((■ (♟ ♜ ♞ ♝ ♛ ♚))
     (□ (♙ ♖ ♘ ♗ ♕ ♔))
     (? (_ ■ □))
     (■/_ (_ ■))
     (_/■ ■/_)
     (□/_ (_ □))
     (_/□ □/_)))
  '((■ (♟ ♜ ♞ ♝ ♛ ♚)) 
    (□ (♙ ♖ ♘ ♗ ♕ ♔)) 
    (? (_ ♟ ♜ ♞ ♝ ♛ ♚ ♙ ♖ ♘ ♗ ♕ ♔)) 
    (■/_ (_ ♟ ♜ ♞ ♝ ♛ ♚)) 
    (_/■ (_ ♟ ♜ ♞ ♝ ♛ ♚)) 
    (□/_ (_ ♙ ♖ ♘ ♗ ♕ ♔)) 
    (_/□ (_ ♙ ♖ ♘ ♗ ♕ ♔)))))
