(define-module (schess elements)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:export (
	    rect? rect-height rect-width rect-size take-subrect take-from-rect
	    upper-left-corner lower-right-corner upper-right-corner
	    lower-left-corner rect-map
	    subrect-indices displacement complements
	    rotate-left rotate-right flip-horizontally flip-vertically
	    all-rotations any-direction horizontal vertical
	    flip-arrow-vertically flip-arrow-horizontally
	    rotate-arrow-right rotate-arrow-left
	    with-context-for-arrows
	    possible-moves fit-wildcards
	    ))

(define (rect? x)
  (and (list? x)
       (every list? x)
       (apply = (map length x))))

(prototype upper-left-corner : <rect> <natural> <natural> -> <rect>)

(define (upper-left-corner rect w h)
  (map (lambda (row)
	 (take row w))
       (take rect h)))

(e.g.
 (upper-left-corner
  '((a b c)
    (d e f)
    (g h i)) 2 2)
 equal?
 '((a b)
   (d e)))

(define (lower-right-corner rect w h)
  (map (lambda (row)
	 (take-right row w))
       (take-right rect h)))

(e.g.
 (lower-right-corner
  '((a b c)
    (d e f)
    (g h i)) 2 2)
 equal?
 '((e f)
   (h i)))

(define (upper-right-corner rect w h)
  (map (lambda (row)
	 (take-right row w))
       (take rect h)))

(define (lower-left-corner rect w h)
  (map (lambda (row)
	 (take row w))
       (take-right rect h)))

(prototype rect-height : <rect> -> <natural>)

(define (rect-height rect)
  (length rect))

(define (rect-width rect)
  (or 
   (and-let* ((h (rect-height rect))
	      ((positive? h)))
     (length (first rect)))
   0))

(define (rect-size rect)
  (list (rect-width rect) (rect-height rect)))

(e.g.
 (rect-size '((a b c)
	      (d e f)))
 equal?
 '(3 2))

(define (rect-map f rect)
  (map (lambda (row)
	 (map f row))
       rect))

(define (take-subrect rect x y w h)
  (upper-left-corner
   (lower-right-corner rect 
		       (- (rect-width rect) x) 
		       (- (rect-height rect) y))
   w h))

(define (take-from-rect rect x y)
  (match (take-subrect rect x y 1 1)
    (((x)) x))) ; tutaj możnaby równoważnie napisać (first (take-subrect ...)),
;; ale wówczas nie zaakcentowalibyśmy jedyności elementu, który pobieramy

(with-default ((fit? (lambda(field pattern)(eq? pattern '?))))
  (define (rect-match? rect pattern)
    (cond ((atom? rect)
	   (or (eq? rect pattern)
	       (and (list? pattern) (in? rect pattern))
	       ((specific fit?) rect pattern)))
	  ((and (list? rect) (list? pattern))
	   (every rect-match? rect pattern))
	  (else
	   (error "invalid rect or pattern given")))))

(define ((fit-wildcards wildcards) field pattern)
  (or (eq? pattern '?)
      (eq? field pattern)
      (and-let* ((possible-values #[wildcards pattern]))
	(in? field possible-values))))

;; Powiemy, że s jest podtablicą r wtedy, gdy istnieją takie i oraz j,
;; że dla każdego n ∈ [0, (height r)) oraz dla każdego m ∈ [0, (width r)),
;; r[i+n][j+m] = s[n][m]. Wówczas parę (i, j) nazwiemy współrzędnymi
;; podtablicy s w tablicy r.

(prototype (subrect-indices (rect <rect>) (sub <rect>))
	   (assert (and (<= (rect-width sub) (rect-width rect))
			(<= (rect-height sub) (rect-height rect))))
	   -> ((<integer> <integer>) ...))

(define (subrect-indices rect sub)
  (let ((w (rect-width sub))
	(h (rect-height sub)))
    (filter (lambda ((left top))
	      (rect-match? (take-subrect rect left top w h) sub))
	    (cart (iota (- (rect-width rect) w -1)) 
		  (iota (- (rect-height rect) h -1))))))

(prototype (displacement <symbol> <rect> <rect>) -> (<integer> <integer>))

(define (displacement #;of figure #;from source #;to dest)
  (let ((dest-position (subrect-indices dest `((,figure))))
	(source-position (subrect-indices source `((,figure)))))
    (if (or (null? source-position) (null? dest-position))
	#f
	(map - (match dest-position ((x) x))
	     (match source-position ((x) x))))))

(e.g. 
 (displacement #;of '♞ #;from '((_ ?)
				(? ?)
				(? ♞))  #;to '((♞ ?)
					       (? ?)
					       (? _)))
 equal?
 '(-1 -2))

;; the procedures in the following section are used during
;; the parsing of game's rules

(define (transpose rect)
  (apply map list rect))

(e.g.
 (transpose '((a b c)
	      (d e f)))
 equal?
 '((a d)
   (b e)
   (c f)))

(observation: 
 (for-all <rect> (equal? <rect> (transpose (transpose <rect>)))))

(with-default ((rotate-item-left identity))
  (define (rotate-left rect)
    (reverse (transpose (tree-map (specific rotate-item-left) rect)))))

(e.g.
 (rotate-left '((a b c)
		(d e f)))
 equal?
 '((c f)
   (b e)
   (a d)))

(observation:
 (for-all <procedure>
     (if (and (allows-arity? <procedure> 1)
	      (for-all <symbol> 
		  (equal? <symbol> 
			  ((iterations 4 <procedure>) <symbol>))))
	 #;then
	 (specify ((rotate-item-left <procedure>))
	   (for-all <rect> (equal? <rect> 
				   ((iterations 4 rotate-left) <rect>)))))))

(with-default ((rotate-item-right identity))
  (define (rotate-right rect)
    (transpose (reverse (tree-map (specific rotate-item-right) rect)))))

(e.g.
 (rotate-right '((a b c)
		 (d e f)))
 equal?
 '((d a)
   (e b)
   (f c)))

(with-default ((flip-item-horizontally identity))
  (define (flip-horizontally rect)
    (map reverse (tree-map (specific flip-item-horizontally) rect))))

(e.g.
 (flip-horizontally '((a b)
		      (c d)
		      (e f)))
 equal?
 '((b a)
   (d c)
   (f e)))

(with-default ((flip-item-vertically identity))
  (define (flip-vertically rect)
    (reverse (tree-map (specific flip-item-vertically) rect))))

(e.g.
 (flip-vertically '((a b)
		    (c d)
		    (e f)))
 equal?
 '((e f)
   (c d)
   (a b)))

(prototype (all-rotations <rect>) -> (<rect> <rect> <rect> <rect>))

(define (all-rotations rect)
  (map (lambda(n)((iterations n rotate-left) rect)) (iota 4)))

(e.g.
 (all-rotations '((a b c)
		  (d e f)))
 same-set?
 '(((a b c)
    (d e f)) ((c f)
	      (b e)
	      (a d)) ((f e d)
		      (c b a)) ((d a)
				(e b)
				(f c))))

(define (horizontal rect)
  (list rect (flip-horizontally rect)))

(define (vertical rect)
  (list rect (flip-vertically rect)))

(prototype (any-direction (before <rect>) (after <rect>))
	   -> (<move> ...))

(define (any-direction before after)
  (unique
   (append
    (zip (all-rotations before)
	 (all-rotations after))
    (zip (all-rotations (flip-vertically before))
	 (all-rotations (flip-vertically after))))))

(publish
 (define (rotate-arrow-right arrow)
   (or (neighbour arrow orthogonal-arrows +1)
       (neighbour arrow diagonal-arrows +1)
       arrow))
 (define (rotate-arrow-left arrow)
   (or (neighbour arrow orthogonal-arrows -1)
       (neighbour arrow diagonal-arrows -1)
       arrow))
 where
 (define (neighbour item list n)
   (and-let* ((i (list-index (equals? item) list)))
     (list-ref list (modulo (+ i n) (length list)))))
 (define orthogonal-arrows '(← ↑ → ↓))
 (define diagonal-arrows '(↖ ↗ ↘ ↙)))

(e.g. (rotate-arrow-right '↖) eq? '↗)

(publish 
 (define (flip-arrow-vertically arrow)
   (counterpart arrow upward-arrows downward-arrows))
 (define (flip-arrow-horizontally arrow)
   (counterpart arrow leftward-arrows rightward-arrows))
 #;(define (transpose-arrow arrow)
   (counterpart arrow transpose-1 transpose-2))
 where
 (define (counterpart item list1 list2)
   (or 
    (and-let* ((i (list-index (equals? item) list1)))
      (list-ref list2 i))
    (and-let* ((i (list-index (equals? item) list2)))
      (list-ref list1 i))
    item))
 (define upward-arrows   '(↖ ↑ ↗))
 (define downward-arrows '(↙ ↓ ↘))
 (define leftward-arrows  '(↙ ← ↖))
 (define rightward-arrows '(↘ → ↗))
 #;(define transpose-1 '(← → ↗))
 #;(define transpose-2 '(↑ ↓ ↙)))

(e.g. (flip-arrow-horizontally '←) eq? '→)

(define-syntax with-context-for-arrows
  (syntax-rules ()
    ((_ actions ...)
     (specify (#;(transpose-item transpose-arrow)
	       (rotate-item-left rotate-arrow-left)
	       (rotate-item-right rotate-arrow-right)
	       (flip-item-horizontally flip-arrow-horizontally)
	       (flip-item-vertically flip-arrow-vertically))
       actions ...))))

(e.g.
 (with-context-for-arrows
  (rotate-right '((↖ ↑ ↗)
		  (↙ ↓ ↘))))
 equal?
 '((↖ ↗)
   (← →)
   (↙ ↘)))

(publish 
 (define (complements state-description board-width board-height)
   (let ((arrow-position (subrect-indices state-description
					  '(((← ↑ → ↓ ↖ ↗ ↘ ↙))))))
     (match arrow-position
       (()
	(list state-description))
       (((x y))
	(let ((arrow (take-from-rect state-description x y))
	      (flip (iterations 2 rotate-left)))
	  (with-context-for-arrows
	   (case arrow
	     ((→ ↗) (twisted-complements identity identity
					 arrow x y
					 state-description
					 board-width board-height))
	     ((↑ ↖) (twisted-complements rotate-right rotate-left
					 arrow y x
					 state-description
					 board-height board-width))
	     ((← ↙) (twisted-complements flip flip
					 arrow x y
					 state-description
					 board-width board-height))
	     ((↓ ↘) (twisted-complements rotate-left rotate-right
					 arrow y x
					 state-description
					 board-height board-width))))))
       (((x y) rest ...)
	(unless (or (every (lambda ((x y))
			     (in? (take-from-rect state-description x y)
				  '(← →)))
			   `((,x ,y) ,@rest))
		    (every (lambda ((x y))
			     (in? (take-from-rect state-description x y)
				  '(↑ ↓)))
			   `((,x ,y) ,@rest)))
	  (error "Unsupported configuration of arrows")))
       (else
	(error "No match: " arrow-position)))))

 where
 (define (twisted-complements transform-there transform-back arrow 
			      x y state-description board-width board-height)
   (map transform-back
	((proper-complements #;for arrow) (transform-there state-description)
	 x y board-width board-height)))
 (define (proper-complements #;for arrow)
   (if (in? arrow '(← ↑ → ↓))
       right-complements
       top-right-complements))
 (define (right-complements desc x y board-width -board-height-)
   (let ((w (rect-width desc))
	 (h (rect-height desc)))
     (match-let ((((filling-column) ...) (take-subrect desc (- x 1) y 1 h))
		 (suffix (take-subrect desc (+ x 1) 0 (- w x 1) h))
		 (prefix (take-subrect desc 0 0 (- x 1) h)))
       (map (lambda (n)
	      (map append prefix
		   (if (zero? n)
		       (make-list h '())
		       (transpose (make-list n filling-column)))
		   suffix))
	    (iota (- board-width w -3))))))
 (define (slice-rect rect x y)
   ;; the column (x *) and the row (* y) are dismissed
   (let ((w (rect-width rect))
	 (h (rect-height rect)))
     (let ((top-left (upper-left-corner rect x y))
	   (top-right (upper-right-corner rect (- w x 1) y))
	   (bottom-left (lower-left-corner rect x (- h y 1)))
	   (bottom-right (lower-right-corner rect (- w x 1) (- h y 1))))
       `((,top-left       ,top-right)
	 (,bottom-left ,bottom-right)))))
 (define (diagonal n top mid bot)
   ;; generates a square list-of-lists/matrix of size n that looks like this:
   ;; ((top top ... top mid)
   ;;  (top top ... mid bot)
   ;;           ...
   ;;  (top mid ... bot bot)
   ;;  (mid bot ... bot bot))
   (map (lambda (k)
	  (append (make-list (- n k 1) top) 
		  (list mid) 
		  (make-list k bot)))
	(iota n)))
 (define (top-right-complements desc x y board-width board-height)
   (match-let* (([(((TL ... tc) ...)     TR )
		  (((lc ... re) 
		    (BL ... bc) ...)    (rc 
					 BR 
					 ...))]  (slice-rect desc x y))
		((_ ... lc*) lc)
		((bc* _ ...) bc))
     ;; Maybe some day some brilliant hacker will figure out the way to
     ;; write it like that, but until then we're doomed to analyse all those
     ;; lists, maps, appends and applies below.
     ;;
     ;; (sequence (_)
     ;; ((TL TR)
     ;;  (BL BR))  ((TL tc TR)
     ;;             (lc re rc)
     ;;             (BL bc BR))  ((TL tc  tc  TR)
     ;;                           (lc lc* re  rc)
     ;; 			  (lc re  bc* rc)
     ;; 			  (BL bc  bc  BR))  ((TL  tc  tc  tc  TR)
     ;; 					     (lc  lc* lc* re  rc)
     ;; 					     (lc  lc* re  bc* rc)
     ;; 					     (lc  re  bc* bc* rc)
     ;; 					     (BL  bc  bc  bc  BR))
     ;; 				...
     ;;  (until (or (= (rect-height _) board-height)
     ;; 	    (= (rect-width _) board-width))))
     (map (lambda (n)
	    (append
	     (map append TL 
		  (apply map append 
			 (if (zero? n) 
			     (list (make-list (length TL) '()))
			     (make-list n (map list tc)))) 
		  TR)
	     (map append (make-list n lc)
		  (diagonal n lc* re bc*)
		  (make-list n rc))
	     (map append BL 
		  (apply map append 
			 (if (zero? n)
			     (list (make-list (length BL) '()))
			     (make-list n (map list bc)))) 
		  BR)))
	  (iota (+ 3 (min (- board-width (rect-width desc))
			  (- board-height (rect-height desc))))))))
 ) ;D publish complements

#| w przypadku jak powyżej sprawa jest prosta:
(complements '((x _ → y _ → x)) n m)
== (append-map (lambda (R)
		 (complements `((x _ → ,@R)) n m))
	       (complements '((y _ → x)) n m))

|#

(e.g. ; that uses right-complements
 (complements '((♜ _ → □/_)) 6 6)
 same-set?
 '(((♜ □/_))
   ((♜ _ □/_))
   ((♜ _ _ □/_))
   ((♜ _ _ _ □/_))
   ((♜ _ _ _ _ □/_))))

(e.g. ; should work for multi-dimensional cases as well
 (complements '((x _ → y)
		(z _ … v)) 4 4)
 same-set?
 '(((x y) 
    (z v)) 
   ((x _ y) 
    (z _ v)) 
   ((x _ _ y) 
    (z _ _ v)) ))

(e.g. ; that uses top-right-complements
 (complements 
  '((? ? … □/_)
    (⁝ ⁝ ↗  ⁝ )
    (? _ …  ? )
    (♝ ? …  ? ))
  5 5)
 same-set?
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
					   (♝ ? ? ?  ? ))))

(prototype (possible-moves #;on <rect> #;at (<natural> <natural>) 
			   ((<figure> <move> ...) ...))	   
	   -> ((<rect> <rect>) ...))

(define (possible-moves #;on board/rect #;at (x y) #;among allowed-moves)
  (and-let* ((figure (take-from-rect board/rect x y))
	     (moves #[allowed-moves figure]))
    (unique
     (filter-map
      (lambda ((initial-state final-state))
	(match (subrect-indices initial-state `((,figure)))
	  (((dx dy))
	   (and 
	    (not (null? (filter 
			 (equals? `(,(- x dx) ,(- y dy)))
			 (subrect-indices board/rect initial-state))))
	    `(,initial-state ,final-state)))))
      moves))))

(e.g.
 (possible-moves
  #;on
  ;;x0 1 2 3 4 5 6 7   y
  '((♜ ♟ _ _ ♙ _ _ ♖) ;0
    (_ ♟ _ _ _ _ ♙ ♘) ;1
    (♝ ♟ _ _ _ _ ♙ ♗) ;2
    (♛ ♟ _ _ _ _ ♙ ♕) ;3
    (♚ ♟ _ ♞ _ _ ♙ ♔) ;4
    (_ ♟ _ _ _ _ ♙ ♗) ;5
    (♞ _ _ _ ♟ ♙ _ ♘) ;6
    (_ _ _ _ _ _ ♙ ♖));7
  #;at
  '(3 4)
  #;among
  `((♞
     ,@(any-direction '((♞ ? ?)
			(? ? _))
		      #;=======
		      '((_ ? ?)
			(? ? ♞))))))
 same-set?
 '((((_ ?) 
     (? ?) 
     (? ♞)) ((♞ ?) 
	     (? ?) 
	     (? _))) 
   (((♞ ? ?) 
     (? ? _)) ((_ ? ?) 
	       (? ? ♞))) 
   (((? ♞) 
     (? ?) 
     (_ ?)) ((? _) 
	     (? ?) 
	     (♞ ?))) 
   (((? ? _) 
     (♞ ? ?)) ((? ? ♞) 
	       (_ ? ?))) 
   (((? _) 
     (? ?) 
     (♞ ?)) ((? ♞)
	     (? ?) 
	     (_ ?)))))
