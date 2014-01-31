(define-module (schess elements)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:export (
	    rect? rect-height rect-width rect-size take-subrect take-from-rect
	    upper-left-corner lower-right-corner upper-right-corner
	    lower-left-corner rect-map replace-subrect
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

(e.g. (rect? '((a b c)
	       (d e f))))

#;(define-type <rect>
  rect?)

(prototype upper-left-corner : <rect> <natural> <natural> -> <rect>)

(define (upper-left-corner rect w h)
  (map (lambda (row)
	 (take row w))
       (take rect h)))

(e.g. (upper-left-corner '((a b c)
			   (d e f)
			   (g h i)) 2 2) ===> ((a b)
					       (d e)))

(define (lower-right-corner rect w h)
  (map (lambda (row)
	 (take-right row w))
       (take-right rect h)))

(e.g. (lower-right-corner '((a b c)
			    (d e f)
			    (g h i)) 2 2) ===> ((e f)
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

(e.g. (rect-size '((a b c)
		   (d e f))) ===> (3 2))

(define (rect-map f . rects)
  (apply map (lambda rows
	       (apply map f rows))
	 rects))

(e.g.
 (rect-map + '((1 2 3)
	       (4 5 6)) '((6 5 4)
			  (3 2 1))) ===> ((7 7 7)
					  (7 7 7)))

(define (take-subrect rect x y w h)
  (upper-left-corner
   (lower-right-corner rect 
		       (- (rect-width rect) x) 
		       (- (rect-height rect) y))
   w h))

(e.g. (take-subrect '((a b c d)
		      (e f g h)) 1 0 2 2) ===> ((b c)
						(f g)))

(define (take-from-rect rect x y)
  (match (take-subrect rect x y 1 1)
    (((x)) x))) ; tutaj możnaby równoważnie napisać (first (take-subrect ...)),
;; ale wówczas nie zaakcentowalibyśmy jedyności elementu, który pobieramy

(e.g. (take-from-rect '((a b c)
			(d e f)
			(g h i)) 1 0) ===> b)

(define (take-rows rect n)
  (take rect n))

(e.g. (take-rows '((a b c)
		   (d e f)) 1) ===> ((a b c)))

(define (drop-rows rect n)
  (drop rect n))

(e.g. (drop-rows '((a b c)
		   (d e f)) 1) ===> ((d e f)))

(define (take-columns rect n)
  (map (lambda(row)(take row n)) rect))

(e.g. (take-columns '((a b c)
		      (d e f)) 1) ===> ((a)
					(d)))

(define (drop-columns rect n)
  (map (lambda(row)(drop row n)) rect))

(e.g. (drop-columns '((a b c)
		      (d e f)) 1) ===> ((b c)
					(e f)))

(define (append-columns rect . rects)
  (apply map append rect rects))

(e.g. (append-columns '((a b)
			(c d)) '((e)
				 (f))) ===> ((a b e)
					     (c d f)))

(define (append-rows . rects)
  (apply append rects))

(e.g. (append-rows '((a b c)) '((d e f))) ===> ((a b c)
						(d e f)))

(define (replace-subrect #;of original #;with replacement #;at x y)
  (match-let* (((w h) (rect-size replacement))
	       (TOP (take-rows original y))
	       (MID (take-rows (drop-rows original y) h))
	       (LEFT (take-columns MID x))
	       (ORIG (take-columns (drop-columns MID x) w))
	       (RIGHT (drop-columns MID (+ x w)))
	       (BOT (drop-rows original (+ y h))))
    (append-rows 
     TOP
     (append-columns
      LEFT
      (rect-map (lambda (old new)
		  (if (eq? new '?) old new))
		ORIG
		replacement)
      RIGHT)
     BOT)))

(e.g. (replace-subrect '((a b c d e)
			 (f g h i j)
			 (k l m n o)
			 (p q r s t)) '((X Y)
					(Z ?)) 2 1) ===> ((a b c d e)
							  (f g X Y j)
							  (k l Z n o)
							  (p q r s t)))

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
#|
(match `(,(subrect-indices source `((,figure)))
	 ,(subrect-indices dest `((,figure))))
  ((or (() _) (_ ()))
   #f)
  ((((xs ys)) ((xd yd)))
   `(,(- xd xs) ,(- yd yd)))
  (else
   (error "non-unique source or dest")))
|#

(e.g. (displacement #;of '♞ #;from '((_ ?)
				     (? ?)
				     (? ♞))  #;to '((♞ ?)
						    (? ?)
						    (? _))) ===> (-1 -2))

;; the procedures in the following section are used during
;; the parsing of game's rules

(define (transpose rect)
  (apply map list rect))

(e.g. (transpose '((a b c)
		   (d e f))) ===> ((a d)
				   (b e)
				   (c f)))

(observation: 
 (for-all <rect> (equal? <rect> (transpose (transpose <rect>)))))

(with-default ((rotate-item-left identity))
  (define (rotate-left rect)
    (reverse (transpose (tree-map (specific rotate-item-left) rect)))))

(e.g. (rotate-left '((a b c)
		     (d e f))) ===> ((c f)
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

(e.g. (rotate-right '((a b c)
		      (d e f))) ===> ((d a)
				      (e b)
				      (f c)))

(prototype (all-rotations <rect>) -> (<rect> <rect> <rect> <rect>))

(define (all-rotations rect)
  (map (lambda(n)((iterations n rotate-left) rect)) (iota 4)))

;; Suppose that we have an element of a rect at coordinates (x y).
;; The example below shows the element X at initial position (1 1).
;; We would like to obtain the coordinates of the element X on
;; a rotated board.

(e.g. 
 (all-rotations '((_ _ _ _ _)
		  (_ X _ _ _)
		  (_ _ _ _ _)
		  (_ _ _ _ _)))
 same-set?
 '(((_ _ _ _ _) 
    (_ X _ _ _) 
    (_ _ _ _ _) 
    (_ _ _ _ _)) 
   #;=========
   ((_ _ _ _) 
    (_ _ _ _) 
    (_ _ _ _) 
    (_ X _ _) 
    (_ _ _ _))
   #;=========
   ((_ _ _ _ _) 
    (_ _ _ _ _) 
    (_ _ _ X _) 
    (_ _ _ _ _)) 
   #;=========
   ((_ _ _ _) 
    (_ _ X _) 
    (_ _ _ _) 
    (_ _ _ _) 
    (_ _ _ _))))

;; Clearly, the transformed coordinates depend on the size of a given rect:

(e.g.
 (append-map (lambda(rect)
	       (subrect-indices rect '((X))))
	     (all-rotations '((_ _ _ _ _)
			      (_ X _ _ _)
			      (_ _ _ _ _)
			      (_ _ _ _ _))))
 same-set?
 '((1 1) (1 3) (3 2) (2 1)))

;; where of course

(e.g. (rect-size '((_ _ _ _ _)
		   (_ X _ _ _)
		   (_ _ _ _ _)
		   (_ _ _ _ _))) ===> (5 4))

(observation:
 (for-all <rect>
     (let ((x <integer>) (y <integer>))
       (if (and (<= 0 x (- (rect-width <rect>) 1))
		(<= 0 y (- (rect-height <rect>) 1)))
	   (let ((R* (rotate-left <rect>))
		 (x* y)
		 (y* (- (rect-height <rect>) x 1)))
	     (equal? (take-from-rect <rect> x y)
		     (take-from-rect R* x* y*)))))))

;; and hence

(define (rotate-rect-coords-left x y w h)
  `(,y ,(- w x 1)))

(e.g. (rotate-rect-coords-left 1 1 5 4) ===> (1 3))

(define (rotate-rect-coords-right x y w h)
  `(,(- h y 1) ,x))

(e.g. (rotate-rect-coords-right 1 1 5 4) ===> (2 1))

;; likewise

(with-default ((flip-item-horizontally identity))
  (define (flip-horizontally rect)
    (map reverse (tree-map (specific flip-item-horizontally) rect))))

(e.g. (flip-horizontally '((a b)
			   (c d)
			   (e f))) ===> ((b a)
					 (d c)
					 (f e)))

(with-default ((flip-item-vertically identity))
  (define (flip-vertically rect)
    (reverse (rect-map (specific flip-item-vertically) rect))))

(e.g. (flip-vertically '((a b)
			 (c d)
			 (e f))) ===> ((e f)
				       (c d)
				       (a b)))

(define (flip-rect-coords x y w h)
  `(,(- w x 1) ,(- h y 1)))

(e.g. (flip-rect-coords 1 1 5 4) ===> (3 2))

(observation:
 (for-all (x y w h) in <integer>
   (if (and (<= 0 x (- w 1))
	    (<= 0 y (- h 1)))
       (equal?
	(match-let (((x* y*) (rotate-rect-coords-left x y w h)))
	  (rotate-rect-coords-left x* y* h w))
	,(rotate-rect-coords-right ,@(rotate-rect-coords-right x y w h) h w)
	(flip-rect-coords x y w h)
	))))

#;(let ((x 1) (y 1) (w 5) (h 4))
  (equal?
   (match-let (((x* y*) (rotate-rect-coords-left x y w h)))
     (rotate-rect-coords-left x* y* h w))
   ,(rotate-rect-coords-right ,@(rotate-rect-coords-right x y w h) h w)
   (flip-rect-coords x y w h)))

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

(e.g. (with-context-for-arrows
       (rotate-right '((↖ ↑ ↗)
		       (↙ ↓ ↘)))) ===> ((↖ ↗)
					(← →)
					(↙ ↘)))

(publish
 (define (complements state-description board-width board-height)
   (let ((arrow-positions (subrect-indices state-description
					   '(((← ↑ → ↓ ↖ ↗ ↘ ↙)))))
	 (D state-description) (W board-width) (H board-height)
	 (w (rect-width state-description)) (h (rect-height state-description)))
     (match arrow-positions
       (()
	(list D))
       (((x y) . rest)
	(let ((arrow (take-from-rect D x y))
	      (flip (iterations 2 rotate-left))
	      (actual-complements (if (null? rest) 
				      proper-complements
				      recursive-complements)))
	  (with-context-for-arrows
	   (case arrow
	     ((→ ↗) 
	      (twisted actual-complements identity identity arrow D 
		       (list x y) W H))
	     ((↑ ↖)
	      (twisted actual-complements rotate-right rotate-left arrow D
		       (rotate-rect-coords-right x y w h) H W))
	     ((← ↙)
	      (twisted actual-complements flip flip arrow D 
		       (flip-rect-coords x y w h) W H))
	     ((↓ ↘)
	      (twisted actual-complements rotate-left rotate-right arrow D
		       (rotate-rect-coords-left x y w h) H W))
	     ))))
       (else
	(error "No match: " arrow-positions (current-source-location))))))
 where
 (define (twisted actual-complements transform-there transform-back arrow 
		  state-description (x y) board-width board-height)
   (map transform-back
	((actual-complements #;for arrow) (transform-there state-description)
	 x y board-width board-height)))
 (define (proper-complements #;for arrow)
   (if (in? arrow '(← ↑ → ↓))
       right-complements
       top-right-complements))
 (define (recursive-complements #;for arrow)
   (if (in? arrow '(← ↑ → ↓))
       recursive-right-complements
       (throw 'not-implemented)))
 (define (recursive-right-complements D x -y- W H)
   (match-let (((((left ... repeat-column) 
		  #;-        ...          )  arrow-column right)
		(split-rect-vertically D x)))
     (let ((right-complements (complements right W H))
	   (left-complements (complements left W H)))
       (filter (lambda(state)
		 (and (<= (rect-width state) W)
		      (<= (rect-height state) H)))
	       (append-map (lambda (state)
			     (complements state W H))
			   (map (lambda ((left right))
				  (map append left `(,repeat-column)
				       arrow-column right))
				(cart left-complements 
				      right-complements)))))))
 (define (right-complements D x y W -H-)
   (let ((w (rect-width D))
	 (h (rect-height D)))
     (match-let ((((filling-column) 
		   #;-    ...      ) (take-subrect D (- x 1) y 1 h))
		 (suffix (take-subrect D (+ x 1) 0 (- w x 1) h))
		 (prefix (take-subrect D 0 0 (- x 1) h)))
       (map (lambda (n)
	      (map append prefix
		   (if (zero? n)
		       (make-list h '())
		       (transpose (make-list n filling-column)))
		   suffix))
	    (iota (- W w -3))))))
 (define (split-rect-vertically rect col)
   (transpose 
    (map (lambda(row)
	   (let-values (((left right) (split-at row col)))
	     `(,left ,(take right 1) ,(drop right 1))))
	 rect)))
 (e.g.
  (split-rect-vertically '((a b c d e)
			   (f g h i j)
			   (k l m n o)) 2) 
  ===> (((a b)
	 (f g)
	 (k l)) ((c)
		 (h)
		 (m)) ((d e)
		       (i j)
		       (n o))))
 (define (split-rect-horizontally rect row)
   (let-values (((top bottom) (split-at rect row)))
     `(,top ,(take bottom 1) ,(drop bottom 1))))
 (e.g.
  (split-rect-horizontally '((a b c)
			     (d e f)
			     (g h i)
			     (j k l)
			     (m n o)) 2) 
  ===> (((a b c)
	 (d e f)) ((g h i)) ((j k l)
			     (m n o))))
 (define (slice-rect rect x y)
   ;; the column (x *) and the row (* y) are dismissed
   (let ((w (rect-width rect))
	 (h (rect-height rect)))
     (let ((top-left (upper-left-corner rect x y))
	   (top-center (take-subrect rect x 0 1 y))
	   (top-right (upper-right-corner rect (- w x 1) y))
	   (middle-left (take-subrect rect 0 y x 1))
	   (middle-center (take-subrect rect x y 1 1))
	   (middle-right (take-subrect rect (+ x 1) y (- w x 1) 1))
	   (bottom-left (lower-left-corner rect x (- h y 1)))
	   (bottom-center (take-subrect rect x (+ y 1) 1 (- h y 1)))
	   (bottom-right (lower-right-corner rect (- w x 1) (- h y 1))))
       `((,top-left      ,top-center     ,top-right)
	 (,middle-left ,middle-center ,middle-right)
	 (,bottom-left ,bottom-center ,bottom-right)))))
 (e.g. 
  (slice-rect '((a b c d e)
		(f g h i j)
		(k l m n o)) 2 1)
  ===> ((((a b))   ((c))   ((d e)))
	(((f g))   ((h))   ((i j)))
	(((k l))   ((m))   ((n o)))))
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
 (define (top-right-complements D x y W H)
   (match-let* (([(((TL ... tc) 
		    #;- ...    ) _      TR   )
		  (     _        _      _    )
		  (((ml ... mc)
		    (BL ... bc)
		    #;- ...    ) _     (mr 
					BR 
					...))]  (slice-rect D x y))
		((_ ... ml*) ml)
		((bc* _ ...) bc))
     ;; Maybe some day some brilliant hacker will figure out the way to
     ;; write it like that, but until then we're doomed to analyse all those
     ;; lists, maps, appends and applies below.
     ;;
     ;; (sequence (_)
     ;; ((TL TR)
     ;;  (BL BR))  ((TL tc TR)
     ;;             (ml mc mr)
     ;;             (BL bc BR))  ((TL tc  tc  TR)
     ;;                           (ml ml* mc  mr)
     ;; 			  (ml mc  bc* mr)
     ;; 			  (BL bc  bc  BR))  ((TL  tc  tc  tc  TR)
     ;; 					     (ml  ml* ml* mc  mr)
     ;; 					     (ml  ml* mc  bc* mr)
     ;; 					     (ml  mc  bc* bc* mr)
     ;; 					     (BL  bc  bc  bc  BR))
     ;; 				...
     ;;  (until (or (= (rect-height _) H)
     ;; 	    (= (rect-width _) W))))
     (map (lambda (n)
	    (append
	     (map append TL 
		  (apply map append 
			 (if (zero? n) 
			     (list (make-list (length TL) '()))
			     (make-list n (map list tc)))) 
		  TR)
	     (map append (make-list n ml)
		  (diagonal n ml* mc bc*)
		  (make-list n mr))
	     (map append BL 
		  (apply map append 
			 (if (zero? n)
			     (list (make-list (length BL) '()))
			     (make-list n (map list bc)))) 
		  BR)))
	  (iota (+ 3 (min (- W (rect-width D))
			  (- H (rect-height D))))))))
 ) ;D publish complements

(e.g.                                   ; that one uses right-complements
 (complements '((♜ _ → □/_)) 6 1)
 same-set?
 '(((♜ □/_))
   ((♜ _ □/_))
   ((♜ _ _ □/_))
   ((♜ _ _ _ □/_))
   ((♜ _ _ _ _ □/_))))

(e.g.                                   ; should work for multi-dimensional 
 (complements '((x _ → y)               ; cases as well (but only one arrow
		(z _ … v)) 4 2)         ; per row/column may appear)
 same-set?
 '(((x y) 
    (z v)) 
   ((x _ y) 
    (z _ v)) 
   ((x _ _ y) 
    (z _ _ v)) ))

(e.g.                                   ; it is also possible to use multiple 
 (complements '((a _ → b _ → c)) 6 1)   ; arrows (but currently not the 
 same-set?                              ; slanting ones). It is an error, if 
 '(((a b c))                            ; an arrow appears in the same row 
   ((a b _ c))                          ; (in case of vertical arrows) or column
   ((a b _ _ c))                        ; (in case of horizontal arrows) or in 
   ((a b _ _ _ c))                      ; the row or column that will be 
   ((a _ b c))                          ; repeated, and the result of calling
   ((a _ b _ c))                        ; complements with such argument is 
   ((a _ b _ _ c))                      ; unspecified, and it is possible that
   ((a _ _ b c))                        ; the function will never return,
   ((a _ _ b _ c))                      ; having fallen into an infinite loop
   ((a _ _ _ b c))))

(e.g.                                   ; that one uses top-right-complements
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
      (lambda ((initial-state final-state . extra))
	(match (subrect-indices initial-state `((,figure)))
	  (((dx dy))
	   (and 
	    (not (null? (filter 
			 (equals? `(,(- x dx) ,(- y dy)))
			 (subrect-indices board/rect initial-state))))
	    `(,initial-state ,final-state . ,extra)))))
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
