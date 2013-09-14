(use-modules (slayer)
	     (slayer image)
	     (extra common)
	     (extra ref)
	     (ice-9 threads)
	     (oop goops)
	     (widgets base))

(use-modules (extra common) (extra ref) (oop goops) (ice-9 q))

;; OPUS MINORUM

(define click 'mouse-left)

(define-class <queue> ()
  (mutex #:init-thunk make-mutex)
  (q #:init-thunk make-q)
  (condition-variable #:init-thunk make-condition-variable))

(define-method (get! #;via (medium <queue>))
  (with-mutex (slot-ref medium 'mutex)
	      (let again ()
		(if (q-empty? (slot-ref medium 'q))
		    (if (wait-condition-variable (slot-ref medium 
							   'condition-variable)
						 (slot-ref medium 'mutex))
			(deq! (slot-ref medium 'q))
			#;else
			(again))
		  #;else
		  (deq! (slot-ref medium 'mutex))))))

(define-method (give! item #;via (medium <queue>))
  (with-mutex (slot-ref medium 'mutex)
	      (enq! (slot-ref medium 'q) item))
  (signal-condition-variable (slot-ref medium 'condition-variable)))

(define (make-queue) (make <queue>))

;; Powiemy, że s jest podtablicą r wtedy, gdy istnieją takie i oraz j,
;; że dla każdego n ∈ [0, (height r)) oraz dla każdego m ∈ [0, (width r)),
;; r[i+n][j+m] = s[n][m]. Wówczas parę (i, j) nazwiemy współrzędnymi
;; podtablicy s w tablicy r.

(define (rect? x)
  (and (list? x)
       (every list? x)
       (apply = (map length x))))

(define (upper-left-corner rect w h)
  (map (lambda (row)
	 (take row w))
       (take rect h)))

(define (lower-right-corner rect w h)
  (map (lambda (row)
	 (take-right row w))
       (take-right rect h)))

(define (upper-right-corner rect w h)
  (map (lambda (row)
	 (take-right row w))
       (take rect h)))

(define (lower-left-corner rect w h)
  (map (lambda (row)
	 (take row w))
       (take-right rect h)))

(define (rect-height rect)
  (length rect))

(define (rect-width rect)
  (or 
   (and-let* ((h (rect-height rect))
	      ((positive? h)))
     (length (first rect)))
   0))

(define (take-subrect rect x y w h)
  (upper-left-corner
   (lower-right-corner rect 
		       (- (rect-width rect) x) 
		       (- (rect-height rect) y))
   w 
   h))

(define (take-from-rect rect x y)
  (match (take-subrect rect x y 1 1)
    (((x)) x)))

(define (rect-match? rect pattern)
  (cond ((atom? pattern)
	 (or (eq? pattern '?)
	     (equal? rect pattern)))
	((atom? rect)
	 (in? rect pattern))
	((and (list? rect) (list? pattern))
	 (every rect-match? rect pattern))
	(else
	 (error "invalid rect or pattern given"))))

(define (subrect-indices rect sub)
  (let ((w (rect-width sub))
	(h (rect-height sub)))
    (filter 
     (match-lambda ((left top)
		    (rect-match? (take-subrect rect left top w h) sub)))
    (cart (iota (- (rect-width rect) w -1)) 
	  (iota (- (rect-height rect) h -1))))))

(define-class <checker> (<widget>)
  (position #:init-keyword #:position)
  (type #:init-keyword #:type) ;; one of (♟ ♜ ♞ ♝ ♛ ♚) or (♙ ♖ ♘ ♗ ♕ ♔)
  )

(define-class <field> (<widget>)
  ...)

(define-class <board> (<widget>)
  (selected-checker #:init-thunk make-queue)
  (current-player #:init-value #f)
  (chosen-destination #:init-thunk make-queue)
  (fields #:init-thunk (lambda()(empty-fields 8 8))
	  #:init-keyword #:fields)
  (history #:init-value '()))

(define (gameplay board)
  (let turn ((n 0) (player 0))
    (let ((checker (choose-checker player board)))
      (move! checker #;to (choose-destination player #;on board)
	     #;on board)
      (if (final? board)
	  (wins! player #;in-round n)
       #;else
	  (turn (1+ n) (switch player))))))

(begin-thread
 (gameplay))

(define-method (choose-checker player (board <board>))
  (set-current-player! player #;for board)
  ;; czekamy, aż na planszy pojawi się jakiś wybór
  (get! (slot-ref board 'selected-checker)))

(define-method (choose-destination player (board <board>))
  (get! (slot-ref board 'chosen-destination)))

;; pozostaje nam do poruszenia kwestia poruszania się. Schematycznie można
;; ją ująć następująco:
;; każdy z graczy może sobie wybrać jedną figurę, którą chce się poruszyć.
;; Kiedy ją wybierze, chcielibyśmy otrzymać listę możliwych do wykonania
;; przez nią ruchów.
;; Możliwe scenariusze są następujące:
;; a) użytkownik wybierze jedno z dostępnych pól. Wówczas
;; aktualnie wybrany pionek zostaje przekazany logice gry poprzez slot
;; `selected-checker', zaś pole, na które chcemy się poruszyć, zostaje
;; przekazane logice zapośrednictwem slotu `chosen-destination'
;; b) użytkownik naciśnie `escape'. Wówczas podpowiedzi dotyczące możliwych
;; ruchów powinny zostać usunięte
;; c) użytkownik kliknie na inny pionek. Wówczas pokazane zostają możliwe
;; ruchy z perspektywy tego pionka, zaś stare podpowiedzi zostają usunięte
;; d) użytkownik kliknie na nielegalne pole. Wówczas nic się nie stanie.

;; czyli tryb wyboru pionka można opisać następująco:

(define-method (enter-checker-selection-mode! player (board <board>))
  (for field in (slot-ref board 'fields)
       (and-let* ((checker (checker #;from field))
		  ((belongs? checker #;to player)))
	 (slot-set! 
	  field click
	  (lambda (x y)
	    (let* ((escape (keydn 'esc))
		   (keydn 'esc 
			  (lambda()
			    (enter-checker-selection-mode! player board)
			    (keydn 'esc escape))))
	      (enter-destination-selection-mode! checker board)))))))

(define-method (enter-destination-selection-mode! (checker <checker>)
						  (board <board>))
  (for field in (reachable-fields #;for checker #;on board)
       (highlight! field)
       (slot-set! 
	field click
	(lambda (x y)
	  (select-destination! #;of checker #;to field #;on board)))))

;; wreszcie dochodzimy do miejsca, w którym komunikujemy się z wątkiem
;; gameplay'u, przekazując do niego wybory pochodzące od interfejsu użytkownika

(define-method (select-destination! (checker <checker>) #;to (field <field>)
				    #;on (board <board>))
  (give! field #;via (slot-ref board 'chosen-destination))
  (give! checker #;via (slot-ref board 'chosen-checker)))

;; do wyjaśnienia pozostaje jeszcze jedna enigma, mianowicie -- skąd możemy
;; wiedzieć, które pola są dostępne dla jakich pionków. Jest to oczywiście
;; sprawa kluczowa, ponieważ to właściwie tutaj zasadza się opis reguł gry
;; szacho-podobnej. Zasadniczo rzecz wygląda następująco:
;; każdy rodzaj figury posiada dozwolone ruchy, które zależą od pozycji tej
;; figury oraz od rozmieszczenia pozostałych figur na planszy.
;; Konkrety będą już zależeć od poszczególnego rodzaju gry.
;; Chcielibyśmy dysponować opisem, który mógłby być użyty przez dwie
;; spośród wzmiankowanych wyżej funkcji, mianowicie przez
;; `fields-reachable-for' oraz `move!'

;; przedstawiono ruchy dla czarnych figur. Ruchy dla figur białych
;; uzyskamy, odbijając reguły względem pionowej osi

;; teraz, kiedy już zapisaliśmy zestaw reguł szachowych przy pomocy schematów,
;; wypadałoby, żebyśmy wytłumaczyli naszemu systemowi, w jaki sposób należy je
;; przetwarzać. zależy nam na tym, żebyśmy dla wybranej bierki mogli otrzymać
;; listę możliwych do wykonania przez nią ruchów.

;; w tym celu musimy zdefiniować sobie adresy na szachownicy

;;   1 2 3 4 5 6 7 8
;; ((♜ ♟ _ _ _ _ ♙ ♖)  A
;;  (♞ ♟ _ _ _ _ ♙ ♘)  B
;;  (♝ ♟ _ _ _ _ ♙ ♗)  C
;;  (♛ ♟ _ _ _ _ ♙ ♕)  D
;;  (♚ ♟ _ _ _ _ ♙ ♔)  E
;;  (♝ ♟ _ _ _ _ ♙ ♗)  F
;;  (♞ ♟ _ _ _ _ ♙ ♘)  G
;;  (♜ ♟ _ _ _ _ ♙ ♖)) H

;; konkretnej, chcielibyśmy na przykład, żeby funkcja
;; possible-moves działała następująco
;; (possible-moves 'C2) ===> (C3 C4)
;; (possible-moves 'B1) ===> (A3 C3)
;; (possible-moves 'A1) ===> ()
;; (possible-moves 'A3) ===> #f

;; jeżeli idzie o implementację, to będzie się ona opierać na zaproponowanym
;; przez nas opisie ruchów. Dla przypomnienia, opis składał się przede wszystkim
;; ze stanu szachownicy przed wykonaniem ruchu, oraz ze stanu szachownicy po
;; jego wykonaniu. Pierwsza część stanowi zatem strukturę (albo rodzinę struktur), 
;; którą musimy odnaleźć w okolicy naszego pionka, zaś druga część koduje ruch
;; naszej figury (albo konsekwentny ruch kilku figur, jak w przypadku roszady)

;; Jeżeli zatem mamy zbiór opisów ruchów możliwych dla danej figury, o postaci,
;; dajmy na to, (figura ((stan-przed stan-po) ...)), to aby wyznaczyć wszystkie
;; ruchy figury w danej sytuacji, spośród wszystkich możliwych ruchów musimy
;; wybrać te, które "pasują" do rozważanej sytuacji [trochę to zdanie brzmi jak
;; masło maślane]

(define (load-rules #;from file)
  ...)

(define *game-rules* (load-board-game-rules "chess.scm"))

(publish 
 (define (field-name->coords name)
   (let ((name-string (symbol->string name)))
     (match (string-matches "^([A-Z])([0-9])$" name-string)
       ((X N)
	`(,(1- (string->number N)) ,(letter->value X))))))
 where
 (define (letter->value letter-as-string)
   (match (string->list letter-as-string)
     ((char)
      (- (char->integer char) (char->integer #\A))))))

(publish
 (define (coords->field-name coords)
   (match coords
     ((x y)
      (symbol (char+ #\A y) (char+ #\1 x)))))
 where
 (define (char+ char n)
   (integer->char (+ (char->integer char) n))))

(define (displacement #;of figure #;from source #;to dest)
  (map - (match (subrect-indices dest `((,figure))) ((x) x))
       (match (subrect-indices source `((,figure))) ((x) x))))

(define (possible-destinations board field allowed-moves)
  (match-let (((x y) (field-name->coords field)))
    (and-let* ((figure (take-from-rect board x y))
	       (moves (assoc-ref allowed-moves figure)))
      (map coords->field-name
	   (unique
	    (filter-map
	     (match-lambda 
		 ((initial-state final-state)
		  (match (subrect-indices initial-state `((,figure)))
		    (((dx dy))
		     (and (not (null? (filter (equals? `(,(- x dx) ,(- y dy)))
					      (subrect-indices board 
							       initial-state))))
			  (map + (list x y) 
			       (displacement #;of figure #;from initial-state
						  #;to final-state)))))))
	     moves))))))

(define (transpose rect)
  (apply map list rect))

(with-default ((rotate-item-left identity))
  (define (rotate-left rect)
    (reverse (transpose (tree-map (specific rotate-item-left) rect)))))

(with-default ((rotate-item-right identity))
  (define (rotate-right rect)
    (transpose (reverse (tree-map (specific rotate-item-right) rect)))))

(with-default ((flip-item-horizontally identity))
  (define (flip-horizontally rect)
    (reverse (tree-map (specific flip-item-horizontally) rect))))

(with-default ((flip-item-vertically identity))
  (define (flip-vertically rect)
    (map reverse (tree-map (specific flip-item-vertically) rect))))

(define (all-rotations rect)
  (map (lambda(n)((iterations n rotate-left) rect)) (iota 4)))

(define (any-direction before after)
  (unique
   (append
    (zip (all-rotations before)
	 (all-rotations after))
    (zip (all-rotations (flip-horizontally before))
	 (all-rotations (flip-horizontally after))))))

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

(define-syntax with-context-for-arrows
  (syntax-rules ()
    ((_ actions ...)
     (specify (#;(transpose-item transpose-arrow)
	       (rotate-item-left rotate-arrow-left)
	       (rotate-item-right rotate-arrow-right)
	       (flip-item-horizontally flip-arrow-horizontally)
	       (flip-item-vertically flip-arrow-vertically))
	      actions ...))))

(publish 
 (define (complements state-description board-width board-height)
   (let ((arrow-position (subrect-indices state-description
					  '(((← ↑ → ↓ ↖ ↗ ↘ ↙))))))
     (match arrow-position
       (((x1 y1) (x2 y2) . _)
	(error "Non-unique repetition symbol!"))
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
					 board-height board-width)))))))))
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
     (match-let (((filling-column) (take-subrect desc (- x 1) y 1 h))
		 (suffix (take-subrect desc (+ x 1) 0 (- w x 1) h))
		 (prefix (take-subrect desc 0 0 (- x 1) h)))
       (map (lambda (n)
	      (map append prefix
		   (if (zero? n)
		       '(())
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
   (match-let* ((((((TL ... tc) ...)     TR )
		  (((lc ... re) 
		    (BL ... bc) ...)    (rc 
					 BR 
					 ...)))  (slice-rect desc x y))
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
 ) ; publish complements

