(use-modules (slayer)
	     (slayer image)
	     (extra common)
	     (extra ref)
	     (ice-9 threads)
	     (oop goops)
	     (widgets base))

(use-modules (extra common) (extra ref) (oop goops) (ice-9 q))

(define click 'mouse-left-down)

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


(define (gameplay board)
  (let turn ((n 0) (player (initial-player board)))
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
