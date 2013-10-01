#! /bin/sh
./slayer -d3d -i$0
exit
!#

(use-modules (slayer) (slayer image) (extra slayer)
	     (widgets base) (widgets bitmap) (schess interface))

#;(use-modules (oop goops) 
	     (extra common) (extra ref)
	     (schess elements)
	     (schess interface))

(keydn 'esc quit)

;; DO ZROBIENIA:
;; - wczytywanie i przetwarzanie danych z pliku z regułami ~~
;; - ładowanie planszy ("initial board"), w szczególności
;;   wyświetlanie wszystkich rodzajów figur ~~
;; - zrobienie, żeby przy kliknięciu podświetlały się
;;   dopuszczalne pola

(define b (load-board-game "chess.scm"))

#;(define ((employ symmetries) state-description)
  (list state-description))

#|
((employ '(all-rotations)) state-description)
=> (all-rotations state-description)

((employ '(all-rotations (all-rotations flip-horizontal))) state-description)
=> (append (all-rotations state-description)
	   (all-rotations (flip-horizontal state-description)))
|#

(keydn 's (lambda()(pretty-print (current-board-state/array b))))

(add-child! *stage* b)
