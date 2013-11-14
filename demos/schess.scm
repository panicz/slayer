#!./slayer -d3d
exit
!#
(use-modules (slayer) (slayer image) (extra slayer) (extra common)
	     (widgets base) (widgets bitmap) (schess interface))

(keydn 'esc quit)

(define *board* (load-board-game "chess.scm"))

(add-child! *stage* *board*)

(start-gameplay *board*)

  
