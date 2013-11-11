#! /bin/sh
./slayer -d3d -i$0
exit
!#
(use-modules (slayer) (slayer image) (extra slayer) (extra common)
	     (widgets base) (widgets bitmap) (schess interface))

(keydn 'esc quit)

(define *board* (load-board-game "ard-ri.scm"))

(add-child! *stage* *board*)

(start-gameplay *board*)

  
