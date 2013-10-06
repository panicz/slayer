#! /bin/sh
./slayer -d3d -i$0
exit
!#
(use-modules (slayer) (slayer image) (extra slayer)
	     (widgets base) (widgets bitmap) (schess interface))

(keydn 'esc quit)

(define *board* (load-board-game "chess.scm"))

(add-child! *stage* *board*)

(call-with-new-thread (lambda()(gameplay *board*)))
