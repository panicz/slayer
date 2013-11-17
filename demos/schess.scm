#!../src/slayer -d3d
exit
!#
(use-modules (slayer) (slayer image) (extra slayer) (extra common)
	     (widgets base) (widgets bitmap) (schess interface))

(keydn 'esc quit)

(define rule-book (if (defined? '$1) $1 "chess.lsp"))

(define *board* (load-board-game rule-book))

(add-child! *stage* *board*)

(start-gameplay *board*)
