#!../src/slayer -d3d
exit
!#
(use-modules (slayer) (slayer image) (extra slayer) (extra common) (extra ref)
	     (widgets base) (schess simple-game))

(keydn 'esc quit)

(set! %load-path (append '("." "./guile-modules" ".." "../guile-modules") 
			 %load-path))

;; (use-modules (oop goops)(extra common)(extra ref))

(publish
 (define *game* (make <simple-board-game> #:rule-book rule-book
		      #:field-decorator  
		      (lambda (x y) 
			(if (= 0 (modulo (+ x y) 2))
			    empty-field
			    empty-field*))
		      #:image-transformer
		      (lambda (image)
			(subtract-image image empty-field))
		      #:resize
		      (lambda (w h . _)
			(set-screen-size! w h))))
 where
 (define rule-book (if (defined? '$1) $1 "chess.ss"))
 (define empty-field (load-image "art/chess/b.png"))
 (define empty-field* (load-image "art/chess/w.png")))



(set-window-title! (string-append "SCHESS: "
				  (symbol->string
				   #[*game* : 'rules : 'game-name])))

(add-child! *stage* *game*)

(start-gameplay *game*)
