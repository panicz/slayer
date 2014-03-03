#!../src/slayer -d3d
exit
!#
(use-modules (slayer) (slayer image) (extra slayer) (extra common) (extra ref)
	     (widgets base) (widgets sprite) (schess network-client))

(keydn 'esc quit)

(publish
 (define *game* (make <board-network-client>
		  #:address "127.0.0.1" #:port 7777
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
 (define empty-field (load-image "art/chess/b.png"))
 (define empty-field* (load-image "art/chess/w.png")))

(set-window-title! "SCHESS (network)")

(keydn 
 'p
 (lambda ()
   (request *game* '(my-player)
	    (lambda(player)
	      (format #t "my player is ~a\n" player)))))

(add-child! *game* #;to *stage*)
