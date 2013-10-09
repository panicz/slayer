(use-modules (slayer)
	     (slayer image)
	     (extra common)
	     (extra ref)
	     (extra threads)
	     (oop goops)
	     (widgets base))

(define click 'mouse-left-down)



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


