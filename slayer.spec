(keydn 'esc
  (lambda (type state code name mod unicode)
    (quit)))

(add-child! *stage* (make-image (load-image "./tk4d.png") 50 50))
(add-child! *stage* (make-image (render-text "the game" *default-font*) 150 150))
(let ((ku (make-image (load-image "./ku.png") 50 150)))
  (make-timer 100 (lambda()
		    (slot-set! ku 'x (+ (slot-ref ku 'x) 1))
		    (slot-set! ku 'y (+ (slot-ref ku 'y) 1)))
  (add-child! *stage* ku)))


(let* ((t (make <text-area>))
       (put-string (lambda(s)
	    (let ((p (slot-ref t 'port)))
	      (let ((row (port-line p))
		    (col (port-column p)))
		(let ((line (vector-ref (slot-ref t 'lines) row)))
		  (vector-set! (slot-ref t 'lines) row
			       (string-append
				(substring line 0 col)
				s
				(substring line col)))))))))
  (slot-set! t 'lines '#("wielki test" "numer" "jeden"))
  (slot-set! t 'port (make-soft-port
		      (vector
		       (lambda(c) (put-string (list->string (list c))))
		       put-string
		       (lambda()
			 (for-each display 
				   (vector->list (slot-ref t 'lines))))
		       #f
		       #f) "w"))
  (let* ((special-keys (slot-ref t 'special-keys))
	 (set-key! (lambda (key action)
		    (vector-set! special-keys (hash-ref *scancodes* key) action))))
    (set-key! "esc" (lambda()
		      (set-current-output-port *stdout*)
		      (input-mode 'direct)))
    (set-key! "f1" (lambda()
		     (display 
		      (list (port-line (current-output-port))(port-column (current-output-port))) 
		      *stdout*)
		     (newline *stdout*)))

    (set-key! "f2" (lambda()(with-output-to-port *stdout* (lambda()(display "chuj")(newline)))))
    (set-key! "backspace" (lambda()
			    (let* ((w *input-widget*)
				   (p (slot-ref w 'port))) 
			      (delete-char! w) 
			      (move-cursor! w -1 0))))
    (set-key! "delete" (lambda()
			 (let* ((w *input-widget*)
				(p (slot-ref w 'port))) 
			   (delete-char! w (+ (port-column p) 1) (port-line p)) 
			   (move-cursor! w 0 0)))))
    
  (slot-set! t 'click 
	     (lambda e
	       (set-current-output-port (slot-ref t 'port))
	       (set! *input-widget* t)
	       (input-mode 'typing)))
  (add-child! *stage* t))

;(set! hash{z} 20)

(keydn 'mouse1 
  (lambda (type name state x y)
    (and-let* ((w (widget-nested-find (lambda(w)
					(in-area? (list x y) (area w)))
				      *stage*)))
	      ;(display `(grabbing ,w with children at ,(map area (slot-ref w 'children))))(newline)
	      (set! *active-widget* w)
	      ((slot-ref w 'click) type name state x y))))

(keyup 'mouse1 (lambda (type name state x y) (set! *active-widget* *stage*)))

(mousemove (lambda (type state x y xrel yrel) 
	     ((slot-ref *active-widget* 'drag) type state x y xrel yrel)))

(keydn 'e
  (lambda e (display e)
	  (newline)))

(set-caption! "*SLAYER*")


(keydn 't (lambda e (input-mode 'typing)))

;(make-timer 1000 (lambda()(display "hello")) ) 


