
(keydn 'esc
       (function (type state code name mod unicode)
	 (quit)))

(keydn 'v
       (function e
	 (display (version)) (newline)))

(define ku (load-image "./ku.png"))

(add-child! *stage* (make-image ku 50 50))
(add-child! *stage* (make-image (render-text "the game" *default-font* #x000000 #xccaa22) 150 150))

(add-child! *stage* (make-image (rectangle 50 50 #x20eeaa22) 50 50))

;; (let ((ku (make-image (load-image "./ku.png")  50 150)))
;;   (make-timer 100 (lambda()
;; 		    (set! #[ ku 'x ] (+ #[ ku 'x ] 1))
;; 		    (set! #[ ku 'y ] (+ #[ ku 'y ] 1));;   (add-child! *stage* ku)))


;; (with-input-from-string "read" read)


(let* ((t (make <text-area>))
       (put-string (function(s)
		     (let ((p #[ t 'port ]))
		       (let ((row (port-line p))
			     (col (port-column p)))
			 (let ((line #[ #[ t 'lines ] row ]))
			   (set! #[ #[ t 'lines ] row ]
				 (string-append
				  (substring line 0 col)
				  s
				  (substring line col)))))))))
  (slot-set! t 'lines '#("wielki test" "numer" "jeden"))
  (slot-set! t 'port (make-soft-port*
		      (vector
		       (function(c) (put-string (list->string (list c))))
		       put-string
		       (function()
			 (for-each display 
				   (vector->list (slot-ref t 'lines))))
		       #f
		       #f) "w"))
  (let* ((set-key! (function (key action)
		     (set! #[ #[ t 'special-keys ] #[ *scancodes* key ] ] action))))
    (set-key! "esc" (function()
		      (set-current-output-port *stdout*)
		      (input-mode 'direct)))
    (set-key! "return"
	      (function ()
		(let ((line #[ #[ t 'lines ] (port-line #[ t 'port ]) ])
		      (line-number (port-line #[ t 'port ]))
		      (lines (vector->list #[ t 'lines ]))
		      (column (port-column #[ t 'port ])))
		  (let ((left (substring line 0 column))
			(right (substring line column)))
		    (set! #[ t 'lines ]
			  (list->vector
			   (append (take lines line-number)
				   (list left right)
				   (drop lines (+ line-number 1)))))
		    (move-cursor! t (- (port-column #[ t 'port ])) 1)))))
    (set-key! "left" (function()
		       (let ((line (port-line #[ t 'port ]))
			     (column (port-column #[ t 'port ])))
			 (if (and (= column 0)
				  (> line 0))
			     (move-cursor! t (string-length #[ #[ t 'lines ] (- line 1) ]) -1)
			     ;;else
			     (move-cursor! t -1 0)))))
    (set-key! "right" (function()
			(let ((line (port-line #[ t 'port ]))
			      (column (port-column #[ t 'port ])))
			  (if (and (= column (string-length #[ #[ t 'lines ] line ]))
				   (< (+ line 1) (vector-length #[ t 'lines ])))
			      (move-cursor! t (- (string-length #[ #[ t 'lines ] line ])) 1)
			      ;;else
			      (move-cursor! t 1 0)))))
    (set-key! "up" (function()(move-cursor! t 0 -1)))
    (set-key! "down" (function()(move-cursor! t 0 1)))

    (set-key! "f1" (function()
		     (let* ((lines (vector->list #[ t 'lines ]))
			    (line (port-line #[ t 'port ]))
			    (column (port-column #[ t 'port ]))
			    (text (string-join (append (take lines line)
						       (list (substring #[ #[ t 'lines ] line ] 0 column)))
					       "\n"))
			    (last-sexp (substring text (last-sexp-starting-position text))))
		       (display (eval-string last-sexp) *stdout*))))

    (set-key! "backspace" (function()
			    (let ((p #[ t 'port ])
				  (lines #[ t 'lines ]))
			      (if (= (port-column p) 0)
				  (let*-values (((pre post) (split-at (vector->list lines) (port-line p))))
				    (if (> (length pre) 0)
					(let ((this (last pre))
					      (pre (drop-right pre 1)))
					  (match post ((next . rest)
						       (set! #[ t 'lines ] 
							     (list->vector 
							      (append pre 
								      `(,(string-append this next)) 
								      rest)))
						       (move-cursor! t 
								     (string-length #[ lines(-(port-line p)1) ])
								     -1))))))
				  (begin 
				    (delete-char! t)
				    (move-cursor! t -1 0))))))
`
    (set-key! "delete" (function()
			 (let* ((p #[ t 'port ])
				(lines #[ t 'lines ]))
			   (if (= (port-column p) (string-length #[ lines (port-line p) ]))
			       (let*-values (((pre post) (split-at (vector->list lines) (+ (port-line p) 1))))
				 (if (> (length pre) 0)
				     (let ((this (last pre))
					   (pre (drop-right pre 1)))
				       (match post ((next . rest)
						    (set! #[ t 'lines ]
							  (list->vector
							   (append pre
								   `(,(string-append this next))
								   rest))))))))
			       (begin
				 (delete-char! t (+ (port-column p) 1) (port-line p)) 
				 (move-cursor! t 0 0)))))))
    
  (slot-set! t 'click 
	     (function e
	       (set-current-output-port #[ t 'port ])
	       (set! *input-widget* t)
	       (input-mode 'typing)))
  (add-child! *stage* t))

(keydn 'mouse1 
  (function (type name state x y)
    (and-let* ((w (widget-nested-find (function(w)
					(in-area? (list x y) (area w)))
				      *stage*)))
	      ;(display `(grabbing ,w with children at ,(map area (slot-ref w 'children))))(newline)
	      (set! *active-widget* w)
	      (#[ w 'click ] type name state x y))))

(keyup 'mouse1 (function (type name state x y) (set! *active-widget* *stage*)))

(mousemove (function (type state x y xrel yrel) 
	     (#[ *active-widget* 'drag ] type state x y xrel yrel)))

(keydn 'e
  (function e (display e)
	  (newline)))

(set-caption! "*SLAYER*")

(keydn 't (function e (input-mode 'typing)))

;(make-timer 1000 (function()(display "hello")) ) 

