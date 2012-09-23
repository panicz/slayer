(keydn 'esc
  (lambda (type state code name mod unicode)
    (quit)))

(keydn 'v
       (lambda e
	 (display (version)) (newline)))

(add-child! *stage* (make-image (load-image "./tk4d.png") 50 50))
(add-child! *stage* (make-image (render-text "the game" *default-font* #x000000 #xccaa22) 150 150))

(add-child! *stage* (make-image (rectangle 50 50 #x20eeaa22) 50 50))

;; (let ((ku (make-image (load-image "./ku.png")  50 150)))
;;   (make-timer 100 (lambda()
;; 		    (set! #[ ku 'x ] (+ #[ ku 'x ] 1))
;; 		    (set! #[ ku 'y ] (+ #[ ku 'y ] 1));;   (add-child! *stage* ku)))


;; (with-input-from-string "read" read)
    
(define (last-sexp-starting-position str)
  (define opening-braces '(#\( #\[))
  (define closing-braces '(#\) #\]))
  (define braces (append opening-braces closing-braces))
  (define* (rewind #:key string while starting-from)
    (let loop ((pos starting-from))
      (if (and (>= pos 0) (while #[ string pos ]))
	  (loop (1- pos))
	  pos)))
  (define (last-symbol-starting-position str init-pos)
    (rewind #:string str #:starting-from init-pos 
	    #:while (lambda(c)(and (not (char-whitespace? c)) (not (in? c braces))))))
  (define (last-whitespaces-starting-position str init-pos)
    (rewind #:string str #:starting-from init-pos #:while char-whitespace?))
  (define (last-string-starting-position str init-pos)
    (let loop ((pos init-pos))
      (if (and (eq? #[ str pos ] #\")
	       (or (= pos 0) (not (eq? #[ str (- pos 1) ] #\\))))
	  pos
	  (loop (1- pos)))))
  (let eat ((pos (- (string-length str) 1))
	    (level 0))
    (cond ((char-whitespace? #[ str pos ])
	   (eat (last-whitespaces-starting-position str pos) level))
	  ((eq? #[ str pos ] #\")
	   (if (= level 0)
	       (+ (last-string-starting-position str (- pos 1)) 1)
	       (eat (- (last-string-starting-position str (- pos 1)) 1) level)))
	  ((not (in? #[ str pos ] braces))
	   (if (= level 0)
	       (+ (last-symbol-starting-position str pos) 1)
	       (eat (last-symbol-starting-position str pos) level)))
	  ((in? #[ str pos ] closing-braces)
	   (eat (- pos 1) (+ level 1)))
	  ((in? #[ str pos ] opening-braces)
	   (cond ((= level 1)
		  (if (and (> pos 0)
			   (not (char-whitespace? #[ str (- pos 1) ]))
			   (not (in? #[ str (- pos 1) ] braces)))
		      (let ((pos* (+ (last-symbol-starting-position str (- pos 1)) 1)))
			(if (and (in? #[ str pos* ] '(#\# #\'))
				 (not (eq? #[ str (+ pos* 1)] #\:)))
			    pos*
			    pos))
		      pos))
		 ((> level 1)
		  (eat (- pos 1) (- level 1)))
		 (#t
		  (error "mismatch braces")))))))


(let* ((t (make <text-area>))
       (put-string (lambda(s)
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
  (slot-set! t 'port (make-soft-port
		      (vector
		       (lambda(c) (put-string (list->string (list c))))
		       put-string
		       (lambda()
			 (for-each display 
				   (vector->list (slot-ref t 'lines))))
		       #f
		       #f) "w"))
  (let* ((set-key! (lambda (key action)
		     (set! #[ #[ t 'special-keys ] #[ *scancodes* key ] ] action))))
    (set-key! "esc" (lambda()
		      (set-current-output-port *stdout*)
		      (input-mode 'direct)))
    (set-key! "return"
	      (lambda ()
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
    (set-key! "left" (lambda()
		       (let ((line (port-line #[ t 'port ]))
			     (column (port-column #[ t 'port ])))
			 (if (and (= column 0)
				  (> line 0))
			     (move-cursor! t (string-length #[ #[ t 'lines ] (- line 1) ]) -1)
			     ;;else
			     (move-cursor! t -1 0)))))
    (set-key! "right" (lambda()
			(let ((line (port-line #[ t 'port ]))
			      (column (port-column #[ t 'port ])))
			  (if (and (= column (string-length #[ #[ t 'lines ] line ]))
				   (< (+ line 1) (vector-length #[ t 'lines ])))
			      (move-cursor! t (- (string-length #[ #[ t 'lines ] line ])) 1)
			      ;;else
			      (move-cursor! t 1 0)))))
    (set-key! "up" (lambda()(move-cursor! t 0 -1)))
    (set-key! "down" (lambda()(move-cursor! t 0 1)))

    (set-key! "f1" (lambda()
		     (let* ((lines (vector->list #[ t 'lines ]))
			    (line (port-line #[ t 'port ]))
			    (column (port-column #[ t 'port ]))
			    (text (string-join (append (take lines line)
						       (list (substring #[ #[ t 'lines ] line ] 0 column)))
					       "\n"))
			    (last-sexp (substring text (last-sexp-starting-position text))))
		       (display (eval-string last-sexp) *stdout*))))

    (set-key! "backspace" (lambda()
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
    (set-key! "delete" (lambda()
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
	     (lambda e
	       (set-current-output-port #[ t 'port ])
	       (set! *input-widget* t)
	       (input-mode 'typing)))
  (add-child! *stage* t))

(keydn 'mouse1 
  (lambda (type name state x y)
    (and-let* ((w (widget-nested-find (lambda(w)
					(in-area? (list x y) (area w)))
				      *stage*)))
	      ;(display `(grabbing ,w with children at ,(map area (slot-ref w 'children))))(newline)
	      (set! *active-widget* w)
	      (#[ w 'click ] type name state x y))))

(keyup 'mouse1 (lambda (type name state x y) (set! *active-widget* *stage*)))

(mousemove (lambda (type state x y xrel yrel) 
	     (#[ *active-widget* 'drag ] type state x y xrel yrel)))

(keydn 'e
  (lambda e (display e)
	  (newline)))

(set-caption! "*SLAYER*")


(keydn 't (lambda e (input-mode 'typing)))

;(make-timer 1000 (lambda()(display "hello")) ) 

