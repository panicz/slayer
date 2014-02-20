(define-module (widgets text-area)
  #:use-module (oop goops)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (slayer font)
  #:use-module (widgets base)
  #:export (<text-area>
	    make-text-area))

(define-class <text-widget> (<widget>)
  (%space #:init-value #f)
  (%cursor #:init-value #f)
  (%background #:init-value #f)
  (font #:init-value *default-font* #:init-keyword #:font)
  (input-port #:init-value (current-input-port))
  (char-height
   #:allocation #:virtual
   #:slot-ref (lambda (self) (font-line-skip #[self 'font]))
   #:slot-set! noop)
  (char-width
   #:allocation #:virtual
   #:slot-ref (lambda (self) (image-width #[self '%space]))
   #:slot-set! noop)
  (special-keys #:init-thunk 
		(lambda()
		  (make-vector (vector-length *key-names*) noop))))

(define-method (initialize (self <text-widget>) args)
  (next-method)
  (set! #[self '%space] (render-text "_" #[self 'font])))

(define-class <text-area> (<text-widget>)
  (lines #:init-value '#(""))
  (%render-cache #:init-value #f)
  (%thread-results #:init-thunk make-hash-table))

(define-method (initialize (self <text-area>) args)
  (next-method)
  (set! #[self '%cursor] (rectangle 2 #;(image-width #[self '%space]) 
				    #[self 'char-height]
				    #x20eeaa22)))

(define-method (draw (t <text-area>))
  (let* ((font #[t 'font])
	 (line-skip (font-line-skip font))
	 (lines #[t 'lines])
	 (cursor #[t '%cursor])
	 (space #[t '%space]))
    (if (or (not #[t '%render-cache])
	    (not (= (vector-length #[t '%render-cache])
		    (vector-length lines))))
	(set! #[t '%render-cache] (make-vector (vector-length lines) #f)))
    (for n in 0 .. (1- (vector-length lines))
	 (let ((image (or #[t : '%render-cache : n]
			  (render-text #[lines n] font))))
	   (set! #[t : '%render-cache : n] image)
	   (set! #[t 'w] (max #[t 'w] (image-width image)))
	   (draw-image! image 
			(+ (or #[t : 'parent : 'x] 0) 
			   #[t 'x]) 
			(+ (or #[t : 'parent : 'y] 0) 
			   #[t 'y] (* n line-skip)))))
    (if (equal? (current-output-port) #[t 'input-port])
	(draw-image! cursor 
		     (+ #[t 'x] (* (image-width space) 
				   (port-column #[t 'input-port])) )
		     (+ #[t 'y] (* line-skip 
				   (port-line #[t 'input-port])))))
    (set! #[ t 'h ] (* (vector-length lines) line-skip))))

(define-method (last-sexp (t <text-area>))
  (and-let* ((lines (vector->list #[ t 'lines ]))
	     (line (port-line #[ t 'input-port ]))
	     (column (port-column #[ t 'input-port ]))
	     (text (string-join 
		    (append (take lines line)
			    (list (substring 
				   #[t : 'lines : line]
				   0 column)))
		    "\n"))
	     (starting-position (last-sexp-starting-position text)))
    (substring text starting-position)))

(define-method (move-cursor! (w <text-area>)
			     (right <integer>)
			     (down <integer>))
  (set-port-line! #[ w 'input-port ] 
		  (max 0 (min (+ (port-line #[w 'input-port]) down) 
			      (- (vector-length #[w 'lines]) 1))))
  (set-port-column! 
   #[ w 'input-port ] 
   (max 0 (min 
	   (+ (port-column #[w 'input-port]) right)
	   (string-length #[w : 'lines : (port-line #[w 'input-port])])))))

(define-method (delete-char! (w <text-area>))
  (let ((p (slot-ref w 'input-port)))
    (delete-char! w (port-column p) (port-line p))))

(define-method (delete-char! (w <text-area>) 
			     (col <integer>) 
			     (line <integer>))
  (let* ((s #[w : 'lines : line ])
	 (sl (string-length s))
	 (l (substring s 0 (max 0 (min sl (- col 1)))))
	 (r (substring s (min sl col))))
    (set! #[w : 'lines : line] (string-append l r))))

(define* (make-text-area #:key (text "hi! :)\n") (x 0)(y 0))
  (let* ((t (make <text-area>))
	 (put-string (lambda(s)
		       (let ((p #[ t 'input-port ]))
			 (let ((row (port-line p))
			       (col (port-column p)))
			   (set! #[t : '%render-cache : row] #f)
			   (let ((line #[t : 'lines : row ]))
			     (set! #[t : 'lines : row]
				   (string-append
				    (substring line 0 col)
				    s
				    (substring line col)))))))))
    (set! #[t 'lines] (list->vector (string-split text #\newline)))
    (set! #[t 'input-port] (make-soft-port
		      (vector
		       (lambda(c) 
			 (put-string (list->string (list c))))
		       put-string
		       noop
		       #;(lambda()
			 (for-each display 
				   (vector->list #[t 'lines])))
		       #f
		       #f) "w"))
  (let* ((set-key! (lambda (key action)
		     (set! #[#[t 'special-keys] #[*scancodes* key]] 
			   action))))
    (set-key! "esc" (lambda()
		      (set-current-output-port *stdout*)
		      (set-direct-input-mode!)))
    (set-key! 
     "return"
     (lambda ()
       (let ((line #[#[t 'lines] (port-line #[t 'input-port])])
	     (line-number (port-line #[ t 'input-port ]))
	     (lines (vector->list #[ t 'lines ]))
	     (column (port-column #[ t 'input-port ])))
	 (let ((left (substring line 0 column))
	       (right (substring line column)))
	   (set! #[ t 'lines ]
		 (list->vector
		  (append (take lines line-number)
			  (list left right)
			  (drop lines (+ line-number 1)))))
	   (move-cursor! t (- (port-column #[t 'input-port])) 
			 1)))))
    (set-key! 
     "left" 
     (lambda()
       (let ((line (port-line #[ t 'input-port ]))
	     (column (port-column #[ t 'input-port ])))
	 (if (and (= column 0)
		  (> line 0))
	     (move-cursor! 
	      t 
	      (string-length #[#[t 'lines](1- line)]) 
	      -1)
	     ;;else
	     (move-cursor! t -1 0)))))
    (set-key! 
     "right" 
     (lambda()
       (let ((line (port-line #[ t 'input-port ]))
	     (column (port-column #[ t 'input-port ])))
	 (if (and (= column (string-length 
			     #[t : 'lines : line]))
		  (< (+ line 1) 
		     (vector-length #[t 'lines])))
	     (move-cursor! 
	      t 
	      (- (string-length #[t : 'lines : line]))
	      1)
	     ;;else
	     (move-cursor! t 1 0)))))
    (set-key! "up" (lambda()(move-cursor! t 0 -1)))
    (set-key! "down" (lambda()(move-cursor! t 0 1)))
    (set-key! "end" (lambda()
		      (let* ((port #[t 'input-port])
			     (line #[t : 'lines : (port-line port)]))
			(move-cursor! t (- (string-length line) 
					   (port-column port)) 
				      0))))
    (set-key! "home" (lambda()(move-cursor! t (- (port-column #[t 'input-port])) 0)))
    (set-key! 
     "f1" 
     (lambda()
       (call-with-new-thread 
	(lambda()(display (eval-string (last-sexp t)) *stdout*)))))

    (set-key! "f2" (lambda()(display (last-sexp t) *stdout*)))
    (set-key! 
     "backspace" 
     (lambda()
       (set! #[t '%render-cache] #f)
       (let ((p #[ t 'input-port ])
	     (lines #[ t 'lines ]))
	 (if (= (port-column p) 0)
	     (let*-values (((pre post) (split-at (vector->list lines)
						 (port-line p))))
	       (if (> (length pre) 0)
		   (let ((this (last pre))
			 (pre (drop-right pre 1)))
		     (match post 
		       ((next . rest)
			(set! #[ t 'lines ] 
			      (list->vector 
			       (append pre 
				       `(,(string-append this next)) 
				       rest)))
			(move-cursor! 
			 t 
			 (string-length #[lines (- (port-line p) 1)])
			 -1))))))
	     (begin 
	       (delete-char! t)
	       (move-cursor! t -1 0))))))
    (set-key! 
     "delete" 
     (lambda()
       (set! #[t '%render-cache] #f)
       (let* ((p #[ t 'input-port ])
	      (lines #[ t 'lines ]))
	 (if (= (port-column p) 
		(string-length #[ lines (port-line p) ]))
	     (let*-values (((pre post) 
			    (split-at (vector->list lines)
				      (+ (port-line p) 1))))
	       (if (> (length pre) 0)
		   (let ((this (last pre))
			 (pre (drop-right pre 1)))
		     (match post 
		       ((next . rest)
			(set! #[t 'lines]
			      (list->vector
			       (append pre
				       `(,(string-append this next))
				       rest))))))))
	     (begin
	       (delete-char! t (+ (port-column p) 1) (port-line p)) 
	       (move-cursor! t 0 0)))))))
  (set! #[t 'left-mouse-down]
	(lambda e
	  (set-current-output-port #[ t 'input-port ])
	  (set-typing-special-procedure! (lambda(scancode)
					   (#[t : 'special-keys : scancode])))
	  (set-typing-input-mode!)))
  (set! #[t 'x] x)
  (set! #[t 'y] y)
  t))

(define-generic input-text!)
