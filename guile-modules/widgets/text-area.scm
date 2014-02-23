(define-module (widgets text-area)
  #:use-module (oop goops)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (slayer font)
  #:use-module (widgets base)
  #:export (<text-area>
	    )
  #:re-export (make)
  )

(define-class <text-widget> (<widget>)
  (%space #:init-value #f)  ;; 
  (%cursor #:init-value #f) ;; cursor image
  (background-color #:init-value #f #:init-keyword #:background-color)
  (text-color #:init-value #f #:init-keyword #:text-color)
  (font #:init-value *default-font* #:init-keyword #:font)
  (port #:init-value #f)
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
  (set! #[self 'left-click]
	(lambda _
	  (set-current-output-port #[ self 'port ])
	  (set-typing-special-procedure! 
	   (lambda(scancode)
	     (#[self : 'special-keys : scancode])))
	  (set-typing-input-mode!)))
  (set! #[self '%cursor] (rectangle 2 #;(image-width #[self '%space]) 
				    #[self 'char-height]
				    #x20eeaa22))
  (set! #[self '%space] (render-text "_" #[self 'font])))

(define-class <text-area> (<text-widget>)
  (lines #:init-value '#(""))
  (max-lines #:init-value +inf.0 #:init-keyword #:max-lines)
  (%render-cache #:init-value #f)
  (%thread-results #:init-thunk make-hash-table))

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
			  (let ((fresh-image (render-text #[lines n] font)))
			    (set! #[t : '%render-cache : n] fresh-image)
			    fresh-image))))
	   (set! #[t 'w] (max #[t 'w] (image-width image)))
	   (draw-image! image 
			(+ (or #[t : 'parent : 'x] 0) 
			   #[t 'x])
			(+ (or #[t : 'parent : 'y] 0) 
			   #[t 'y] (* n line-skip)))))
    (if (equal? (current-output-port) #[t 'port])
	(draw-image! cursor 
		     (+ #[t 'x] (* (image-width space) 
				   (port-column #[t 'port])) )
		     (+ #[t 'y] (* line-skip 
				   (port-line #[t 'port])))))
    (set! #[ t 'h ] (* (vector-length lines) line-skip))))

(define-method (last-sexp (t <text-area>))
  (and-let* ((lines (vector->list #[ t 'lines ]))
	     (line (port-line #[ t 'port ]))
	     (column (port-column #[ t 'port ]))
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
  (set-port-line! #[ w 'port ] 
		  (max 0 (min (+ (port-line #[w 'port]) down) 
			      (- (vector-length #[w 'lines]) 1))))
  (set-port-column! 
   #[ w 'port ] 
   (max 0 (min 
	   (+ (port-column #[w 'port]) right)
	   (string-length #[w : 'lines : (port-line #[w 'port])])))))

(define-method (delete-char! (w <text-area>))
  (let ((p (slot-ref w 'port)))
    (delete-char! w (port-column p) (port-line p))))

(define-method (delete-char! (w <text-area>) 
			     (col <integer>) 
			     (line <integer>))
  (let* ((s #[w : 'lines : line ])
	 (sl (string-length s))
	 (l (substring s 0 (max 0 (min sl (- col 1)))))
	 (r (substring s (min sl col))))
    (set! #[w : 'lines : line] (string-append l r))))

(define-method (break-line! (t <text-area>))
  (let ((line #[#[t 'lines] (port-line #[t 'port])])
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
      (move-cursor! t (- (port-column #[t 'port])) 
		    1))))

(define-method (leave-typing-mode! (t <text-area>))
  (set-current-output-port *stdout*)
  (set-direct-input-mode!))

(define-method (move-cursor-back! (t <text-area>))
  (let ((line (port-line #[ t 'port ]))
	(column (port-column #[ t 'port ])))
    (if (and (= column 0)
	     (> line 0))
	(move-cursor! 
	 t 
	 (string-length #[#[t 'lines](1- line)]) 
	 -1)
    #;else
	(move-cursor! t -1 0))))

(define-method (move-cursor-forward! (t <text-area>))
  (let ((line (port-line #[ t 'port ]))
	(column (port-column #[ t 'port ])))
    (if (and (= column (string-length 
			#[t : 'lines : line]))
	     (< (+ line 1) 
		(vector-length #[t 'lines])))
	(move-cursor! 
	 t 
	 (- (string-length #[t : 'lines : line]))
	 1)
    #;else
	(move-cursor! t 1 0))))

(define-method (move-cursor-to-the-end-of-line! (t <text-area>))
  (let* ((port #[t 'port])
	 (line #[t : 'lines : (port-line port)]))
    (move-cursor! t (- (string-length line) 
		       (port-column port)) 
		  0)))

(define-method (move-cursor-to-the-beginning-of-line! (t <text-area>))
  (move-cursor! t (- (port-column #[t 'port])) 0))

(define-method (delete-previous-char! (t <text-area>))
  (set! #[t '%render-cache] #f)
  (let ((p #[ t 'port ])
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
    #;else
	(begin 
	  (delete-char! t)
	  (move-cursor! t -1 0)))))

(define-method (delete-next-char! (t <text-area>))
  (set! #[t '%render-cache] #f)
  (let* ((p #[ t 'port ])
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
    #;else
	(begin
	  (delete-char! t (+ (port-column p) 1) (port-line p)) 
	  (move-cursor! t 0 0)))))

(define-method (initialize (t <text-area>) args)
  (next-method)
  (let-keywords args #t ((text "hi! :)\n"))
    (let ((put-string (lambda(s)
			(let ((p #[ t 'port ]))
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
      (set! #[t 'port] (make-soft-port
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
			   (lambda()(action t))))))
	(set-key! "esc" leave-typing-mode!)
	(set-key! "return" break-line!)
	(set-key! "left"  move-cursor-back!)
	(set-key! "right" move-cursor-forward!)
	(set-key! "up" (lambda(t)(move-cursor! t 0 -1)))
	(set-key! "down" (lambda(t)(move-cursor! t 0 1)))
	(set-key! "end" move-cursor-to-the-end-of-line!)
	(set-key! "home" move-cursor-to-the-beginning-of-line!)
	(set-key! 
	 "f1" 
	 (lambda(t)
	   (call-with-new-thread 
	    (lambda()(display (eval-string (last-sexp t)) *stdout*)))))
	(set-key! "f2" (lambda(t)(display (last-sexp t) *stdout*)))
	(set-key! "backspace" delete-previous-char!)
	(set-key! "delete" delete-next-char!)
	))))
