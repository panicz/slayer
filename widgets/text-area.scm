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

(define-class <text-area> (<widget>)
  (lines #:init-value '#(""))
  (special-keys #:init-thunk 
		(lambda()
		  (make-vector (vector-length *key-names*) noop)))
  ;(cursor-position #:init-value '(0 0))
  (font #:init-value *default-font* #:init-keyword #:font)
  (port #:init-value (current-input-port))
  #;(visible-cols #:init-keyword #:visible-cols)
  #;(visible-lines #:init-keyword #:visible-lines)
  #;(rendered-lines #:init-thunk make-hash-table))

(define (render-text-cached text font)
  (define cached-text (make-hash-table))
  (define cached-image (make-hash-table))
  (if (equal? #[cached-text font] text)
      #[cached-image font]
      (let ((image (render-text text font)))
	(set! #[cached-text font] text)
	(set! #[cached-image font] image)
	image)))

(define-method (draw (t <text-area>))
  (let* ((font #[t 'font])
	 (line-skip (font-line-skip font))
	 (lines #[t 'lines])
	 (space (render-text "_" font))
	 (cursor (rectangle 2 line-skip #x20eeaa22)))
      (for-each (lambda(line top)
		  (let ((image (render-text-cached line font)))
		    (set! #[ t 'w ] 
			  (max #[ t 'w ] (image-width image)))
		    (draw-image image 
				(+ (or #[ #[ t 'parent ] 'x ] 0) 
				   #[t 'x]) 
				(+ (or #[#[t 'parent] 'y] 0) 
				   #[t 'y] top))))
		(vector->list lines)
		(iota (vector-length lines) 0 line-skip))
      (if (equal? (current-output-port) #[t 'port])
	  (draw-image cursor 
		      (+ #[t 'x] (* (image-width space) 
				    (port-column #[ t 'port ])) )
		      (+ #[t 'y] (* line-skip 
				    (port-line #[ t 'port ])))))
    (set! #[ t 'h ] (* (vector-length lines) line-skip))))

(define-method (move-cursor! (w <text-area>)
			     (right <integer>)
			     (down <integer>))
  (set-port-line! #[ w 'port ] 
		  (max 0 (min (+ (port-line #[ w 'port ]) down) 
			      (- (vector-length #[ w 'lines ]) 1))))
  (set-port-column! 
   #[ w 'port ] 
   (max 0 (min 
	   (+ (port-column #[w 'port]) right)
	   (string-length #[#[w 'lines](port-line #[w 'port])])))))

(define-method (delete-char! (w <text-area>))
  (let ((p (slot-ref w 'port)))
    (delete-char! w (port-column p) (port-line p))))

(define-method (delete-char! (w <text-area>) 
			     (col <integer>) 
			     (line <integer>))
  (let* ((s #[ #[ w 'lines ] line ])
	 (sl (string-length s))
	 (l (substring s 0 (max 0 (min sl (- col 1)))))
	 (r (substring s (min sl col))))
    (set! #[ #[ w 'lines ] line ] (string-append l r))))

(define* (make-text-area #:key (text "hi! :)\n") (x 0)(y 0))
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
    (set! #[t 'lines] (list->vector (string-split text #\newline)))
    (set! #[t 'port] (make-soft-port
		      (vector
		       (lambda(c) 
			 (put-string (list->string (list c))))
		       put-string
		       (lambda()
			 (for-each display 
				   (vector->list #[t 'lines])))
		       #f
		       #f) "w"))
  (let* ((set-key! (lambda (key action)
		     (set! #[#[t 'special-keys] #[*scancodes* key]] 
			   action))))
    (set-key! "esc" (lambda()
		      (set-current-output-port *stdout*)
		      (input-mode 'direct)))
    (set-key! 
     "return"
     (lambda ()
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
			 1)))))
    (set-key! 
     "left" 
     (lambda()
       (let ((line (port-line #[ t 'port ]))
	     (column (port-column #[ t 'port ])))
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
       (let ((line (port-line #[ t 'port ]))
	     (column (port-column #[ t 'port ])))
	 (if (and (= column (string-length 
			     #[#[t 'lines] line]))
		  (< (+ line 1) 
		     (vector-length #[t 'lines])))
	     (move-cursor! 
	      t 
	      (- (string-length #[#[t 'lines] line]))
	      1)
	     ;;else
	     (move-cursor! t 1 0)))))
    (set-key! "up" (lambda()(move-cursor! t 0 -1)))
    (set-key! "down" (lambda()(move-cursor! t 0 1)))
    (set-key! 
     "f1" 
     (lambda()
       (let* ((lines (vector->list #[ t 'lines ]))
	      (line (port-line #[ t 'port ]))
	      (column (port-column #[ t 'port ]))
	      (text (string-join 
		     (append (take lines line)
			     (list (substring 
				    #[#[t 'lines] line]
				    0 column)))
		     "\n"))
	      (last-sexp (substring 
			  text 
			  (last-sexp-starting-position text))))
	 (display (eval-string last-sexp) *stdout*))))
    (set-key! 
     "backspace" 
     (lambda()
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
	     (begin 
	       (delete-char! t)
	       (move-cursor! t -1 0))))))
    (set-key! 
     "delete" 
     (lambda()
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
			(set! #[ t 'lines ]
			      (list->vector
			       (append pre
				       `(,(string-append this next))
				       rest))))))))
	     (begin
	       (delete-char! t (+ (port-column p) 1) (port-line p)) 
	       (move-cursor! t 0 0)))))))
  (set! #[t 'click]
	(lambda e
	  (set-current-output-port #[ t 'port ])
	  (set-typing-special! (lambda(scancode)
				 (#[#[t 'special-keys] scancode])))
	  (input-mode 'typing)))
  (set! #[t 'x] x)
  (set! #[t 'y] y)
  t))

(define-generic input-text!)
