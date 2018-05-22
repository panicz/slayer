(define-module (widgets text-area)
  #:use-module (oop goops)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (extra slayer)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (slayer font)
  #:use-module (widgets base)
  #:use-module (widgets sprite)
  #:use-module (widgets image-clipper)
  #:export (<text-area>
	    <input>
	    <parameter-editor>
	    <numeric-parameter-editor>
	    <numeric-input>
	    set-target!
	    clear-text!
	    )
  #:re-export (make)
  #:export-syntax (parameter-editor property-editor)
  )

(define-class <text-area> (<image-clipper>)
  (%space #:init-value #f)  ;; 
  (%cursor #:init-value #f) ;; cursor image
  (%background #:init-value #f)
  (%%image #:init-value #f)
  (%image
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (or #[self '%%image]
	 (let ((image (rectangle (max #[self 'w] #[self 'width])
				 (max #[self 'h] #[self 'height])
				 #[self 'background-color])))
	   (with-video-output-to image (render self))
	   (set! #[self '%%image] image)
	   image)))
   #:slot-set! noop)
  (%original-lines #:init-value #f) ;; if type is #:field, here's where
  ;; the original text is stored
  (%original-row #:init-value 0)
  (%original-column #:init-value 0)
  (text 
   #:init-keyword #:text
   #:allocation #:virtual
   #:slot-ref 
   (lambda (self)
     (string-join (vector->list #[self 'lines]) "\n"))
   #:slot-set! 
   (lambda (self text)
     (set! #[self 'lines]
       (list->vector (string-split text #\newline)))))

  (valid-lines #:init-value (lambda(lines) #t) #:init-keyword #:validate-lines)
  (on-confirm-change #:init-value noop #:init-keyword #:on-confirm-change)
  
  (background-color #:init-value #f #:init-keyword #:background-color)
  (text-color #:init-value #xffffff #:init-keyword #:text-color)
  (font #:init-value *default-font* #:init-keyword #:font)
  (port #:init-value #f)
  (on-text-change #:init-value noop #:init-keyword #:on-text-change)
  (char-height
   #:allocation #:virtual
   #:slot-ref (lambda (self) (font-line-skip #[self 'font]))
   #:slot-set! noop)
  (height
   #:allocation #:virtual
   #:slot-ref (lambda (self)
		(* #[self 'char-height] 
		   (vector-length #[self 'lines])))
   #:slot-set! noop)
  (char-width
   #:allocation #:virtual
   #:slot-ref (lambda (self) (image-width #[self '%space]))
   #:slot-set! noop)
  (width 
   #:allocation #:virtual
   #:slot-ref (lambda (self)
		(* #[self 'char-width]
		   (apply max  (map string-length
				    (vector->list 
				     #[self 'lines])))))
   #:slot-set! noop)
  (special-keys #:init-thunk 
		(lambda () (make-vector (vector-length *key-names*) noop)))
  
  (lines #:init-value '#(""))
  (max-lines #:init-value +inf.0 #:init-keyword #:max-lines)
  (on-max-lines-reached 
   #:init-value noop 
   #:init-keyword #:on-max-lines-reached)
  (%render-cache #:init-value #f)
  (%thread-results #:init-thunk make-hash-table))

(define-method (render (t <text-area>))
  (let* ((font #[t 'font])
	 (line-skip (font-line-skip font))
	 (lines #[t 'lines])
	 (cursor #[t '%cursor])
	 (space #[t '%space]))
    (if (or (not #[t '%render-cache])
	    (not (= (vector-length #[t '%render-cache])
		    (vector-length lines))))
	(set! #[t '%render-cache] (make-vector (vector-length lines) #f)))
    (when #[t 'background-color]
      (when (or (not #[t '%background])
		(not (equal? (image-size #[t '%background])
			     `(,(max #[t 'w] #[t 'width]) 
			       ,(max #[t 'h] #[t 'height])))))
	(set! #[t '%background] 
	      (rectangle (max #[t 'w] #[t 'width]) 
			 (max #[t 'h] #[t 'height]) 
			 #[t 'background-color])))
      (draw-image! #[t '%background]))
    (for n in 0 .. (1- (vector-length lines))
	 (let ((image (or #[t : '%render-cache : n]
			  (let ((fresh-image (render-text 
					      (string-append #[lines n] " ")
					      font
					      #[t 'text-color]
					      #[t 'background-color])))
			    (set! #[t : '%render-cache : n] fresh-image)
			    fresh-image))))
	   (draw-image! image 0 (* n line-skip))))
    (if (equal? (current-output-port) #[t 'port])
	(draw-image! cursor 
		     (* (image-width space) 
			(port-column #[t 'port]))
		     (* line-skip 
			(port-line #[t 'port]))))))

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

(define-method (%flush-cache! (w <text-area>))
  (set! #[w '%background] #f)
  (set! #[w '%render-cache] #f)
  (set! #[w '%cropped-image] #f)
  (set! #[w '%%image] #f))

(define-method (move-cursor! (w <text-area>)
			     (right <integer>)
			     (down <integer>))
  (wrr "Moving cursor "right" "down)
  (%flush-cache! w)
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
  (%flush-cache! w)
  (let* ((s #[w : 'lines : line ])
	 (sl (string-length s))
	 (l (substring s 0 (max 0 (min sl (- col 1)))))
	 (r (substring s (min sl col))))
    (set! #[w : 'lines : line] (string-append l r))))

(define-method (break-line! (t <text-area>))
  (%flush-cache! t)
  (let ((line #[#[t 'lines] (port-line #[t 'port])])
	(line-number (port-line #[ t 'port ]))
	(lines (vector->list #[ t 'lines ]))
	(column (port-column #[ t 'port ])))
    (if (>= (length lines) #[t 'max-lines])
	(#[t 'on-max-lines-reached] t)
    #;else
	(let ((left (substring line 0 column))
	      (right (substring line column)))
	  (set! #[ t 'lines ]
		(list->vector
		 (append (take lines line-number)
			 (list left right)
			 (drop lines (+ line-number 1)))))
	  (move-cursor! t (- (port-column #[t 'port])) 
			1)))))

(define-method (clear-text! (t <text-area>))
  (%flush-cache! t)
  (set-port-line! #[t 'port] 0)
  (set-port-column! #[t 'port] 0)
  (set! #[t 'text] "")
  (display #[t 'text] *stderr*))
  

(define-method (leave-typing-mode! (t <text-area>))
  (%flush-cache! t)
  (set-current-output-port *stdout*)
  (set-input-mode! 'direct))

(define-method (move-cursor-back! (t <text-area>))
  (%flush-cache! t)
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
  (%flush-cache! t)
  (let ((line (port-line #[ t 'port ]))
	(column (port-column #[ t 'port ])))
    (if (and (= column (string-length 
			#[t : 'lines : line]))
	     (< (+ line 1) 
		(vector-length #[t 'lines])))
	(move-cursor! t (- (string-length #[t : 'lines : line])) 1)
    #;else
	(move-cursor! t 1 0))))

(define-method (move-cursor-to-the-end-of-line! (t <text-area>))
  (%flush-cache! t)
  (let* ((port #[t 'port])
	 (line #[t : 'lines : (port-line port)]))
    (move-cursor! t (- (string-length line) 
		       (port-column port)) 
		  0)))

(define-method (move-cursor-to-the-beginning-of-line! (t <text-area>))
  (%flush-cache! t)
  (move-cursor! t (- (port-column #[t 'port])) 0))

(define-method (delete-previous-char! (t <text-area>))
  (%flush-cache! t)
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
		   (move-cursor! t (string-length #[lines (- (port-line p) 1)])
				 -1))))))
    #;else
	(begin 
	  (delete-char! t)
	  (move-cursor! t -1 0)))))

(define-method (delete-next-char! (t <text-area>))
  (%flush-cache! t)
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
				  rest))))
		  (else
		   (noop))))))
    #;else
	(begin
	  (delete-char! t (+ (port-column p) 1) (port-line p)) 
	  (move-cursor! t 0 0)))))

(define-method (restore-original-text! (t <text-area>))
  (set! #[t 'lines] #[t '%original-lines])
  (%flush-cache! t)
  (leave-typing-mode! t))

(define-method (confirm-text-change! (t <text-area>))
  (leave-typing-mode! t)
  (#[t 'on-confirm-change] t))

(define-method (enter-typing-mode! (t <text-area>))
  (set! #[t '%original-lines]
	(vector-copy #[t 'lines]))
  (%flush-cache! t)
  (set-current-output-port #[ t 'port ])
  (set-typing-special-procedure! 
   (lambda(scancode)
     (#[t : 'special-keys : scancode])))
  (set-input-mode! 'typing))

(define (vector-insert v i x)
  (let* ((n (vector-length v))
	 (v* (make-vector (+ n 1))))
    (for j in (iota i)
      (vector-set! v* j (vector-ref v j)))
    (vector-set! v* i x)
    (for j in (iota (- n i) i)
      (vector-set! v* (+ j 1) (vector-ref v j)))
    v*))

(define (wrr . xs)
  (for x in xs
    (write x *stderr*))
  (newline *stderr*))


(define-method (initialize (t <text-area>) args)
  (next-method)
  (let-keywords args #t ((type #:text-area))
		(let* ((put-string
			(lambda (s row col)
			  (let ((p #[t 'port]))
			    (set! #[t '%cropped-image] #f)
			    (set! #[t '%%image] #f)
			    (set! #[t '%render-cache] #f)
			    (let ((line #[t : 'lines : row ]))
			      (set! #[t : 'lines : row]
				    (string-append
				     (substring line 0 col)
				     s
				     (substring line col)))))))
		       (put-lines (lambda (s)
				    (let ((p #[t 'port]))
				      (let loop ((lines (string-split
							 s #\newline))
						 (line (port-line p))
						 (col (port-column p)))
					(match lines
					  (`(,last)
					   (put-string last line col))
					  
					  (`(,first . ,rest)
					   (put-string first line col)
					   (set! #[t 'lines] (vector-insert
							      #[t 'lines]
							      (+ line 1)
							      ""))
					   (loop rest (+ line 1) 0))))))))

      (set! #[t '%cursor] (rectangle 2 #[t 'char-height]
				     #x20eeaa22))
      (set! #[t '%space] (render-text "_" #[t 'font]))

      (set! #[t 'port] (make-soft-port
			(vector
			 (lambda(c) 
			   (put-lines (list->string (list c))))
			 put-lines
			 noop
			 #f
			 #f) "w"))
      (set! #[t 'left-click] 
	(lambda (x y)
	  (let* ((port #[t 'port])
		 (lines #[t 'lines])
		 (line (1- (min (quotient (- y #[t 'y] (- #[t 'top]))
					  #[t 'char-height])
				(vector-length lines))))
		 (column (min (quotient (- x #[t 'x] (- #[t 'left]))
					#[t 'char-width])
			      (string-length #[lines line]))))
	    (enter-typing-mode! t)
	    (set-port-column! port column)
	    (set-port-line! port line)
	    (if (eq? type #:field)
		(set-port-column! port 0)))))
      (let* ((set-key! (lambda (key action)
			 (set! #[#[t 'special-keys] #[*scancodes* key]]
			       (lambda()(action t))))))
	(set-key! 'left  move-cursor-back!)
	(set-key! 'right move-cursor-forward!)
	(set-key! 'up (lambda(t)(move-cursor! t 0 -1)))
	(set-key! 'down (lambda(t)(move-cursor! t 0 1)))
	(set-key! 'end move-cursor-to-the-end-of-line!)
	(set-key! 'home move-cursor-to-the-beginning-of-line!)
	(set-key! 'f1 
		  (lambda(t)
		    (call-with-new-thread 
		     (lambda ()
		       (display (let ((result (eval-string (last-sexp t))))
				  (if (unspecified? result) "" result))
				*stdout*)
		       (newline *stdout*)))))
	(set-key! 'f2 (lambda(t)(display (last-sexp t) *stdout*)))
	(set-key! 'backspace delete-previous-char!)
	(set-key! 'delete delete-next-char!)
	(case type
	  ((#:field)
	   (set-key! 'esc restore-original-text!)
	   (set-key! 'return (lambda(t)
				(if (#[t 'valid-lines] #[t 'lines])
				    (confirm-text-change! t)
				    (begin
				      (display "invalid text\n" *stdout*)
				      (restore-original-text! t))))))
	  (else
	   (set-key! 'esc leave-typing-mode!)
	   (set-key! 'return (lambda (t) (if (ctrl?)
					(begin (call-with-new-thread 
						(lambda ()
						  (display (let ((result (eval-string (last-sexp t))))
							     (if (unspecified? result) "" result))
							   *stdout*)
						  (newline *stdout*)))
					       (leave-typing-mode! t))
					(break-line! t))))))
	))))

(define-class <parameter-editor> (<text-area>)
  (%render-cache
   #:allocation #:virtual
   #:slot-ref (lambda (self) (make-vector (vector-length #[self 'lines]) #f))
   #:slot-set! noop)

  (%%%image #:init-value #f)
  (previous-value #:init-value #f)
  (%%image
   #:allocation #:virtual
   #:slot-ref (lambda (self)
		(if (equal? #[self 'value] #[self 'previous-value])
		    #[self '%%%image]
		#;else 
		    (begin
		      (set! #[self 'previous-value] #[self 'value])
		      (set! #[self '%%%image] #f)
		      #f)))
   #:slot-set! (lambda (self value)
		 (set! #[self '%%%image] value)))
  (%cropped-image
   #:allocation #:virtual
   #:slot-ref (lambda (self) #f)
   #:slot-set! noop)
		
  (target #:init-value #f #:init-keyword #:target)

  (value-getter #:init-value noop #:init-keyword #:value-getter)
  (value-setter #:init-value noop #:init-keyword #:value-setter)

  (%lines-confirmed #:init-value #t)
  (%lines #:init-value #(""))

  (value
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (if (not #[self 'target])
	 "<no target>"
	 (->string (#[self 'value-getter] #[self 'target]))))
   #:slot-set!
   (lambda (self value)
     (if #[self 'target]
	 (#[self 'value-setter] #[self 'target] #[self : 'lines : 0]))))

  (on-confirm-change
   #:init-value 
   (lambda (self)
     (set! #[self 'value] #[self : 'lines : 0])
     (set! #[self '%lines-confirmed] #t)))

  (lines
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (if #[self '%lines-confirmed]
	 (vector #[self 'value])
	 #[self '%lines]))
   #:slot-set!
   (lambda (self value)
     (set! #[self '%lines] value)
     (set! #[self '%lines-confirmed] #f)
     (%flush-cache! self))))

(define-class <input> (<widget>)
  (default-editor-class #:allocation #:each-subclass 
    #:init-value <parameter-editor>)
  (%label #:init-value #f)
  (input #:init-value #f)
  (label
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     #[self : '%label : 'text])
   #:slot-set!
   (lambda (self value)
     (set! #[self : '%label : 'text] value)
     (set! #[self : 'input : 'x] #[self : '%label : 'w])))

  (target
   #:allocation #:virtual
   #:slot-ref 
   (lambda (self) 
     #[self : 'input : 'target])
   #:slot-set! 
   (lambda (self value) 
     (set! #[self : 'input : 'target] value)))

  (children
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     `(,#[self '%label] ,#[self 'input]))
   #:slot-set! noop))

(define-method (enter-typing-mode! (t <parameter-editor>))
  (next-method)
  (set! #[t '%lines] #[t 'lines])
  (set! #[t '%lines-confirmed] #f))

(define-method (initialize (self <parameter-editor>) args)
  (next-method)
  (set! #[self '%lines-confirmed] #t)
  (let-keywords args #t ((accessor noop))
    (set! #[self 'value-getter]
	  accessor)
    (set! #[self 'value-setter]
	  (lambda (target value)
	    (let ((value (read-string value)))
	      (set! (accessor target) value))))))

(define-method (initialize (self <input>) args)
  (next-method)
  (let-keywords args #t ((label "input: ")
			 (editor-class #[self 'default-editor-class])
			 (target #f)
			 (value-getter noop)
			 (validate-lines (lambda (lines) #t))
			 (accessor noop)
			 (value-setter noop))
    (let* ((label (make <label> #:text label #:x #[self 'x] #:y #[self 'y]))
	   (field (make editor-class #:max-lines 1 #:type #:field
			#:target target
			#:value-getter value-getter
			#:value-setter value-setter
			#:accessor accessor
			#:validate-lines validate-lines
			#:text-color #x000000
			#:background-color #xffffff
			#:x (+ #[self 'x] #[label 'w]) #:y #[self 'y]
			#:w (- #[self 'w] #[label 'w]) #:h #[self 'h])))
      (set! #[field 'parent] self)
      (set! #[label 'parent] self)
      (set! #[self '%label] label)
      (set! #[self 'input] field)
      (set! #[self 'left-click] (lambda (x y)
				  (#[self : 'input : 'left-click] x y))))))

(define-method (draw (self <input>))
  (for child in #[self 'children]
       (draw child)))

(define-class <numeric-parameter-editor> (<parameter-editor>)
  (valid-lines #:init-value 
	       (lambda (lines)
		 (and (= (vector-length lines) 1)
		      (string-match
		       "^\\s*[+-]?[0-9]*\\.?[0-9]+\\s*$"
		       #[lines 0])))))

(define-method (initialize (self <numeric-parameter-editor>) args)
  (next-method)
  (let-keywords args #t ((accessor noop))
    (set! #[self 'mouse-wheel-down]
      (lambda (x y)
	(let ((target #[self 'target])
	      (value (read-string #[self 'value])))
	  (set! (accessor target) (- value (if (shift?) 0.001 0.01))))))
    (set! #[self 'mouse-wheel-up]
      (lambda (x y)
	(let ((target #[self 'target])
	      (value (read-string #[self 'value])))
	  (set! (accessor target) (+ value (if (shift?) 0.001 0.01))))))))


(define-class <numeric-input> (<input>)
  (default-editor-class #:allocation #:each-subclass 
    #:init-value <numeric-parameter-editor>))

(define-syntax-rule (parameter-editor target (label property) ...)
  ((layout #:lay-out lay-out-horizontally)
   (make <numeric-input> #:w 60 #:h 12 #:label label
	 #:target target
	 #:accessor (accessor target property))
   ...))

(define-syntax-rule (property-editor target (label property) ...)
  ((layout #:lay-out lay-out-horizontally)
   (make <input> #:w 180 #:h 12 #:label label
	 #:target target
	 #:accessor (accessor target property))
   ...))

(define-method (set-target! #;of (w <widget>) #;as target)
  (if (is-a? w <parameter-editor>)
      (set! #[w 'target] target))
  (for child in #[w 'children]
       (set-target! #;of child #;as target)))
