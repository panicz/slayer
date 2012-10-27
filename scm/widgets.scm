(display "loading widgets.scm\n")

(define-generic update!)
(define-generic draw)
(define-generic area)
(define-generic add-child!)
(define-generic remove-child!)

(define-class <widget> ()
  (parent #:init-value #f #:init-keyword #:parent)
  (children #:init-value '() #:init-keyword #:children)
  (click #:init-value noop #:init-keyword #:click)
  (unclick #:init-value noop #:init-keyword #:unclick)
  (right-click #:init-value noop #:init-keyword #:right-click)
  (right-unclick #:init-value noop #:init-keyword #:right-unclick)
  (mouse-over #:init-value noop #:init-keyword #:mouse-over)
  (mouse-out #:init-value noop #:init-keyword #:mouse-out)
  (drag #:init-value noop #:init-keyword #:drag)
  (update!  #:init-value noop #:init-keyword #:update)
  (activate #:init-value noop #:init-keyword #:activate)
  (deactivate #:init-value noop #:init-keyword #:deactivate)
  (x #:init-value 0 #:init-keyword #:x)
  (y #:init-value 0 #:init-keyword #:y)
  (w #:init-value 0 #:init-keyword #:w)
  (h #:init-value 0 #:init-keyword #:h))

(define-method (ancestors (w <widget>))
  (or (and-let* ((parent #[w 'parent]))
	(cons parent (ancestors parent)))
      '()))

(define-method (area (w <widget>))
  (list #[ w 'x ] #[ w 'y ] #[ w 'w ] #[ w 'h ]))

(define-method (absolute-area (w <widget>))
  (let ((widgets (cons w (ancestors w))))
    (list (apply + (map #[_ 'x] widgets))
	  (apply + (map #[_ 'y] widgets))
	  #[w 'w] #[w 'h])))

(define-method (draw (w <widget>))
  (for-each draw (reverse #[ w 'children ])))

(define-method (add-child! (parent <widget>) (child <widget>))
  (set! #[ parent 'children ] (cons child #[ parent 'children ]))
  (set! #[ parent 'w ] (max #[ parent 'w ] (+ #[ child 'x ] #[ child 'w ])))
  (set! #[ parent 'h ] (max #[ parent 'h ] (+ #[ child 'y ] #[ child 'h ])))
  (set! #[ child 'parent ] parent))

(define-method (remove-child! (parent <widget>) (child <widget>))
  (set! #[ child 'parent ] #f)
  (set! #[ parent 'children ] (delete child #[ parent 'children ])))

(define (in-area? point area)
  (match-let
   (((x y w h) area)
    ((px py) point))
   (and (<= x px (+ x w)) (<= y py (+ y h)))))

(define (widget-nested-find condition widget)
  (if (not (condition widget))
      #f
      (let ((w (find condition (slot-ref widget 'children))))
	(if (not w)
	    widget
	    (let ((c (widget-nested-find condition w)))
	      (if (not c) w c))))))

(define-class <bitmap> (<widget>)
  (image #:init-keyword #:image))

(define-method (draw (i <bitmap>))
  (draw-image #[ i 'image ]
	      (+ (or #[#[ i 'parent ] 'x] 0) #[ i 'x ]) 
	      (+ (or #[#[ i 'parent ] 'y] 0) #[ i 'y ])))

(define* (make-button #:key (text "button") (x 0) (y 0) (w #f) (h #f))
  (let ((normal (render-text text *default-font* #xffffff #xff0000))
	(over (render-text text *default-font* #xffffff #x00ff00))
	(clicked (render-text text *default-font* #xffffff #x0000ff)))
    (let ((button (make <bitmap> #:image normal #:x x #:y y 
			#:w (or w (image-width normal))
			#:h (or h (image-height normal)))))
      (set! #[ button 'mouse-over ] (function e (set! #[ button 'image ] over )))
      (set! #[ button 'mouse-out ] (function e (set! #[ button 'image ] normal )))
      (set! #[ button 'click ] (function e (set! #[ button 'image ] clicked )))
      (set! #[ button 'unclick ] #[ button 'mouse-over ])
      (set! #[ button 'drag ] noop
	    )

      button)))

(define* (make-container #:key x y (name "menu") (content '()))
  (let ((container (make <widget>))
	(label (make-button #:text name)))
    (set! #[label 'drag] (function (type state x y xrel yrel)		 
			   (set! #[ container 'x ] (+ #[ container 'x ] xrel))
			   (set! #[ container 'y ] (+ #[ container 'y ] yrel))))
    (for child in (append content `(,label))
	 (add-child! container child))
    ;(add-child! container (make-button #:x 0 #:y 0 #:text name))
    (let ((top 0))
      (for w in #[container 'children]
	   (set! #[container 'w] (max #[w 'w] #[container 'w]))
	   (set! #[w 'y] top)
	   (set! top (+ top #[w 'h])))
      (set! #[container 'h] top))
    container))

(define (make-image image x y)
  (let ((image (make <bitmap> #:image image #:x x #:y y 
		     #:w (image-width image) 
		     #:h (image-height image))))
    (set! #[ image 'drag ]
	  (function (type state x y xrel yrel)		 
	    (set! #[ image 'x ] (+ #[ image 'x ] xrel))
	    (set! #[ image 'y ] (+ #[ image 'y ] yrel))))
    #;(set! #[ image 'mouse-over ]
	  (function (type state x y xrel yrel)
	    (format #t "now mouse is over ~s\n" image)))
    #;(set! #[ image 'mouse-out ]
	  (function (type state x y xrel yrel)
	    (format #t "mouse is no longer over ~s\n" image)))
    image))

(define *default-font* (load-font "./VeraMono.ttf" 12))

;(set-font-style! *default-font* 1)

(define-class <text-area> (<widget>)
  (lines #:init-value '#(""))
  (special-keys #:init-thunk (function()(make-vector (vector-length *key-names*) noop)))
  ;(cursor-position #:init-value '(0 0))
  (font #:init-value *default-font* #:init-keyword #:font)
  (port #:init-value *stdio*)
  #;(visible-cols #:init-keyword #:visible-cols)
  #;(visible-lines #:init-keyword #:visible-lines)
  #;(rendered-lines #:init-thunk make-hash-table))

(define-method (draw (t <text-area>))
  (let* ((font #[ t 'font ])
	 (line-skip (font-line-skip font))
	 (lines #[ t 'lines ])
	 (space (render-text "_" font))
	 (cursor (rectangle 2 line-skip #x20eeaa22)))
      (for-each (lambda(line top)
		  (let ((image (render-text line font)))
		    (set! #[ t 'w ] (max #[ t 'w ] (image-width image)))
		    (draw-image image (+ (or #[ #[ t 'parent ] 'x ] 0) #[ t 'x ]) 
				(+ (or #[ #[ t 'parent ] 'y ] 0) #[ t 'y ] top))))
		(vector->list lines)
		(iota (vector-length lines) 0 line-skip))
      (if (equal? (current-output-port) #[ t 'port ])
	  (draw-image cursor 
		      (+ #[t 'x] (* (image-width space) (port-column #[ t 'port ])) )
		      (+ #[t 'y] (* line-skip (port-line #[ t 'port ])))))
    (set! #[ t 'h ] (* (vector-length lines) line-skip))))

(define-method (move-cursor! (w <text-area>)
			     (right <integer>)
			     (down <integer>))
  (set-port-line! #[ w 'port ] (max 0 (min (+ (port-line #[ w 'port ]) down) 
				(- (vector-length #[ w 'lines ]) 1))))
  (set-port-column! #[ w 'port ] (max 0 (min (+ (port-column #[ w 'port ]) right)
				  (string-length #[ #[ w 'lines ] (port-line #[ w 'port ]) ])))))

(define-method (delete-char! (w <text-area>))
  (let ((p (slot-ref w 'port)))
    (delete-char! w (port-column p) (port-line p))))

(define-method (delete-char! (w <text-area>) (col <integer>) (line <integer>))
  (let* ((s #[ #[ w 'lines ] line ])
	 (sl (string-length s))
	 (l (substring s 0 (max 0 (min sl (- col 1)))))
	 (r (substring s (min sl col))))
    (set! #[ #[ w 'lines ] line ] (string-append l r))))

(define* (make-text-area #:key (text "hi! :)\n") (x 0)(y 0))
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
    (set! #[t 'lines] (list->vector (string-split text #\newline)))
    (set! #[t 'port] (make-soft-port*
		      (vector
		       (function(c) (put-string (list->string (list c))))
		       put-string
		       (function()
			 (for-each display 
				   (vector->list #[t 'lines])))
		       #f
		       #f) "w"))
  (let* ((set-key! (function (key action)
		     (set! #[#[t 'special-keys] #[*scancodes* key]] action))))
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
						       (move-cursor! 
							t 
							(string-length #[lines (- (port-line p) 1)])
							-1))))))
				  (begin 
				    (delete-char! t)
				    (move-cursor! t -1 0))))))
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
  (set! #[t 'click]
	(function e
	  (set-current-output-port #[ t 'port ])
	  (set! *input-widget* t)
	  (input-mode 'typing)))
  (set! #[t 'x] x)
  (set! #[t 'y] y)
  t))

(define-generic input-text!)
(display "loaded widgets.scm\n")
