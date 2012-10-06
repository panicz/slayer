(set! %load-path (append '("./" "../")  %load-path))

(use-modules (oop goops)
	     (srfi srfi-1) 
	     (srfi srfi-2)
	     (srfi srfi-11)
	     (ice-9 match)
	     (ice-9 optargs)
	     (ice-9 pretty-print)
	     (ice-9 local-eval)
	     (system base compile)
	     (system syntax)
	     (extra ref)
	     (extra vector-lib)
	     (extra common)
	     ((rnrs) :version (6))
	     ;(extra function)
	     )
;(setenv "GUILE_WARN_DEPRECATED" "no")
;(putenv "GUILE_WARN_DEPRECATED=no")
;(display (getenv "GUILE_WARN_DEPRECATED"))

(define (local-lexicals id)
  (filter (lambda (x)
	    (eq? (syntax-local-binding x) 'lexical))
	  (syntax-locally-bound-identifiers id)))

(define-syntax lexical-names
  (lambda (x)
    (syntax-case x ()
      ((lexical-names) #'(lexical-names lexical-names))
      ((lexical-names scope)
       (with-syntax (((id ...) (local-lexicals #'scope)))
	 #'(list 'id ...))))))

(define-syntax lexicals
  (lambda (x)
    (syntax-case x ()
      ((lexicals) #'(lexicals lexicals))
      ((lexicals scope)
       (with-syntax (((id ...) (local-lexicals #'scope)))
	 #'(list (cons 'id id) ...))))))

(define-macro (function args . body)
  `(let ((environment (the-environment))
	 (lexical-names (lexical-names))
	 (procedure (lambda ,args ,@body)))
     (set-procedure-property! procedure 'source '(function ,args ,@body))
     (set-procedure-property! procedure 'environment environment)
     (set-procedure-property! procedure 'lexical-names lexical-names)
     procedure))

(define (procedure-environment proc) (procedure-property proc 'environment))
(define (procedure-lexical-names proc) (procedure-property proc 'lexical-names))

(define (procedure-lexicals proc)
  (map (lambda(symbol)
	 (cons symbol
	       (local-eval symbol (procedure-environment proc))))
   (procedure-lexical-names proc)))

(procedure-lexicals (function(x)(+ x x)))

(define *procedure-sources* (make-hash-table))
(hash-set! *procedure-sources* 'keydn (make-hash-table))
(hash-set! *procedure-sources* 'keyup (make-hash-table))

(define-syntax keydn
  (syntax-rules ()
    ((_ key function)
     (begin
       (set! #[ #[ *procedure-sources* 'keydn ] key ] (quote function))
       (%keydn key function)))))

(define-syntax keyup 
  (syntax-rules ()
    ((_ key function)
     (begin
       (set! #[ #[ *procedure-sources* 'keyup ] key ] (quote function))
       (%keyup key function)))))

(define *stdout* (current-output-port))
(define *stdin* (current-input-port))
(define *stderr* (current-error-port))

(define (object-source value)
  (cond 
   ((symbol? value)
    `',value)
   ((procedure? value)
    (or (procedure-name value) (procedure-source value) 'noop))
   ((list? value)
    `',(map object-source value))
   ((vector? value) ; for some reason vector-map doesn't work
    (list->vector (map object-source (vector->list value))))
   ((hash-table? value)
    `(let ((h (make-hash-table)))
       noop
       ,@(map (match-lambda ((key . value)
			   `(hash-set! h ,(object-source key) ,(object-source value))))
	     (hash-map->list cons value))))
   (#t value)))

(define (save file)
  (with-output-to-port *stdout*
    (lambda()
      (display (string-append "saving to file "file":\n"))
      (for-each (match-lambda ((key . function)
			       (if (and function 
					(not (equal? function noop))
					(not (equal? function 'noop)))
				   (pretty-print `(keydn ',key ,function)))))
		(hash-map->list cons #[ *procedure-sources* 'keydn ]))
      (for widget in #[ *stage* 'children ]
	   (let ((w (gensym "widget-")))
	     (pretty-print
	      `(let ((,w (make ,(class-name (class-of widget)))))
		 ,@(map (match-lambda((slot-name . info)
				      `(slot-set! ,w ',slot-name ,
						  (object-source (slot-ref widget slot-name)))))
			(class-slots (class-of widget)))
		 (add-child! *stage* ,w)
		 )))))))

(define *stdio* 
  (make-soft-port 
   (vector 
    (lambda (c) (write c *stdout*)) ; procedure accepting one character for output
    (lambda (s) (display s *stdout*)) ; procedure accepting a string for output
    (lambda () (display "." *stdout*)) ; thunk for flushing output
    (lambda () (char-upcase (read-char))) ; thunk for getting one character
    (lambda () (display "@" *stdout*)) ; thunk for closing port (not by garbage collector)
    #f) ; (if present and not `#f') thunk for computing the number
					; of characters that can be read from the port without blocking
   "rw"))

(set-current-input-port *stdio*)
(set-current-output-port *stdio*)

(define-generic update!)
(define-generic draw)
(define-generic area)
(define-generic add-child!)

(define-class <widget> ()
  (parent #:init-value #f #:init-keyword #:parent)
  (children #:init-value '() #:init-keyword #:children)
  (click #:init-value noop #:init-keyword #:click)
  (drag #:init-value noop #:init-keyword #:drag)
  (update!  #:init-value noop #:init-keyword #:update)
  (activate #:init-value noop #:init-keyword #:activate)
  (deactivate #:init-value noop #:init-keyword #:deactivate)
  (x #:init-value 0 #:init-keyword #:x)
  (y #:init-value 0 #:init-keyword #:y)
  (w #:init-value 0 #:init-keyword #:w)
  (h #:init-value 0 #:init-keyword #:h))

(define-method (area (w <widget>))
  (list (slot-ref w 'x) (slot-ref w 'y) (slot-ref w 'w) (slot-ref w 'h)))

(define-method (draw (w <widget>))
  (for-each draw (reverse (slot-ref w 'children))))

(define-method (add-child! (parent <widget>) (child <widget>))
  (slot-set! parent 'children (cons child (slot-ref parent 'children)))
  (slot-set! parent 'w (max (slot-ref parent 'w) (+ (slot-ref child 'x) (slot-ref child 'w))))
  (slot-set! parent 'h (max (slot-ref parent 'h) (+ (slot-ref child 'y) (slot-ref child 'h))))
  (slot-set! child 'parent parent))

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

(define-class <image> (<widget>)
  (image #:init-keyword #:image))

(define-method (draw (i <image>))
  (draw-image (slot-ref i 'image) (slot-ref i 'x) (slot-ref i 'y)))

(define (make-image image x y)
  (let ((image (make <image> #:image image #:x x #:y y 
		     #:w (image-width image) 
		     #:h (image-height image))))
    (slot-set! image 'drag 
	       (lambda (type state x y xrel yrel)		 
		 (slot-set! image 'x (+ (slot-ref image 'x) xrel))
		 (slot-set! image 'y (+ (slot-ref image 'y) yrel))))
    image))

(define *default-font* (load-font "./VeraMono.ttf" 12))
;(set-font-style! *default-font* 1)

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
			(if (and (in? #[ str pos* ] '(#\# #\' #\`))
				 (not (eq? #[ str (+ pos* 1)] #\:)))
			    pos*
			    pos))
		      pos))
		 ((> level 1)
		  (eat (- pos 1) (- level 1)))
		 (#t
		  (error "mismatch braces")))))))


(define-class <text-area> (<widget>)
  (lines #:init-value '#(""))
  (special-keys #:init-thunk (lambda()(make-vector (vector-length *key-names*) noop)))
  ;(cursor-position #:init-value '(0 0))
  (font #:init-value *default-font* #:init-keyword #:font)
  (port #:init-value *stdio*)
  #;(visible-cols #:init-keyword #:visible-cols)
  #;(visible-lines #:init-keyword #:visible-lines)
  (rendered-lines #:init-thunk make-hash-table))


(define-method (draw (t <text-area>))
  (let* ((font #[ t 'font ])
	 (line-skip (font-line-skip font))
	 (lines #[ t 'lines ])
	 (space (render-text "_" font))
	 (cursor (rectangle 2 line-skip #x20eeaa22)))
      (for-each (lambda(line top)
		  (let ((image (render-text line font)))
		    (set! #[ t 'w ] (max #[ t 'w ] (image-width image)))
		    (draw-image image #[ t 'x ] (+ #[ t 'y ] top))))
		(vector->list lines)
		(iota (vector-length lines) 0 line-skip))
      (if (equal? (current-output-port) #[ t 'port ])
	  (draw-image cursor 
		      (* (image-width space) (port-column #[ t 'port ])) 
		      (* line-skip (port-line #[ t 'port ]))))
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

(define-generic input-text!)
(define *input-widget* #f)

