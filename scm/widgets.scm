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
	     )


(define (local-lexicals id)
  (filter (lambda (x)
	    (eq? (syntax-local-binding x) 'lexical))
	  (syntax-locally-bound-identifiers id)))

(define-syntax lexicals
  (lambda (x)
    (syntax-case x ()
      ((lexicals) #'(lexicals lexicals))
      ((lexicals scope)
       (with-syntax (((id ...) (local-lexicals #'scope)))
	 #'(list (cons 'id id) ...))))))

(define-syntax function
  (lambda (x)
    (syntax-case x ()
      ((function args body ...)
       #'(let ((procedure (lambda args body ...))
	       (get-lexicals (lambda()(lexicals function))))
	   (set-procedure-property! procedure 'source '(function args body ...))
	   (set-procedure-property! procedure 'get-lexicals get-lexicals)
	   procedure)))))

;(procedure-lexicals (let* ((x 1)(y 2)) (function(z)(+ x y z))))

(define (procedure-lexicals proc)
  ((procedure-property proc 'get-lexicals)))

(define *stdout* (current-output-port))
(define *stdin* (current-input-port))
(define *stderr* (current-error-port))

(define *soft-port-sources* (make-hash-table))

(define (make-soft-port* pv modes)
  (let ((port (make-soft-port pv modes)))
    (set! #[ *soft-port-sources* port ] (cons pv modes))
    port))

(define *stdio* 
  (make-soft-port
   (vector 
    (function (c) (write c *stdout*)) ; procedure accepting one character for output
    (function (s) (display s *stdout*)) ; procedure accepting a string for output
    (function () (display "." *stdout*)) ; thunk for flushing output
    (function () (char-upcase (read-char))) ; thunk for getting one character
    (function () (display "@" *stdout*)) ; thunk for closing port (not by garbage collector)
    #f) ; (if present and not `#f') thunk for computing the number
   ;; of characters that can be read from the port without blocking
   "rw"))

(set-current-input-port *stdio*)
(set-current-output-port *stdio*)


(define (port-source port)
  (cond ((equal? port (current-input-port))
	 '(current-input-port))
	((equal? port (current-output-port))
	 '(current-output-port))
	((equal? port (current-error-port))
	 '(current-error-port))
	((and-let* ((source #[ *soft-port-sources* port ]))
	   (match-let (((pv . mode) source))
		      `(make-soft-port* 
			,(vector-map (lambda(p)
				       (and (procedure? p)(procedure-source* p))) 
				     pv) ,mode))))
	(#t '*stdio*)))

(define (vector-source v)
  (or (and-let* ((size (vector-length v))
		 ((> size 7)) 
		 (value-count (make-hash-table))
		 ((vector-for-each (lambda(e)
				     (set! #[ value-count e ]
					   (+ (or #[ value-count e ] 0) 1)))
				   v))
		 (value-score (sort (hash-map->list cons value-count)
				    (match-lambda* (((val1 . cnt1) (val2 . cnt2))
						   (> cnt1 cnt2)))))
		 (dominant-count (match value-score (((value . count) . rest) count)))
		 ((> (/ dominant-count size) 1/2)))
	(let ((dominant-value (match value-score (((value . count) . rest) value)))
	      (w (gensym "vec-")))
	  `(let ((,w (make-vector ,size ,(object-source dominant-value))))
	     ,@(filter-map (lambda(i)
			     (let ((i-th #[ v i ]))
			       (and (not (eq? i-th dominant-value))
				    `(vector-set! ,w ,i ,(object-source i-th)))))
			   (iota size))
	     ,w)))
      (vector-map object-source v)))

(define (hash-source value)
  (let ((h (gensym "hash-")))
    `(let ((,h (make-hash-table)))
       ,@(map (match-lambda ((key . value)
			     `(hash-set! ,h ,(object-source key) ,(object-source value))))
	      (hash-map->list cons value))
       ,h)))

(define (procedure-source* proc)
  (let ((lexis (procedure-lexicals proc)))
    (if (and (list? lexis) (not (null? lexis)))
	`(let* ,(map (match-lambda ((symbol . value)
				   `(,symbol ,(object-source value))))
		    lexis)
	   ,(procedure-source proc))
	(procedure-source proc))))

(define objects=>symbols (make-fluid (make-hash-table)))

(define (goops-symbol object)
  (or (and-let* ((objects=>symbols (fluid-ref objects=>symbols))
		 ((hash-table? objects=>symbols))
		 (symbol #[ objects=>symbols object ]))
	symbol)
      (let ((o (gensym "object-")))
	(set! #[ (fluid-ref objects=>symbols) object ] o)
	o)))

(define (goops-code-to-create-object object)
  (let ((class (class-of object)))
    `(make ,(class-name class))))
 
(define (goops-code-generator-to-assign-values-to-slots object)
  (lambda (name)
    (let ((class (class-of object)))
      (map (match-lambda((slot-name . info)
			 (let ((value (slot-ref object slot-name)))
			   `(slot-set! ,name ',slot-name ,(object-source value)))))
	   (class-slots class)))))

(define (object-source value)
  (cond 
   ((symbol? value)
    `',value)
   ((procedure? value)
    (or (procedure-name value) (procedure-source* value) 'noop))
   ((list? value)
    `(list ,@(map object-source value)))
   ((vector? value) 
    (vector-source value))
   ((hash-table? value)
    (hash-source value))
   ((port? value)
    (port-source value))
   ((is-a? value <image>)
    `(array->image ,(image->array value)))
   ((is-a? value <font>)
    '*default-font*)
   ((instance? value)
    (goops-symbol value))
   (#t value)))

(define (complete-source value)
  (with-fluids ((objects=>symbols (make-hash-table)))
    ;; we need to make entries for all the goops objects that appear in the
    ;; slots of this object, so we call the goops-code-generator... (maybe
    ;; this isn't too pretty, but it's ok for now)
    ((goops-code-generator-to-assign-values-to-slots value) (goops-symbol value))
    `(let ,(map (match-lambda ((object . symbol)
			       `(,symbol ,(goops-code-to-create-object object))))
		(hash-map->alist (fluid-ref objects=>symbols)))
       ,@(append-map (match-lambda((object . symbol)
				   ((goops-code-generator-to-assign-values-to-slots object) symbol)))
		     (hash-map->alist (fluid-ref objects=>symbols)))
       ,#[ objects=>symbols value ])))

(define (save file)
  (with-output-to-port *stdout*
    (lambda()
      ;(display (string-append "saving to file "file":\n"))
      
      (let ((kdn (key-bindings 'pressed))
	    (kup (key-bindings 'released))
	    (dump-key 
	     (lambda (table proc-name i)
	       (or (and-let* ((f #[ table i ])
			      ((not (in? f `(noop ,noop))))
			      ((procedure? f)))
		     `((,proc-name ,#[ *key-names* i ] 
				   ,(or (procedure-source* f)
					(procedure-name f) noop))))
		   '()))))
	(pretty-print
	 (cons 'begin (append-map (lambda(i)
				    (append (dump-key kdn 'keydn i)
					    (dump-key kup 'keyup i)))
				  (iota (vector-length kdn))))))
      (pretty-print `(mousemove ,(procedure-source* (mousemove-binding))))
      (pretty-print `(define *stage* ,(complete-source *stage*))))))


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
  (list #[ w 'x ] #[ w 'y ] #[ w 'w ] #[ w 'h ]))

(define-method (draw (w <widget>))
  (for-each draw (reverse #[ w 'children ])))

(define-method (add-child! (parent <widget>) (child <widget>))
  (set! #[ parent 'children ] (cons child #[ parent 'children ]))
  (set! #[ parent 'w ] (max #[ parent 'w ] (+ #[ child 'x ] #[ child 'w ])))
  (set! #[ parent 'h ] (max #[ parent 'h ] (+ #[ child 'y ] #[ child 'h ])))
  (set! #[ child 'parent ] parent))

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
  (draw-image #[ i 'image ] #[ i 'x ] #[ i 'y ]))

(define (make-image image x y)
  (let ((image (make <bitmap> #:image image #:x x #:y y 
		     #:w (image-width image) 
		     #:h (image-height image))))
    (set! #[ image 'drag ]
	  (function (type state x y xrel yrel)		 
	    (set! #[ image 'x ] (+ #[ image 'x ] xrel))
	    (set! #[ image 'y ] (+ #[ image 'y ] yrel))))
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

