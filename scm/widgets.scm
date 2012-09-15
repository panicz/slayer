(set! %load-path (cons "./" %load-path))

(use-modules (oop goops)
	     (srfi srfi-1) 
	     (srfi srfi-2)
	     (srfi srfi-11)
	     (ice-9 match)
	     (extra ref)
	     (extra vector-lib)
	     ;(extra overload)
	     )

(define *stdout* (current-output-port))
(define *stdin* (current-input-port))
(define *stderr* (current-error-port))

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

(define-class <text-area> (<widget>)
  (lines #:init-value '#(""))
  (special-keys #:init-thunk (lambda()(make-vector (vector-length *key-names*) noop)))
  ;(cursor-position #:init-value '(0 0))
  (font #:init-value *default-font* #:init-keyword #:font)
  (port #:init-value *stdio*)
  (visible-cols #:init-keyword #:visible-cols)
  (visible-lines #:init-keyword #:visible-lines)
  (rendered-lines #:init-thunk make-hash-table))


(define-method (draw (t <text-area>))
  (let* ((font #[t 'font])
	 (line-skip (font-line-skip font))
	 (lines #[t 'lines])
	 (space (render-text "_" font))
	 (cursor (rectangle 2 line-skip #x20eeaa22)))
      (for-each (lambda(line top)
		  (let ((image (render-text line font)))
		    (set! #[t 'w] (max #[t 'w] (image-width image)))
		    (draw-image image #[t 'x] (+ #[t 'y] top))))
		(vector->list lines)
		(iota (vector-length lines) 0 line-skip))
      (if (equal? (current-output-port) #[t 'port])
	  (draw-image cursor 
		      (* (image-width space) (port-column #[t 'port])) 
		      (* line-skip (port-line #[t 'port]))))
    (set! #[t 'h] (* (vector-length lines) line-skip))))

(define-method (move-cursor! (w <text-area>)
			     (right <integer>)
			     (down <integer>))
  (set-port-line! #[w 'port] (max 0 (min (+ (port-line #[w 'port]) down) 
				(- (vector-length #[w 'lines]) 1))))
  (set-port-column! #[w 'port] (max 0 (min (+ (port-column #[w 'port]) right)
				  (string-length #[#[w 'lines] (port-line #[w 'port])])))))

(define-method (delete-char! (w <text-area>))
  (let ((p (slot-ref w 'port)))
    (delete-char! w (port-column p) (port-line p))))

(define-method (delete-char! (w <text-area>) (col <integer>) (line <integer>))
  (let* ((s #[#[w 'lines] line])
	 (sl (string-length s))
	 (l (substring s 0 (max 0 (min sl (- col 1)))))
	 (r (substring s (min sl col))))
    (set! #[#[w 'lines] line] (string-append l r))))

(define-generic input-text!)

(define *input-widget* #f)
