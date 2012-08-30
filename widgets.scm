(use-modules (oop goops)
	     (srfi srfi-1) (srfi srfi-2)
	     (ice-9 match))


(define *stdout* (current-output-port))
(define *stdin* (current-input-port))
(define *stderr* (current-error-port))


(define *stdio* 
  (make-soft-port 
   (vector 
    (lambda (c) (write (char-upcase c) *stdout*)) ; procedure accepting one character for output
    (lambda (s) (display (string-upcase s) *stdout*)) ; procedure accepting a string for output
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
(set-font-style! *default-font* 1)

(define-class <text-area> (<widget>)
  (lines #:init-value '#(""))
  ;(cursor-position #:init-value '(0 0))
  (font #:init-value *default-font* #:init-keyword #:font)
  (port #:init-value *stdio*)
  (visible-cols #:init-keyword #:visible-cols)
  (visible-lines #:init-keyword #:visible-lines)
  (rendered-lines #:init-value #f))

(define-method (draw (t <text-area>))
  (let* ((font (slot-ref t 'font))
	 (line-skip (font-line-skip font))
	 (lines (slot-ref t 'lines)))
    (let ((x (slot-ref t 'x))
	  (y (slot-ref t 'y)))
      (for-each (lambda(line top)
		  (let ((image (render-text line font)))
		    (slot-set! t 'w (max (slot-ref t 'w) (image-width image)))
		    (draw-image image x (+ y top))))
		(vector->list lines)
		(iota (vector-length lines) 0 line-skip)))
    (slot-set! t 'h (* (vector-length lines) line-skip))))

(define-method (move-cursor! (w <text-area>)
			     (right <integer>)
			     (down <integer>))
  (let ((p (slot-ref w 'port))
	(L (slot-ref w 'lines)))
    (set-port-line! p (max 0 (min (+ (port-line p) down) 
				  (vector-length L))))
    (set-port-column! p (max 0 (min (+ (port-column p) right)
				    (string-length (vector-ref L (port-line p))))))))

(define-method (delete-char! (w <text-area>))
  (let ((p (slot-ref w 'port)))
    (delete-char! w (port-column p) (port-line p))))

(define-method (delete-char! (w <text-area>) (col <integer>) (line <integer>))
  (let* ((s (vector-ref (slot-ref w 'lines) line))
	 (sl (string-length s))
	 (l (substring s 0 (max 0 (min sl (- col 1)))))
	 (r (substring s (min sl col))))
    (vector-set! (slot-ref w 'lines) line (string-append l r))))

(define-generic input-text!)


(define *input-widget* #f)
  
