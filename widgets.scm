(use-modules (oop goops)
	     (srfi srfi-1) (srfi srfi-2)
	     (ice-9 match))
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

