#include <SDL/SDL.h>
#include "extend.h"

SCM stage;

void widgets_init(Uint16 w, Uint16 h) {
  eval("(use-modules (oop goops)"
       "  (srfi srfi-1) (srfi srfi-2)"
       "  (ice-9 match))");
  eval("(define-generic update!)");
  eval("(define-generic draw)");
  eval("(define-generic area)");
  eval("(define-generic add-child!)");
  eval("(define-class <widget> ()"
       "  (parent #:init-value #f #:init-keyword #:parent)"
       "  (children #:init-value '() #:init-keyword #:children)"
       "  (click #:init-value noop #:init-keyword #:click)"
       "  (drag #:init-value noop #:init-keyword #:drag)"
       "  (update!  #:init-value noop #:init-keyword #:update)"
       "  (x #:init-value 0 #:init-keyword #:x)"
       "  (y #:init-value 0 #:init-keyword #:y)"
       "  (w #:init-value 0 #:init-keyword #:w)"
       "  (h #:init-value 0 #:init-keyword #:h))");

  evalf("(define *stage* (make <widget> #:w %i #:h %i))", w, h);

  evalf("(slot-set! *stage* 'click"
	"  (lambda e (display e)(newline)))");

  eval("(define-method (area (w <widget>))"
       "  (list (slot-ref w 'x) (slot-ref w 'y) (slot-ref w 'w) (slot-ref w 'h)))");

  eval("(define-method (draw (w <widget>))"
       "  (for-each draw (slot-ref w 'children)))");

  eval("(define-method (add-child! (parent <widget>) (child <widget>))"
       "  (slot-set! parent 'children (cons child (slot-ref parent 'children)))"
       "  (slot-set! child 'parent parent))");

  eval("(define (in-area? point area)"
       "  (match-let"
       "    (((x y w h) area)"
       "     ((px py) point))"
       "       (and (<= x px (+ x w)) (<= y py (+ y h)))))");

  eval("(define (widget-nested-find condition widget)"
       "  (if (not (condition widget))"
       "      #f"
       "      (let ((w (find condition (slot-ref widget 'children))))"
       "	(if (not w)"
       "	    widget"
       "	    (let ((c (widget-nested-find condition w)))"
       "	      (if (not c) w c))))))");

  eval("(define-class <image> (<widget>)"
       "  (image #:init-keyword #:image))");

  eval("(define-method (draw (i <image>))"
       "  (draw-image (slot-ref i 'image) (slot-ref i 'x) (slot-ref i 'y)))");

  eval("(define *active-widget* *stage*)");

  stage = eval("*stage*");
}
