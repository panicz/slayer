#include <SDL/SDL.h>
#include "extend.h"

SCM stage;

void widgets_init(Uint16 w, Uint16 h) {
  eval("(use-modules (oop goops)"
       "  (ice-9 match))");
  eval("(define-generic update)");
  eval("(define-generic draw)");
  eval("(define-generic area)");
  eval("(define-class <widget> ()"
       "  (parent #:init-value #f #:init-keyword #:parent)"
       "  (children #:init-value '() #:init-keyword #:children)"
       "  (x #:init-value 0 #:init-keyword #:x)"
       "  (y #:init-value 0 #:init-keyword #:y)"
       "  (w #:init-value 0 #:init-keyword #:w)"
       "  (h #:init-value 0 #:init-keyword #:h))");
  evalf("(define *stage* (make <widget> #:w %i #:h %i))", w, h);
  eval("(define-method (area (w <widget>))"
       "  (list (slot-ref w 'x) (slot-ref w 'y) (slot-ref w 'w) (slot-ref w 'h)))");
  eval("(define-method (draw (w <widget>))"
       "  (for-each draw (slot-ref w 'children)))");
  eval("(define (in-area? point area)"
       "  (match-let"
       "    (((x y w h) area)"
       "     ((px py) point))"
       "       (and (<= x px w) (<= y py h))))");
  stage = eval("*stage*");
}
