;#| -*-Scheme-*-
;
;$Id: copyright.scm,v 1.4 2005/12/13 06:41:00 cph Exp $
;
;Copyright 2005 Massachusetts Institute of Technology
;
;This file is part of MIT/GNU Scheme.
;
;MIT/GNU Scheme is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 2 of the License, or (at
;your option) any later version.
;
;MIT/GNU Scheme is distributed in the hope that it will be useful, but
;WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with MIT/GNU Scheme; if not, write to the Free Software
;Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;USA.
;
;|#

;;; Makes a canonical point transformation from a 
;;;  time-invariant coordinate transformation T(q)

(define (F->CT F)
  (define (CT H-state)
    (let ((t (time H-state))
          (q (coordinate H-state))
          (p (momentum H-state)))
      (->H-state t
                 (F H-state)
                 (* p (s:inverse 
		       (compatible-shape q)
		       (((partial 1) F) H-state)
		       (compatible-shape p)
		       )))))
  CT)

;#|
;;;; For display in book
;
;(define ((F->CT F) H-state)
;  (->H-state (time H-state)
;             (F H-state)
;             (* (momentum H-state)
;                (invert (((partial 1) F) H-state)))))
;|#
;#|
;
;(define ((H-central m V) state)
;  (let ((x (coordinate state))
;	(p (momentum state)))
;    (+ (/ (square p) (* 2 m))
;       (V (sqrt (square x))))))
;
;(show-expression
; ((compose (H-central 'm (literal-function 'V))
;           (F->CT p->r))
;  (->H-state 't
;             (coordinate-tuple 'r 'phi)
;             (momentum-tuple 'p_r 'p_phi))))
;(+ (V r)
;   (/ (* 1/2 (expt p_r 2)) m)
;   (/ (* 1/2 (expt p_phi 2)) (* m (expt r 2))))
;
;|#

