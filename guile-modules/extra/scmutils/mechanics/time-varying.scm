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

;;; This is used in conjunction with a symplectic test for the C to
;;; establish that a time-dependent transformation is canonical.

;;; To compute the K (addition to the Hamiltonian) from a
;;; time-dependent coordinate transformation F.

(define ((F->K F) s)
  (* -1 (((partial 0) F) s) (momentum ((F->CT F) s))))


;;; Tests that K yields a canonical transformation if the C is
;;; symplectic. 

(define ((canonical-K? C K) s)
  (let ((s* (compatible-shape s)))
    (- (T-func s*)
       (+ (* ((D C) s) (J-func ((D K) s)))
	  (((partial 0) C) s)))))

;#|
;(define ((canonical-K? C K) s)
;  (let ((DCs ((D C) s))
;	(s* (compatible-shape s)))
;    (- (T-func s*)
;       (* DCs ((phase-space-derivative K) s)))))
;|#

;#|
;(define ((rotating n) state)
;  (let ((t (time state))
;	(q (coordinate state)))
;    (let ((x (ref q 0))
;	  (y (ref q 1))
;	  (z (ref q 2)))
;      (coordinate-tuple (+ (* (cos (* n t)) x) (* (sin (* n t)) y))
;			(- (* (cos (* n t)) y) (* (sin (* n t)) x))
;			z))))
;
;(define (C-rotating n) (F->CT (rotating n)))
;
;(define ((K n) s)
;  (let ((q (coordinate s))
;	(p (momentum s)))
;    (let ((x (ref q 0)) (y (ref q 1))
;	  (px (ref p 0)) (py (ref p 1)))
;      (* n (- (* x py) (* y px))))))
;
;(define a-state 
;  (->H-state 't 
;	     (coordinate-tuple 'x 'y 'z)
;	     (momentum-tuple 'p_x 'p_y 'p_z)))
;
;
;(pe ((canonical-K? (C-rotating 'n) (K 'n)) a-state))
;(up 0 (up 0 0 0) (down 0 0 0))
;
;;;; or getting K directly from F
;(pe ((canonical-K? (C-rotating 'n) (F->K (rotating 'n))) a-state))
;(up 0 (up 0 0 0) (down 0 0 0))
;
;(pe ((- (F->K (rotating 'n))
;	(K 'n))
;     a-state))
;0
;
;;;; not all K's work
;
;(define ((bad-K n) s)
;  (- ((K n) s)))
;
;(pe ((canonical-K? (C-rotating 'n) (bad-K 'n)) a-state))
;(up
; 0
; (up (+ (* 2 n x (sin (* n t))) (* -2 n y (cos (* n t))))
;     (+ (* 2 n x (cos (* n t))) (* 2 n y (sin (* n t))))
;     0)
; (down (+ (* 2 n p_x (sin (* n t))) (* -2 n p_y (cos (* n t))))
;       (+ (* 2 n p_x (cos (* n t))) (* 2 n p_y (sin (* n t))))
;       0))
;|#
