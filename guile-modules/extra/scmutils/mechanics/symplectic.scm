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

(define (symplectic-two-form zeta1 zeta2)
  (- (* (momentum zeta2) (coordinate zeta1))
     (* (momentum zeta1) (coordinate zeta2))))


;;; Symplectic test in terms of matrices

(define ((symplectic? C) s)
  (let ((s* (compatible-shape s)))
    (let ((J (s->m s* ((D J-func) s*) s*))
	  (DC (s->m s* ((D C) s) s)))
      (- J (* DC J (m:transpose DC))))))

;#|
;;;; By the way
;
;(define b-state
;  (->H-state
;   't
;   (coordinate-tuple
;    (coordinate-tuple 'x_1 'y_1)
;    (coordinate-tuple 'x_2 'y_2))
;   (momentum-tuple
;    (momentum-tuple 'p_x_1 'p_y_1)
;    (momentum-tuple 'p_x_2 'p_y_2))))
;
;(let* ((s b-state)
;       (s* (compatible-shape s))
;       (A (typical-object (outer-product s s*))))
;  (pe (- (s->m s ((D (Phi* A)) s*) s*)
;	 (m:transpose (s->m s* ((D (Phi A)) s) s)))))
;(matrix-by-rows (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0))
;
;|#

;#|
;(print-expression
; ((symplectic? (F->CT p->r))
;  (->H-state 't
;	     (coordinate-tuple 'r 'phi)
;	     (momentum-tuple 'p_r 'p_phi))))
;(matrix-by-rows (list 0 0 0 0 0)
;                (list 0 0 0 0 0)
;                (list 0 0 0 0 0)
;                (list 0 0 0 0 0)
;                (list 0 0 0 0 0))
;
;
;;;; but not all transforms are
;
;(define (a-non-canonical-transform Istate)
;  (let ((t (time Istate))
;        (theta (coordinate Istate))
;	(p (momentum Istate)))
;    (let ((x (* p (sin theta)))
;	  (p_x (* p (cos theta))))
;      (->H-state t x p_x))))
;
;(print-expression
; ((symplectic? a-non-canonical-transform)
;  (->H-state 't 'theta 'p)))
;(matrix-by-rows (list 0 0 0)
;		(list 0 0 (+ 1 (* -1 p)))
;		(list 0 (+ -1 p) 0))
;
;(print-expression
; ((symplectic? (polar-canonical 'alpha))
;  (->H-state 't 'a 'I)))
;(matrix-by-rows (list 0 0 0)
;		(list 0 0 0)
;		(list 0 0 0))
;
;(define (Cmix H-state)
;  (let ((t (time H-state))
;	(q (coordinate H-state))
;	(p (momentum H-state)))
;    (->H-state t
;	       (coordinate-tuple (ref q 0) (- (ref p 1)))
;	       (momentum-tuple   (ref p 0) (ref q 1)))))
;
;(define a-state (->H-state 't 
;			   (coordinate-tuple 'x 'y)
;			   (momentum-tuple 'p_x 'p_y)))
;
;(print-expression
; ((symplectic? Cmix)
;  a-state))
;(matrix-by-rows (list 0 0 0 0 0)
;                (list 0 0 0 0 0)
;                (list 0 0 0 0 0)
;                (list 0 0 0 0 0)
;                (list 0 0 0 0 0))
;|#

;#|
;(define (Cmix2 H-state)
;  (let ((t (time H-state))
;	(q (coordinate H-state))
;	(p (momentum H-state)))
;    (->H-state t
;	       (flip-outer-index p)
;	       (- (flip-outer-index q)))))
;
;(print-expression
; ((symplectic? Cmix2)
;  a-state))
;(matrix-by-rows (list 0 0 0 0 0)
;                (list 0 0 0 0 0)
;                (list 0 0 0 0 0)
;                (list 0 0 0 0 0)
;                (list 0 0 0 0 0))
;
;
;(define ((C m0 m1) state)
;  (let ((x (coordinate state))
;	(p (momentum state)))
;    (let ((x0 (ref x 0))
;	  (x1 (ref x 1))
;	  (p0 (ref p 0))
;	  (p1 (ref p 1)))
;      (->H-state 
;       (time state)
;       (coordinate-tuple (/ (+ (* m0 x0) (* m1 x1)) (+ m0 m1))
;			 (- x1 x0))
;       (momentum-tuple (+ p0 p1)
;		       (/ (- (* m0 p1) (* m1 p0))
;			  (+ m0 m1)))))))
;
;(define b-state
;  (->H-state 't
;	     (coordinate-tuple
;	      (coordinate-tuple 'x_1 'y_1)
;	      (coordinate-tuple 'x_2 'y_2))
;	     (momentum-tuple
;	      (momentum-tuple 'p_x_1 'p_y_1)
;	      (momentum-tuple 'p_x_2 'p_y_2))))
;
;(print-expression
; ((symplectic? (C 'm1 'm2)) b-state))
;(matrix-by-rows (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0)
;                (list 0 0 0 0 0 0 0 0 0))
;|#

;;; qp version below.

(define ((symplectic-transform? C) arg)
  (symplectic-matrix?
   (qp-submatrix
    (s->m (compatible-shape arg)
	  ((D C) arg)
	  arg))))

(define (qp-submatrix m)
  (m:submatrix m 1 (m:num-rows m) 1 (m:num-cols m)))


(define (symplectic-matrix? M)
  (let ((2n (m:dimension M)))
    (if (not (even? 2n))
	(error "Wrong type -- SYMPLECTIC-MATRIX?" M))
    (let ((J (symplectic-unit (quotient 2n 2))))
      (- J (* M J (m:transpose M))))))

(define (symplectic-unit n)
  (let ((2n (fix:* 2 n)))
    (m:generate 2n 2n
       (lambda (a b) 
	 (cond ((fix:= (fix:+ a n) b) 1)
	       ((fix:= (fix:+ b n) a) -1)
	       (else 0))))))

;#|
;;;; For example, point transforms are canonical
;
;(print-expression
; ((symplectic-transform? (F->CT p->r))
;  (->H-state 't
;	     (coordinate-tuple 'r 'theta)
;	     (momentum-tuple 'p_r 'p_theta))))
;(matrix-by-rows (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0))
;|#

;#|
;(print-expression
; ((symplectic-transform? a-non-canonical-transform)
;  (->H-state 't 'theta 'p)))
;(matrix-by-rows (list 0 (+ 1 (* -1 p))) (list (+ -1 p) 0))
;|#

;#|
;;;; One particularly useful canonical transform is the 
;;;;  Poincare transform, which is good for simplifying 
;;;;  oscillators.
;
;(define ((polar-canonical alpha) Istate)
;  (let ((t (time Istate))
;        (theta (coordinate Istate))
;        (I (momentum Istate)))
;    (let ((x (* (sqrt (/ (* 2 I) alpha)) (sin theta)))
;	  (p_x (* (sqrt (* 2 alpha I)) (cos theta))))
;      (->H-state t x p_x))))
;
;(define ((polar-canonical-inverse alpha) s)
;  (let ((t (time s))
;	(x (coordinate s))
;	(p (momentum s)))
;    (let ((I (/ (+ (* alpha (square x))
;		   (/ (square p) alpha))
;		2)))
;      (let ((theta (atan (/ x (sqrt (/ (* 2 I) alpha)))
;			 (/ p (sqrt (* 2 I alpha))))))
;	(->H-state t theta I)))))
;
;
;
;(pe
; ((compose (polar-canonical-inverse 'alpha)
;	   (polar-canonical 'alpha))
;  (->H-state 't 'x 'p)))
;(up t x p)
;
;|#

;#|
;;;; It is clearly canonical.
;
;(print-expression
; ((symplectic-transform? (polar-canonical 'alpha))
;  (->H-state 't 'a 'I)))
;(matrix-by-rows (list 0 0) (list 0 0))
;|#
