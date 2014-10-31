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

;#|
;;;; Generalize to rank k.
;(define ((Lie-derivative X) Y)
;  (vector-field? X)
;  (assert (form-field? Y))
;  (procedure->1form-field
;   (lambda (Z)
;     (- ((Lie-derivative X) (Y Z))
;	(Y ((Lie-derivative X) Z))))
;   'foo))
;|#

(define (Lie-derivative X)
  (cond ((function? X)			;compatability with SICM Hamiltonian
	 (make-operator
	  (lambda (F)
	    (assert (function? F))
	    (Poisson-bracket F X))
	  `(Lie-derivative ,X)))
	((vector-field? X)
	 (make-operator
	  (lambda (Y)
	    (cond
	     ((function? Y) (X Y))
	     ((vector-field? Y) (commutator X Y))
	     ((form-field? Y)		; Hawking & Ellis p.29
	      (let ((k (get-rank Y)))
		(procedure->nform-field
		 (lambda vectors
		   (assert (= k (length vectors)))
		   (- ((Lie-derivative X) (apply Y vectors))
		      (sigma (lambda (i)
			       (apply Y
				      (list-with-substituted-coord vectors i
					  ((Lie-derivative X)
					   (list-ref vectors i)))))
			     0 (- k 1))))
		 `((Lie-derivative ,(diffop-name X))
		   ,(diffop-name Y))
		 k)))
	     (else
	      (error "Bad input -- Lie-derivative"))))
	  `(Lie-derivative ,X)))
	(else
	 (error "Bad input -- Lie-derivative"))))

;#|
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;(define R3-point ((R3 '->point) (up 'x0 'y0 'z0)))
;
;(define w (literal-1form-field 'w R3))
;(define u (literal-1form-field 'u R3))
;(define v (literal-1form-field 'v R3))
;
;(define X (literal-vector-field 'X R3))
;(define Y (literal-vector-field 'Y R3))
;(define Z (literal-vector-field 'Z R3))
;(define W (literal-vector-field 'W R3))
;
;(define f (literal-scalar-field 'f R3))
;
;(clear-arguments)
;(suppress-arguments (list '(up x0 y0 z0)))
;
;(pec ((((Lie-derivative X) w) Y) R3-point)
;     (compose arg-suppressor simplify))
;#| Result:
;(+ (* ((partial 0) w_0) X^0 Y^0)
;   (* ((partial 0) w_1) X^0 Y^1)
;   (* ((partial 0) w_2) X^0 Y^2)
;   (* ((partial 1) w_0) X^1 Y^0)
;   (* ((partial 1) w_1) X^1 Y^1)
;   (* ((partial 1) w_2) X^1 Y^2)
;   (* ((partial 2) w_0) X^2 Y^0)
;   (* ((partial 2) w_1) X^2 Y^1)
;   (* ((partial 2) w_2) X^2 Y^2)
;   (* ((partial 0) X^0) w_0 Y^0)
;   (* ((partial 1) X^0) w_0 Y^1)
;   (* ((partial 2) X^0) w_0 Y^2)
;   (* ((partial 0) X^1) w_1 Y^0)
;   (* ((partial 1) X^1) w_1 Y^1)
;   (* ((partial 2) X^1) w_1 Y^2)
;   (* ((partial 0) X^2) Y^0 w_2)
;   (* ((partial 1) X^2) Y^1 w_2)
;   (* ((partial 2) X^2) w_2 Y^2))
;|#
;
;
;(pec ((- ((d ((Lie-derivative X) f)) Y)
;	 (((Lie-derivative X) (d f)) Y) )
;      R3-point)
;     (compose arg-suppressor simplify))
;#| Result:
;0
;|#
;
;(pec ((- ((d ((Lie-derivative X) w)) Y Z)
;	 (((Lie-derivative X) (d w)) Y Z) )
;      ((R3 '->point) (up 'x^0 'y^0 'z^0)))
;     (compose arg-suppressor simplify))
;#| Result:
;0
;|#
;|#

;#|
;is this correct?
;
;let phi_t(x) be the integral curve of V from x for interval t
;
;L_V Y (f) (x) = lim_t->0 ( Y(f) (phi_t (x)) - (d phi_t)(Y)(f)(x))/t
;              = D (lambda (t) 
;                       ( Y(f) (phi_t (x)) - (d phi_t)(Y)(f)(x)))
;                (t=0)
;so let g(t) = ( Y(f) (phi_t (x)) - (d phi_t)(Y)(f)(x))
;            = ( Y(f) (phi_t (x)) - Y(f circ phi_t)(x))
;
;we only need linear terms in phi_t(x) = (I + t v(I))(x)
;
;       g(t) = ( Y(f) ((I + t v(I))(x)) - Y(f circ (I + t v(I)))(x))
;
;(define ((((Lie-test V) Y) f) x)
;  (define (g t)
;    (- ((Y f) ((+ identity (* t (V identity))) x))
;       ((Y (compose f (+ identity (* t (V identity))))) x)))
;  ((D g) 0))
;
;(define R2 (rectangular 2))
;(instantiate-coordinates R2 '(x y))
;
;(define X (literal-vector-field 'X R2))
;(define Y (literal-vector-field 'Y R2))
;
;(define f (literal-scalar-field 'f R2))
;
;(clear-arguments)
;(suppress-arguments (list '(up x0 y0)))
;
;(pec ((((Lie-derivative X) Y) f) 
;      ((R2 '->point) (up 'x0 'y0)))
;    (compose arg-suppressor simplify))
;#| Result:
;(+ (* ((partial 0) Y^0) X^0 ((partial 0) f))
;   (* ((partial 0) Y^1) X^0 ((partial 1) f))
;   (* ((partial 1) Y^0) X^1 ((partial 0) f))
;   (* ((partial 1) Y^1) X^1 ((partial 1) f))
;   (* -1 ((partial 0) X^0) Y^0 ((partial 0) f))
;   (* -1 ((partial 0) X^1) Y^0 ((partial 1) f))
;   (* -1 ((partial 1) X^0) ((partial 0) f) Y^1)
;   (* -1 ((partial 1) X^1) Y^1 ((partial 1) f)))
;|#
;
;(pec (- ((((Lie-test X) Y) f) 
;	 ((R2 '->point) (up 'x0 'y0)))
;	((((Lie-derivative X) Y) f) 
;	 ((R2 '->point) (up 'x0 'y0)))))
;#| Result:
;0
;|#
;
;(clear-arguments)
;|#

