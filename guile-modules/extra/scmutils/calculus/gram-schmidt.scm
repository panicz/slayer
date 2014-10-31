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

;;; Gram-Schmidt orthonormalization process...

(define (Gram-Schmidt vector-basis metric)
  (define (make-positive x)
    (sqrt (square x)))
  (define (normalize v)
    (* (/ 1 (sqrt (make-positive (metric v v)))) v))
  (let* ((vects (ultra-flatten vector-basis))
	 (e0 (normalize (car vects))))
    (let lp ((ins (cdr vects)) (outs (list e0)))
      (if (null? ins)
	  (apply down (reverse outs))
	  (lp (cdr ins)
	      (cons (normalize
		     (- (car ins)			   
			(apply +
			       (map (lambda (outv)
				      (* (metric (car ins) outv)
					 outv))
				    outs))))
		    outs))))))

;#|
;;;; Orthonormalizing with respect to the Lorentz metric in 2 dimensions. 
;(define R2 (rectangular 2))
;(instantiate-coordinates R2 '(t x))
;(define R2-point ((R2 '->point) (up 't0 'x0)))
;(define R2-basis (coordinate-system->basis R2))
;
;(define ((L2-metric c) u v)
;  (+ (* -1 c c (dt u) (dt v))
;     (* 1 (dx u) (dx v))))
;
;(define L2-vector-basis
;  (Gram-Schmidt (basis->vector-basis R2-basis) (L2-metric 'c)))
;
;(s:foreach (lambda (v)
;	     (pe ((v (literal-manifold-function 'f R2))
;		  R2-point)))
;	   L2-vector-basis)
;(/ (((partial 0) f) (up t0 x0)) c)
;(((partial 1) f) (up t0 x0))
;
;(s:foreach (lambda (omega)
;	     (pe ((omega (literal-vector-field 'v R2))
;		  R2-point)))
;	   (vector-basis->dual L2-vector-basis R2))
;(* c (v^0 (up t0 x0)))
;(v^1 (up t0 x0))
;|#

;#|
;;;; 4-dimensional Lorentz metric.
;
;(define SR (rectangular 4))
;(instantiate-coordinates SR '(t x y z))
;
;(define ((g-Lorentz c) u v)
;  (+ (* (dx u) (dx v))
;     (* (dy u) (dy v))
;     (* (dz u) (dz v))
;     (* -1 (square c) (dt u) (dt v))))
;
;(define SR-basis (coordinate-system->basis SR))
;
;(define an-event ((SR '->point) (up 't0 'x0 'y0 'z0)))
;
;(define SR-V (basis->vector-basis SR-basis))
;
;(define SR-V1 (ultra-flatten (Gram-Schmidt SR-V (g-Lorentz 'c))))
;
;;;; SR-V1 is orthogonal
;(for-each (lambda (v1)
;	    (for-each (lambda (v2)
;			(pe (((g-Lorentz 'c) v1 v2) an-event)))
;		      (cdr (memq v1 SR-V1))))
;	  SR-V1)
;0
;0
;0
;0
;0
;0
;
;;;; SR-V1 is normal
;(for-each (lambda (v)
;	    (pe (((g-Lorentz 'c) v v) an-event)))
;	  SR-V1)
;-1
;1
;1
;1
;
;(for-each (lambda (v)
;	    (pe ((v (SR '->coords))
;		 an-event)))
;	  SR-V1)
;(up (/ 1 c) 0 0 0)
;(up 0 1 0 0)
;(up 0 0 1 0)
;(up 0 0 0 1)
;|#

;#|
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;(define R3-point ((R3 '->point) (up 'x0 'y0 'z0)))
;(define R3-basis (coordinate-system->basis R3))
;
;(define ((g3-maker a b c d e f) v1 v2)
;  (+ (* a (dx v1) (dx v2))
;     (* b (dx v1) (dy v2))
;     (* c (dx v1) (dz v2))
;     (* b (dx v2) (dy v1))
;     (* d (dy v1) (dy v2))
;     (* e (dy v1) (dz v2))
;     (* c (dx v2) (dz v1))
;     (* e (dy v2) (dz v1))
;     (* f (dz v1) (dz v2))))
;
;(define g3 (g3-maker 'a 'b 'c 'd 'e 'f))
;
;(for-each (lambda (v)
;	    (pe ((v (R3 '->coords))
;		 R3-point)))
;	  (ultra-flatten
;	   (Gram-Schmidt
;	    (basis->vector-basis R3-basis)
;	    g3)))
;(up (/ 1 (sqrt a)) 0 0)
;(up (/ (* -1 b) (sqrt (+ (* (expt a 2) d) (* -1 a (expt b 2)))))
;    (/ a (sqrt (+ (* (expt a 2) d) (* -1 a (expt b 2)))))
;    0)
;;;; Third one is something awful...
;|#

;;; This may be needed... ugh!

(define (completely-antisymmetric indices)
  (permutation-parity indices (iota (length indices))))


