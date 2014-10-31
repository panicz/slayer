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

;;;; Metrics 

;;; A metric is a function that takes two vector fields and produces a
;;; function on the manifold.

;#|
;(set! *divide-out-terms* #f)
;(set! *factoring* #t)
;
;;;; Example: natural metric on a sphere of radius R
;
;(define 2-sphere (rectangular 2))
;(instantiate-coordinates 2-sphere '(theta phi))
;
;(define ((g-sphere R) u v)
;  (* (square R)
;     (+ (* (dtheta u) (dtheta v))
;	(* (compose (square sin) theta)
;	   (dphi u)
;	   (dphi v)))))
;
;(define u (literal-vector-field 'u 2-sphere))
;(define v (literal-vector-field 'v 2-sphere))
;
;(pec (((g-sphere 'R) u v)
;      ((2-sphere '->point) (up 'theta0 'phi0))))
;#| Result:
;(* (+ (* (v^0 (up theta0 phi0))
;	 (u^0 (up theta0 phi0)))
;      (* (expt (sin theta0) 2)
;	 (v^1 (up theta0 phi0))
;	 (u^1 (up theta0 phi0))))
;   (expt R 2))
;|#
;
;;;; Example: Lorentz metric on R^4
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
;
;;;; Example: general metric on R^2
;
;(define R2 (rectangular 2))
;(instantiate-coordinates R2 '(x y))
;(define R2-basis (coordinate-system->basis R2))
;
;(define ((g-R2 g_00 g_01 g_11) u v)
;  (+ (* g_00 (dx u) (dx v))
;     (* g_01 (+ (* (dx u) (dy v)) (* (dy u) (dx v))))
;     (* g_11 (dy u) (dy v))))
;
;(pec (((g-R2 'a 'b 'c)
;       (literal-vector-field 'u R2)
;       (literal-vector-field 'v R2))
;      ((R2 '->point) (up 'x0 'y0))))
;#| Result:
;(+ (* (u^0 (up x0 y0)) (v^0 (up x0 y0)) a)
;   (* (+ (* (v^0 (up x0 y0)) (u^1 (up x0 y0)))
;	 (* (u^0 (up x0 y0)) (v^1 (up x0 y0))))
;      b)
;   (* (v^1 (up x0 y0)) (u^1 (up x0 y0)) c))
;|#
;|#

(define ((metric->coefs metric basis) m)
  (let ((vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis)))
    (s:map/r (lambda (e_i)
	       (s:map/r (lambda (e_j)
			  ((metric e_i e_j) m))
			vector-basis))
	     vector-basis)))


;;; Given a metric and a basis, to compute the inverse metric

(define (metric->inverse-coeffs metric basis)
  (define (the-coeffs m)
    (let ((g_ij ((metric->coefs metric basis) m))
	  (1form-basis (basis->1form-basis basis)))
      (let ((g^ij
	     (s:inverse (typical-object 1form-basis)
			g_ij
			(typical-object 1form-basis))))
	 g^ij)))
  the-coeffs)    

(define (((metric:invert metric basis) w1 w2) m)
  (let ((vector-basis (basis->vector-basis basis))
	(g^ij ((metric->inverse-coeffs metric basis) m)))
    (* (* g^ij ((s:map/r w1 vector-basis) m))
       ((s:map/r w2 vector-basis) m))))

;#|
;(pec (((metric:invert (g-R2 'a 'b 'c) R2-basis)
;       (literal-1form-field 'omega R2)
;       (literal-1form-field 'theta R2))
;      ((R2 '->point) (up 'x0 'y0))))
;#| Result:
;(/ (+ (* (omega_1 (up x0 y0)) (theta_1 (up x0 y0)) a)
;      (* (+ (* -1 (theta_1 (up x0 y0)) (omega_0 (up x0 y0)))
;	    (* -1 (omega_1 (up x0 y0)) (theta_0 (up x0 y0))))
;	 b)
;      (* (theta_0 (up x0 y0)) (omega_0 (up x0 y0)) c))
;   (+ (* c a) (* -1 (expt b 2))))
;|#
;
;;;; Test of inversion
;
;(pec
; (let* ((g (g-R2 'a 'b 'c))
;	(gi (metric:invert g R2-basis))
;	(vector-basis (list d/dx d/dy))
;	(dual-basis (list dx dy))
;	(m ((R2 '->point) (up 'x0 'y0))))
;   (matrix:generate 2 2
;     (lambda (i k)
;       (sigma (lambda (j)
;		(* ((gi (ref dual-basis i) (ref dual-basis j)) m)
;		   ((g  (ref vector-basis j) (ref vector-basis k)) m)))
;	      0 1)))))
;#| Result:
;(matrix-by-rows (list 1 0) (list 0 1))
;|#
;|#

;;; Raising and lowering indices...

(define ((vector-field->1form-field metric) u)
  (define (omega v)
    (metric v u))
  (procedure->1form-field omega
    `(lower ,(diffop-name u)
	    ,(diffop-name metric))))

(define lower vector-field->1form-field)


(define (1form-field->vector-field metric basis)
  (let ((gi (metric:invert metric basis)))
    (let ((vector-basis (basis->vector-basis basis))
	  (1form-basis (basis->1form-basis basis)))
      (define (proc omega)
	(s:sigma/r (lambda (e~i e_i)
		     (* (gi omega e~i) e_i))
		   1form-basis
		   vector-basis))
      proc)))
  
(define raise 1form-field->vector-field)

;;; Note: raise needs an extra argument -- the coordinate system -- why?

;#|
;(pec
; ((((lower (g-R2 'a 'b 'c))
;    (literal-vector-field 'v R2))
;   (literal-vector-field 'w R2))
;  ((R2 '->point) (up 'x0 'y0))))
;#| Result:
;(+ (* (w^0 (up x0 y0)) (v^0 (up x0 y0)) a)
;   (* (+ (* (v^0 (up x0 y0)) (w^1 (up x0 y0)))
;	 (* (w^0 (up x0 y0)) (v^1 (up x0 y0))))
;      b)
;   (* (v^1 (up x0 y0)) (w^1 (up x0 y0)) c))
;|#
;
;(pec
; ((((raise (g-R2 'a 'b 'c) R2-basis)
;    ((lower (g-R2 'a 'b 'c)) (literal-vector-field 'v R2)))
;   (compose (literal-function 'w (-> (UP Real Real) Real))
;	    (R2 '->coords)))
;  ((R2 '->point) (up 'x0 'y0))))
;#| Result:
;(+ (* (v^0 (up x0 y0)) (((partial 0) w) (up x0 y0)))
;   (* (v^1 (up x0 y0)) (((partial 1) w) (up x0 y0))))
;|#
;|#

;;; Unfortunately raise is very expensive because the matrix is
;;; inverted for each manifold point.

(define (sharpen metric basis m)
  (let ((g^ij ((metric->inverse-coeffs metric basis) m))
	(vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis)))
    (define (sharp 1form-field)
      (let ((1form-coeffs
	     (s:map/r (lambda (ei) ((1form-field ei) m))
		      vector-basis)))
	(let ((vector-coeffs (* g^ij 1form-coeffs)))
	  (s:sigma/r * vector-coeffs vector-basis))))
    sharp))
    
;#|
;(pec
; ((((sharpen (g-R2 'a 'b 'c) R2-basis ((R2 '->point) (up 'x0 'y0)))
;    ((lower (g-R2 'a 'b 'c)) (literal-vector-field 'v R2)))
;   (compose (literal-function 'w (-> (UP Real Real) Real))
;	    (R2 '->coords)))
;  ((R2 '->point) (up 'x0 'y0))))
;
;#| Result:
;(up (* (v^0 (up x0 y0)) (((partial 0) w) (up x0 y0)))
;    (* (v^1 (up x0 y0)) (((partial 1) w) (up x0 y0))))
;|#
;|#