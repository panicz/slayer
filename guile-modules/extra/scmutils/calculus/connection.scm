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

;;; A metric induces a torsion-free connection			       
	       
(define (metric->Christoffel-1 metric basis)
  (let ((vector-basis (basis->vector-basis basis)))
    (make-Christoffel
     (s:map/r (lambda (e_k)
		(s:map/r (lambda (e_j)
			   (s:map/r (lambda (e_i)
				      (* 1/2 (- (+ (e_k (metric e_i e_j))
						   (e_j (metric e_i e_k)))
						(e_i (metric e_j e_k)))))
				    vector-basis))
			 vector-basis))
	      vector-basis)
     basis)))

;#|
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
;(pec ((Christoffel->symbols
;       (metric->Christoffel-1 (g-sphere 'R)
;			      (coordinate-system->basis 2-sphere)))
;      ((2-sphere '->point) (up 'theta0 'phi0))))
;#| Result:
;(down
; (down (down 0 0) (down 0 (* (* (cos theta0) (sin theta0)) (expt R 2))))
; (down (down 0 (* (* (cos theta0) (sin theta0)) (expt R 2)))
;       (down (* (* -1 (cos theta0) (sin theta0)) (expt R 2)) 0)))
;|#
;|#

;#|
;;;; Check of text procedure for getting second Christoffel symbols
;
;(define (metric->Christoffel-2 metric basis)
;  (let ((gi (metric:invert metric basis))
;	(G1 (metric->Christoffel-1 metric basis)))
;    (let ((vector-basis (basis->vector-basis basis))
;	  (1form-basis (basis->1form-basis basis))
;	  (G1S (Christoffel->symbols G1)))
;      (define ((Gamma-Bar v w) u)
;	(let ((stuff
;	       (s:map/r (lambda (e_k)
;			  (s:map/r (lambda (e_j)
;				     (s:map/r (lambda (e_i)
;						(* (e_i u) (e_j v) (e_k w)))
;					      1form-basis))
;				   1form-basis))
;			1form-basis)))
;	  (apply + (ultra-flatten (s:map/r * G1S stuff)))))
;      (define (Gamma-hat v w)
;	(apply +
;	       (ultra-flatten
;		(s:map/r
;		 (lambda (e~i e_i)
;		   (* (gi (Gamma-Bar v w) e~i) e_i))
;		 1form-basis vector-basis))))
;      (make-Christoffel
;       (s:map/r (lambda (e_k)
;		  (s:map/r (lambda (e_j)
;			     (s:map/r (lambda (e~i)
;					(e~i (Gamma-hat e_j e_k)))
;				      1form-basis))
;			   vector-basis))
;		vector-basis)       
;       basis))))
;
;
;(pec ((Christoffel->symbols
;       (metric->Christoffel-2 (g-sphere 'R)
;			      (coordinate-system->basis 2-sphere)))
;      ((2-sphere '->point) (up 'theta0 'phi0))))
;#| Result:
;(down (down (up 0 0)
;	    (up 0 (/ (cos theta0) (sin theta0))))
;      (down (up 0 (/ (cos theta0) (sin theta0)))
;	    (up (* -1 (cos theta0) (sin theta0)) 0)))
;|#
;;;; As expected!
;|#

;#|
;;;; Test with general 2d metric
;
;(define R2 (rectangular 2))
;(instantiate-coordinates R2 '(x y))
;
;(define fa
;  (compose (literal-function 'a (-> (UP Real Real) Real))
;	   (R2 '->coords)))
;(define fb
;  (compose (literal-function 'b (-> (UP Real Real) Real))
;	   (R2 '->coords)))
;(define fc
;  (compose (literal-function 'c (-> (UP Real Real) Real))
;	   (R2 '->coords)))
;
;(define ((g-R2 g_00 g_01 g_11) u v)
;  (+ (* g_00 (dx u) (dx v))
;     (* g_01 (+ (* (dx u) (dy v)) (* (dy u) (dx v))))
;     (* g_11 (dy u) (dy v))))
;
;(pec (((g-R2 fa fb fc)
;       (literal-vector-field 'u R2)
;       (literal-vector-field 'v R2))
;      ((R2 '->point) (up 'x0 'y0))))
;#| Result:
;(+ (* (v^1 (up x0 y0)) (u^1 (up x0 y0)) (c (up x0 y0)))
;   (* (v^0 (up x0 y0)) (b (up x0 y0)) (u^1 (up x0 y0)))
;   (* (u^0 (up x0 y0)) (b (up x0 y0)) (v^1 (up x0 y0)))
;   (* (a (up x0 y0)) (u^0 (up x0 y0)) (v^0 (up x0 y0))))
;|#
;
;(define R2-basis (coordinate-system->basis R2))
;
;(pec ((Christoffel->symbols
;       (metric->Christoffel-1 (g-R2 fa fb fc) R2-basis))
;      ((R2 '->point) (up 'x0 'y0))))
;#| Result:
;(down
; (down
;  (down (* 1/2 (((partial 0) a) (up x0 y0)))
;	(+ (* -1/2 (((partial 1) a) (up x0 y0)))
;	   (((partial 0) b) (up x0 y0))))
;  (down (* 1/2 (((partial 1) a) (up x0 y0)))
;        (* 1/2 (((partial 0) c) (up x0 y0)))))
; (down
;  (down (* 1/2 (((partial 1) a) (up x0 y0)))
;        (* 1/2 (((partial 0) c) (up x0 y0))))
;  (down (+ (((partial 1) b) (up x0 y0))
;	   (* -1/2 (((partial 0) c) (up x0 y0))))
;        (* 1/2 (((partial 1) c) (up x0 y0))))))
;|#
;|#

(define (metric->Christoffel-2 metric basis)
  (let ((gi (metric:invert metric basis)))
    (let ((vector-basis (basis->vector-basis basis))
	  (1form-basis (basis->1form-basis basis)))
      (make-Christoffel
       (s:map/r (lambda (e_k)
		  (s:map/r (lambda (e_j)
			     (s:map/r (lambda (w_i)
					(contract-2
					 (lambda (e_m w_m)
					   (* (gi w_i w_m)
					      (* 1/2
						 (- (+ (e_k (metric e_m e_j))
						       (e_j (metric e_m e_k)))
						    (e_m (metric e_j e_k))))))
					 basis))
				      1form-basis))
			   vector-basis))
		vector-basis)
       basis))))

;#|
;(pec ((Christoffel->symbols
;       (metric->Christoffel-2 (g-sphere 'R)
;			      (coordinate-system->basis 2-sphere)))
;      ((2-sphere '->point) (up 'theta0 'phi0))))
;#| Result:
;(down
; (down (up 0 0) (up 0 (/ (cos theta0) (sin theta0))))
; (down (up 0 (/ (cos theta0) (sin theta0)))
;       (up (* -1 (sin theta0) (cos theta0)) 0)))
;|#
;|#

;;; Symbolic metrics are often useful for testing.

(define (gij i j)
  (if (<= i j)
      (literal-function (string->symbol
			 (string-append "g"
					"_"
					(number->string i)
					(number->string j)))
			(-> (UP* Real) Real))
      (gij j i)))
				    
(define (literal-metric basis)
  ;; Flat coordinate systems here only.
  (let ((1form-basis (basis->1form-basis basis)))
    (let ((n (s:dimension 1form-basis)))
      (let ((gcoeffs
	     (s:generate n 'down
			 (lambda (i)
			   (s:generate n 'down
				       (lambda (j)
					 (gij i j)))))))
	(lambda (v1 v2)
	  (* (* gcoeffs (1form-basis v1))
	     (1form-basis v2)))))))
;#|
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;
;(set! *factoring* #f)
;
;(pec (((literal-metric (coordinate-system->basis R3))
;       (literal-vector-field 'u R3)
;       (literal-vector-field 'v R3))
;      ((R3 '->point) (up 'x0 'y0 'z0))))
;#| Result:
;(+ (* (g_00 (up x0 y0 z0)) (v^0 (up x0 y0 z0)) (u^0 (up x0 y0 z0)))
;   (* (v^0 (up x0 y0 z0)) (g_01 (up x0 y0 z0)) (u^1 (up x0 y0 z0)))
;   (* (v^0 (up x0 y0 z0)) (g_02 (up x0 y0 z0)) (u^2 (up x0 y0 z0)))
;   (* (g_01 (up x0 y0 z0)) (v^1 (up x0 y0 z0)) (u^0 (up x0 y0 z0)))
;   (* (g_11 (up x0 y0 z0)) (v^1 (up x0 y0 z0)) (u^1 (up x0 y0 z0)))
;   (* (v^1 (up x0 y0 z0)) (g_12 (up x0 y0 z0)) (u^2 (up x0 y0 z0)))
;   (* (g_02 (up x0 y0 z0)) (u^0 (up x0 y0 z0)) (v^2 (up x0 y0 z0)))
;   (* (g_12 (up x0 y0 z0)) (u^1 (up x0 y0 z0)) (v^2 (up x0 y0 z0)))
;   (* (g_22 (up x0 y0 z0)) (v^2 (up x0 y0 z0)) (u^2 (up x0 y0 z0))))
;|#
;|#

;#|
;;;; Runs out of memory trying to print foo, below if we use the following
;;;; (define polar (polar/cylindrical 2))
;;;; Not anymore!  Now it works.
;
;(define polar (rectangular 2))
;(instantiate-coordinates polar '(r theta))
;
;(define polar-point 
;  ((polar '->point) (up 'r 'theta)))
;
;(define polar-basis
;  (coordinate-system->basis polar))
;
;(define (polar-metric v1 v2)
;  (+ (* (dr v1) (dr v2))
;     (* (square r)
;	(* (dtheta v1) (dtheta v2)))))
;
;(define foo
;  ((Christoffel->symbols
;    (metric->Christoffel-2 polar-metric polar-basis))
;   polar-point))
;
;(pec foo)
;#| Result:
;(down
; (down (up 0 0)
;       (up 0 (/ 1 r)))
; (down (up 0 (/ 1 r))
;       (up (* -1 r) 0)))
;|#
;
;;;; Thus, make simplified version.
;
;(define polar-Gamma
;  (make-Christoffel
;   (let ((O (lambda x 0)))
;     (down
;      (down (up O O)
;	    (up O (/ 1 r)))
;      (down (up O (/ 1 r))
;	    (up (* -1 r) O))))
;   (coordinate-system->basis polar)))
;
;;;; Now look at curvature
;(for-each
; (lambda (alpha)
;   (for-each
;    (lambda (beta)
;      (for-each
;       (lambda (gamma)
;	 (for-each
;	  (lambda (delta)
;	    (newline)
;	    (pe `(,alpha ,beta ,gamma ,delta))
;	    (pe (((Riemann (Christoffel->Cartan polar-Gamma))
;		  alpha beta gamma delta)
;		 polar-point)))
;	  (list d/dr d/dtheta)))
;       (list d/dr d/dtheta)))
;    (list d/dr d/dtheta)))
; (list dr dtheta))
;;;; 16 zeros
;|#

;#|
;(define spherical (rectangular 3))
;(instantiate-coordinates spherical '(r theta phi))
;
;(define spherical-point 
;  ((spherical '->point) (up 'r 'theta 'phi)))
;
;(define spherical-basis
;  (coordinate-system->basis spherical))
;
;(define (spherical-metric v1 v2)
;  (+ (* (dr v1) (dr v2))
;     (* (square r)
;	(+ (* (dtheta v1) (dtheta v2))
;	   (* (expt (sin theta) 2)
;	      (dphi v1) (dphi v2))))))
;
;(define foo
;  ((Christoffel->symbols
;    (metric->Christoffel-2 spherical-metric spherical-basis))
;   spherical-point))
;
;(pec foo)
;#| Result:
;(down
; (down (up 0 0 0) (up 0 (/ 1 r) 0) (up 0 0 (/ 1 r)))
; (down (up 0 (/ 1 r) 0) (up (* -1 r) 0 0) (up 0 0 (/ (cos theta) (sin theta))))
; (down (up 0 0 (/ 1 r))
;       (up 0 0 (/ (cos theta) (sin theta)))
;       (up (* -1 r (expt (sin theta) 2)) (* -1 (sin theta) (cos theta)) 0)))
;|#
;
;;;; Thus, make simplified version.
;
;(define spherical-Gamma
;  (make-Christoffel
;   (let ((O (lambda x 0)))
;     (down
;      (down (up O O O) (up O (/ 1 r) O) (up O O (/ 1 r)))
;      (down (up O (/ 1 r) O) (up (* -1 r) O O) (up O O (/ (cos theta) (sin theta))))
;      (down (up O O (/ 1 r))
;	    (up O O (/ (cos theta) (sin theta)))
;	    (up (* -1 r (expt (sin theta) 2)) (* -1 (sin theta) (cos theta)) O))))
;   (coordinate-system->basis spherical)))
;
;;;; Now look at curvature
;(for-each
; (lambda (alpha)
;   (for-each
;    (lambda (beta)
;      (for-each
;       (lambda (gamma)
;	 (for-each
;	  (lambda (delta)
;	    (newline)
;	    (pe `(,alpha ,beta ,gamma ,delta))
;	    (pe (((Riemann (Christoffel->Cartan spherical-Gamma))
;		  alpha beta gamma delta)
;		 spherical-point)))
;	  (list d/dr d/dtheta d/dphi)))
;       (list d/dr d/dtheta d/dphi)))
;    (list d/dr d/dtheta d/dphi)))
; (list dr dtheta dphi))
;;;; 81 zeros
;|#

;;; Connections for non-coordinate basis -- MTW p.210

;;; c_ijk = g_kl c_ij^l = g_kl e^l([e_i, e_j])

(define (structure-constant e_i e_j e_k basis metric)
  (contract-2 
   (lambda (e_l w_l)
     (* (metric e_k e_l)
	(w_l (commutator e_i e_j))))
   basis))

(define (metric->connection-1 metric basis)
  (let ((vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis)))
    (make-Christoffel
     (s:map/r
      (lambda (e_k)
	(s:map/r
	 (lambda (e_j)
	   (s:map/r
	    (lambda (e_i)
	      (* 1/2 (+ (- (+ (e_k (metric e_i e_j))
			      (e_j (metric e_i e_k)))
			   (e_i (metric e_j e_k)))
			(- (+ (structure-constant e_i e_j e_k basis metric)
			      (structure-constant e_i e_k e_j basis metric))
			   (structure-constant e_j e_k e_i basis metric)))))
	    vector-basis))
	 vector-basis))
      vector-basis)
     basis)))


(define (metric->connection-2 metric basis)
  (let ((vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis))
	(inverse-metric (metric:invert metric basis)))
    (make-Christoffel
     (s:map/r
      (lambda (e_k)
	(s:map/r
	 (lambda (e_j)
	   (s:map/r
	    (lambda (w_i)
	      (contract-2
	       (lambda (e_m w_m)
		 (* (inverse-metric w_i w_m)
		    (* 1/2 (+ (- (+ (e_k (metric e_m e_j))
				    (e_j (metric e_m e_k)))
				 (e_m (metric e_j e_k)))
			      (- (+ (structure-constant e_m e_j e_k basis metric)
				    (structure-constant e_m e_k e_j basis metric))
				 (structure-constant e_j e_k e_m basis metric))))))
	       basis))
	    1form-basis))
	 vector-basis))
      vector-basis)
     basis)))

;#|
;;;; MTW p205 spherical flat lorentz
;
;(define spherical-Lorentz (rectangular 4))
;(instantiate-coordinates spherical-Lorentz '(t r theta phi))
;
;(define spherical-Lorentz-basis
;  (coordinate-system->basis spherical-Lorentz))
;
;(define ((spherical-Lorentz-metric c^2) v1 v2)
;  (+ (* -1 c^2 (* (dt v1) (dt v2)))
;     (* (dr v1) (dr v2))
;     (* (square r)
;	(+ (* (dtheta v1) (dtheta v2))
;	   (* (square (sin theta))
;	      (* (dphi v1) (dphi v2)))))))
;
;(define spherical-Lorentz-point 
;  ((spherical-Lorentz '->point) (up 't 'r 'theta 'phi)))
;
;(define (orthonormal-spherical-Lorentz-vector-basis c^2)
;  (down (* (/ 1 (sqrt c^2)) d/dt)
;	d/dr
;	(* (/ 1 r) d/dtheta)
;	(* (/ 1 (* r (sin theta))) d/dphi)))
;
;(define (orthonormal-spherical-Lorentz-1form-basis c^2)
;  (let ((orthonormal-spherical-Lorentz-vectors
;	 (orthonormal-spherical-Lorentz-vector-basis c^2)))
;    (vector-basis->dual orthonormal-spherical-Lorentz-vectors 
;			spherical-Lorentz)))
;
;(define (orthonormal-spherical-Lorentz-basis c^2)
;  (make-basis (orthonormal-spherical-Lorentz-vector-basis c^2)
;	      (orthonormal-spherical-Lorentz-1form-basis c^2)))
;
;(pec ((s:map/r (orthonormal-spherical-Lorentz-1form-basis 'c^2)
;	       (orthonormal-spherical-Lorentz-vector-basis 'c^2))
;      spherical-Lorentz-point))
;#| Result:
;(down (up 1 0 0 0) (up 0 1 0 0) (up 0 0 1 0) (up 0 0 0 1))
;|#
;
;(pec (((spherical-Lorentz-metric 'c^2)
;       (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 0)
;       (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 0))
;      spherical-Lorentz-point))
;#| Result:
;-1
;|#
;
;(pec (((spherical-Lorentz-metric 'c^2)
;       (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 1)
;       (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 1))
;      spherical-Lorentz-point))
;#| Result:
;1
;|#
;
;(pec (((spherical-Lorentz-metric 'c^2)
;       (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 2)
;       (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 2))
;      spherical-Lorentz-point))
;#| Result:
;1
;|#
;
;(pec (((spherical-Lorentz-metric 'c^2)
;       (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 3)
;       (ref (orthonormal-spherical-Lorentz-vector-basis 'c^2) 3))
;      spherical-Lorentz-point))
;#| Result:
;1
;|#
;
;(pec ((Christoffel->symbols
;       (metric->connection-1 (spherical-Lorentz-metric 'c^2)
;			     (orthonormal-spherical-Lorentz-basis 'c^2)))
;      spherical-Lorentz-point))
;#| Result:
;(down
; (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
; (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
; (down (down 0 0 0 0) (down 0 0 (/ 1 r) 0) (down 0 (/ -1 r) 0 0) (down 0 0 0 0))
; (down (down 0 0 0 0)
;       (down 0 0 0 (/ 1 r))
;       (down 0 0 0 (/ (cos theta) (* r (sin theta))))
;       (down 0 (/ -1 r) (/ (* -1 (cos theta)) (* r (sin theta))) 0)))
;|#
;
;(define foo
;  ((Christoffel->symbols
;    (metric->connection-2 (spherical-Lorentz-metric 'c^2)
;			  (orthonormal-spherical-Lorentz-basis 'c^2)))
;   spherical-Lorentz-point))
;
;(pec foo)
;#| Result:
;(down
; (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
; (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
; (down (up 0 0 0 0) (up 0 0 (/ 1 r) 0) (up 0 (/ -1 r) 0 0) (up 0 0 0 0))
; (down (up 0 0 0 0)
;       (up 0 0 0 (/ 1 r))
;       (up 0 0 0 (/ (cos theta) (* r (sin theta))))
;       (up 0 (/ -1 r) (/ (* -1 (cos theta)) (* r (sin theta))) 0)))
;|#
;;;; The last two are essentially the same.  Is this correct?
;
;#|
;;;; Check answers from MTW p.213
;;;; t r theta phi
;;;; 0 1 2     3
;
;(pe (ref foo 3 2 3))
;(/ (cos theta) (* r (sin theta)))
;
;(pe (ref foo 3 3 2))
;(/ (* -1 (cos theta)) (* r (sin theta)))
;
;(pe (ref foo 2 1 2))
;(/ 1 r)
;
;(pe (ref foo 3 1 3))
;(/ 1 r)
;
;(pe (ref foo 2 2 1))
;(/ -1 r)
;
;(pe (ref foo 3 3 1))
;(/ -1 r)
;|#
;
;(define (orthonormal-spherical-Lorentz-second-connection c^2)
;  (make-Christoffel
;   (let ((zero (lambda (point) 0)))
;     (down
;      (down (up zero zero zero zero)
;	    (up zero zero zero zero)
;	    (up zero zero zero zero)
;	    (up zero zero zero zero))
;      (down (up zero zero zero zero)
;	    (up zero zero zero zero)
;	    (up zero zero zero zero)
;	    (up zero zero zero zero))
;      (down (up zero zero zero zero)
;	    (up zero zero (/ 1 r) zero)
;	    (up zero (/ -1 r) zero zero)
;	    (up zero zero zero zero))
;      (down (up zero zero zero zero)
;	    (up zero zero zero (/ 1 r))
;	    (up zero zero zero (/ (cos theta) (* r (sin theta))))
;	    (up zero
;		(/ -1 r)
;		(/ (* -1 (cos theta)) (* r (sin theta)))
;		zero))))
;   (orthonormal-spherical-Lorentz-basis c^2)))
;
;;;; Look at curvature
;(for-each
; (lambda (alpha)
;   (for-each
;    (lambda (beta)
;      (for-each
;       (lambda (gamma)
;	 (for-each
;	  (lambda (delta)
;	    (newline)
;	    (pe `(,alpha ,beta ,gamma ,delta))
;	    (pe (((Riemann
;		   (Christoffel->Cartan
;		    (orthonormal-spherical-Lorentz-second-connection 'c^2)))
;		  alpha beta gamma delta)
;		 spherical-Lorentz-point)))
;	  (list d/dt d/dr d/dtheta d/dphi)))
;       (list d/dt d/dr d/dtheta d/dphi)))
;    (list d/dt d/dr d/dtheta d/dphi)))
; (list dt dr dtheta dphi))
;;;; 256 zeros
;|#

