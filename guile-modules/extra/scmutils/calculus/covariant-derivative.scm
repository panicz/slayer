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

;#|
;;;; nabla_X V  =  covariant derivative of V wrt X
;;;; V is a vector field, X is a vector
;;;; same as nabla V (X), where nabla V is covariant differential
;
;;;; nabla_X V -- The covariant derivative over a map is the same as
;;;; this program, but it needs to build operators.
;
;(define (((covariant-derivative Cartan) X) V)
;  (assert (Cartan? Cartan))
;  (let ((basis (Cartan->basis Cartan))
;	(Cartan-forms (Cartan->forms Cartan)))
;    (let ((vector-basis (basis->vector-basis basis))
;	  (1form-basis (basis->1form-basis basis)))
;      (let ((v-components (1form-basis V)))
;	(define (the-derivative f)
;	  (* (vector-basis f)
;	     (+ (X v-components)
;		(* (Cartan-forms X) v-components))))
;	the-derivative))))
;|#

;;; More complete covariant derivative procedure

(define (covariant-derivative Cartan)
  (assert (Cartan? Cartan))
  (let ((basis (Cartan->basis Cartan))
	(Cartan-forms (Cartan->forms Cartan)))
    (let ((vector-basis (basis->vector-basis basis))
	  (1form-basis (basis->1form-basis basis)))  
      (define (nabla X)
	(define (nabla_X V)
	  (let ((v-components (1form-basis V)))      
	    (let ((deriv-components
		   (+ (X v-components)
		      (* (Cartan-forms X) v-components))))
	      (define (the-derivative f)
		(* (vector-basis f) deriv-components))
	      (procedure->vector-field the-derivative
				       `((nabla ,(diffop-name X))
					 ,(diffop-name V))))))
	(procedure->vector-field nabla_X
				 `(nabla ,(diffop-name X))))
      nabla)))


;;; nabla V(X)

(define (((covariant-differential Cartan) V) X)
  (((covariant-derivative Cartan) X) V))


(define (Cartan->Christoffel Cartan)
  (assert (Cartan? Cartan))
  (let ((basis (Cartan->basis Cartan))
	(Cartan-forms (Cartan->forms Cartan)))
    (make-Christoffel
     (s:map/r Cartan-forms
	      (basis->vector-basis basis))
     basis)))

(define (Christoffel->Cartan Christoffel)
  (assert (Christoffel? Christoffel))
  (let ((basis (Christoffel->basis Christoffel))
	(Christoffel-symbols
	 (Christoffel->symbols Christoffel)))
    (make-Cartan
     (* Christoffel-symbols (basis->1form-basis basis))
     basis)))

;;; Constructors and Selectors

(define (make-Cartan forms basis)
  (list '*Cartan* forms basis))

(define (Cartan? thing)
  (and (pair? thing)
       (eq? (car thing) '*Cartan*)))

(define (Cartan->forms thing) (cadr thing))

(define (Cartan->basis thing) (caddr thing))


(define (make-Christoffel symbols basis)
  (list '*Christoffel* symbols basis))

(define (Christoffel? thing)
  (and (pair? thing)
       (eq? (car thing) '*Christoffel*)))

(define (Christoffel->symbols thing) (cadr thing))

(define (Christoffel->basis thing) (caddr thing))

;#|
;;;; Fun with Christoffel symbols.
;
;(define R2 (rectangular 2))
;(instantiate-coordinates R2 '(x y))
;(define R2-basis
;  (coordinate-system->basis R2))
;(define R2-point
;  ((R2 '->point) (up 'x0 'y0)))
;
;(define (Gijk i j k)
;  (literal-function (string->symbol
;		     (string-append "G^"
;				    (number->string i)
;				    "_"
;				    (number->string j)
;				    (number->string k)))
;		    (-> (UP* Real) Real)))
;				    
;(define G
;  (down (down (up (Gijk 0 0 0)
;		  (Gijk 1 0 0))
;	      (up (Gijk 0 1 0)
;		  (Gijk 1 1 0)))
;	(down (up (Gijk 0 0 1)
;		  (Gijk 1 0 1))
;	      (up (Gijk 0 1 1)
;		  (Gijk 1 1 1)))))
;	      
;(clear-arguments)
;(suppress-arguments '((up x0 y0)))
;
;(pec (G R2-point)
;    (compose arg-suppressor simplify))
;#| Result:
;(down (down (up G^0_00 G^1_00) (up G^0_10 G^1_10))
;      (down (up G^0_01 G^1_01) (up G^0_11 G^1_11)))
;|#
;
;(define CG (make-Christoffel G R2-basis))
;
;(define CF (Christoffel->Cartan CG))
;
;(pec (Christoffel->Cartan CG))
;#| Result:
;(*Cartan*
; (down
;  (up (+ (* G^0_00 (w 0)) (* G^0_01 (w 1)))
;      (+ (* G^1_00 (w 0)) (* G^1_01 (w 1))))
;  (up (+ (* G^0_10 (w 0)) (* G^0_11 (w 1)))
;      (+ (* G^1_10 (w 0)) (* G^1_11 (w 1)))))
; (*coordinate-basis* (down (e 0) (e 1))
;		     (up (w 0) (w 1))
;		     2 R2))
;|#
;
;(pec ((Christoffel->symbols
;       (Cartan->Christoffel (Christoffel->Cartan CG)))
;      R2-point)
;    (compose arg-suppressor simplify))
;#| Result:
;(down (down (up G^0_00 G^1_00) (up G^0_10 G^1_10))
;      (down (up G^0_01 G^1_01) (up G^0_11 G^1_11)))
;|#
;
;(pec (((((covariant-derivative CF) d/dx) d/dy)
;       (literal-scalar-field 'f R2))
;      R2-point))
;#| Result:
;(+ (* (((partial 0) f) (up x0 y0)) (G^0_10 (up x0 y0)))
;   (* (((partial 1) f) (up x0 y0)) (G^1_10 (up x0 y0))))
;|#
;
;(define X (literal-vector-field 'X R2))
;
;(define V (literal-vector-field 'v R2))
;
;(pec (((((covariant-derivative CF) X) V)
;       (literal-function 'f (-> (UP Real Real) Real)))
;      R2-point)
;    (compose arg-suppressor simplify))
;#| Result:
;(+ (* G^0_00 v^0 ((partial 0) f) X^0)
;   (* G^0_01 v^0 ((partial 0) f) X^1)
;   (* G^0_10 v^1 ((partial 0) f) X^0)
;   (* G^0_11 v^1 ((partial 0) f) X^1)
;   (* G^1_00 v^0 X^0 ((partial 1) f))
;   (* G^1_01 v^0 ((partial 1) f) X^1)
;   (* G^1_10 v^1 X^0 ((partial 1) f))
;   (* G^1_11 v^1 ((partial 1) f) X^1)
;   (* ((partial 0) v^0) ((partial 0) f) X^0)
;   (* ((partial 1) v^0) ((partial 0) f) X^1)
;   (* ((partial 0) v^1) X^0 ((partial 1) f))
;   (* ((partial 1) v^1) ((partial 1) f) X^1))
;|#
;
;|#

;;; Over a map

(define (Christoffel->Cartan-over-map Christoffel-on-M mu:N->M)
  (make-Cartan
   (s:map/r (pullback mu:N->M) 
	    (Cartan->forms
	     (Christoffel->Cartan Christoffel-on-M)))
   (basis->basis-over-map mu:N->M
			  (Christoffel->basis Christoffel-on-M))))

;#|
;(define (Christoffel->Cartan-over-map Christoffel-on-M mu:N->M)
;  (let ((symbols (Christoffel->symbols Christoffel-on-M))
;	(1form-basis-over-mu
;	 (basis->1form-basis
;	  (basis->basis-over-map mu:N->M
;	   (Christoffel->basis Christoffel-on-M)))))
;    (make-Cartan
;     (procedure->1form-field
;      (lambda (X-on-N)
;	(* (compose symbols mu:N->M)
;	   (s:map/r (lambda (w)
;		      (w ((differential mu:N->M) X-on-N)))
;		    1form-basis-over-mu)))
;      `(Cartan ,(diffop-name Christoffel-on-M)
;	       ,(diffop-name mu:N->M)))
;     (basis->basis-over-map mu:N->M
;			  (Christoffel->basis Christoffel-on-M)))))
;|#

;#|
;(define rect-plane (rectangular 2))
;(instantiate-coordinates rect-plane '(x y))
;(define rect-basis 
;  (coordinate-system->basis rect-plane))
;(define polar-plane (polar/cylindrical 2))
;(instantiate-coordinates polar-plane '(r theta))
;(define polar-basis (coordinate-system->basis polar-plane))
;(define rect-chi (rect-plane '->coords))
;(define rect-chi-inverse (rect-plane '->point))
;(define polar-chi (polar-plane '->coords))
;(define polar-chi-inverse (polar-plane '->point))
;(define m2 (rect-chi-inverse (up 'x0 'y0)))
;
;(define rect-Christoffel
;  (make-Christoffel
;   (let ((zero (lambda (m) 0)))
;     (down (down (up zero zero)
;		 (up zero zero))
;	   (down (up zero zero)
;		 (up zero zero))))
;   rect-basis))
;
;(define polar-Christoffel
;  (make-Christoffel
;   (let ((zero (lambda (m) 0)))
;     (down (down (up zero zero)
;		 (up zero (/ 1 r)))
;	   (down (up zero (/ 1 r))
;		 (up (* -1 r) zero))))
;   polar-basis))
;
;(define rect-Cartan
;  (Christoffel->Cartan rect-Christoffel))
;
;(define polar-Cartan
;  (Christoffel->Cartan polar-Christoffel))
;
;(define circular (- (* x d/dy) (* y d/dx)))
;
;(define f (literal-scalar-field 'f rect-plane))
;
;(pec
; (((((covariant-derivative rect-Cartan) 
;     d/dx)
;    circular)
;   f)
;  m2))
;#| Result:
;(((partial 1) f) (up x0 y0))
;|#
;
;(pec
; (((((covariant-derivative polar-Cartan) 
;     d/dx)
;    circular)
;   f)
;  m2))
;#| Result:
;(((partial 1) f) (up x0 y0))
;|#
;|#

;#|
;;;; More generally, can show independence here
;
;(define v (literal-vector-field 'v rect-plane))
;(define w (literal-vector-field 'w rect-plane))
;
;(pec
; (((((- (covariant-derivative rect-Cartan)
;	(covariant-derivative polar-Cartan))
;     v)
;    w)
;   f)
;  m2))
;#| Result:
;0
;|#
;
;(define v (literal-vector-field 'v polar-plane))
;(define w (literal-vector-field 'w polar-plane))
;
;(pec
; (((((- (covariant-derivative rect-Cartan)
;	(covariant-derivative polar-Cartan))
;     v)
;    w)
;   f)
;  m2))
;#| Result:
;0
;|#
;
;|#

;#|
;;;;; Geodesic Equations = Lagrange Equations 
;
;;;; Here I restrict everything to the unit sphere.
;
;(instantiate-coordinates the-real-line 't)
;
;(define S (S2 1))
;
;;;; The coordinates on the unit sphere
;
;(instantiate-coordinates S '(theta phi))
;
;(define 2-sphere-basis (coordinate-system->basis S))
;
;;;; The Christoffel symbols (for r=1) (p.341 MTW) are:
; 
;(define G-S2-1
;  (make-Christoffel
;   (let ((zero  (lambda (point) 0))) 
;     (down (down (up zero zero)
;		 (up zero (/ 1 (tan theta))))
;	   (down (up zero (/ 1 (tan theta)))
;		 (up (- (* (sin theta) (cos theta))) zero))))
;   2-sphere-basis))
;
;(pec (let ((U d/dt)
;	   (mu:N->M (compose (S '->point)
;			     (up (literal-function 'theta)
;				 (literal-function 'phi))
;			     (the-real-line '->coords))))
;       (let* ((basis-over-mu (basis->basis-over-map mu:N->M 2-sphere-basis))
;	      (1form-basis (basis->1form-basis basis-over-mu))
;	      (Cartan-over-mu
;	       (Christoffel->Cartan-over-map G-S2-1 mu:N->M)))
;	 (s:map/r 
;	  (lambda (w)
;	    ((w
;	      (((covariant-derivative Cartan-over-mu) U)
;	       ((differential mu:N->M) U)))
;	     ((the-real-line '->point) 'tau)))
;	  1form-basis))))
;#| Result:
;(up (+ (* -1
;	  (cos (theta tau))
;	  (expt ((D phi) tau) 2)
;	  (sin (theta tau)))
;       (((expt D 2) theta) tau))
;     (/ (+ (* (sin (theta tau)) (((expt D 2) phi) tau))
;	   (* 2 (cos (theta tau)) ((D phi) tau) ((D theta) tau)))
;	(sin (theta tau))))
;|#
;
;;;; We can get the geodesic equations as ordinary Lagrange
;;;; equations of a free particle constrained to the surface
;;;; of the sphere:
;
;(define ((Lfree m) s)
;  (let ((t (time s))
;	(q (coordinate s))
;	(v (velocity s)))
;    (* 1/2 m (square v))))
;
;(define F
;  (compose (S '->point) coordinate))
;
;(define Lsphere
;  (compose (Lfree 1) (F->C F)))
;
;(pec (((Lagrange-equations Lsphere)
;       (up (literal-function 'theta)
;	   (literal-function 'phi)))
;      't))
;#| Result:
;(down
; (+ (((expt D 2) theta) t)
;    (* -1 (cos (theta t)) (sin (theta t)) (expt ((D phi) t) 2)))
; (+ (* (expt (sin (theta t)) 2) (((expt D 2) phi) t))
;    (* 2 (cos (theta t)) (sin (theta t)) ((D phi) t) ((D theta) t))))
;|#
;
;
;;;; Note these are DOWN while the geodesic equations are UP.  This is
;;;; due to the fact that the geodesic equations are raised by the
;;;; metric, which is diagonal, here R=1, and cancels an instance
;;;; of(expt (sin theta) 2).
;
;;;; Also see p.345 MTW for computing Christoffel symbols from Lagrange
;;;; equations.
;|#

;#|
;;;; Exercise on computation of Christoffel symbols.
;
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;(define R3-point ((R3 '->point) (up 'x0 'y0 'z0)))
;
;(define C3 (polar/cylindrical 3))
;(instantiate-coordinates C3 '(r theta zeta))
;(define C3-point ((C3 '->point) (up 'r0 'theta0 'z0)))
;
;(pec (((* d/dr d/dr) identity) R3-point))
;#| Result:
;(up 0 0 0)
;|#
;;;; So \Gamma^r_{rr} = 0, \Gamma^\theta_{rr} = 0
;
;(pec (((* d/dtheta d/dr) identity) R3-point))
;#| Result:
;(up (/ (* -1 y0) (sqrt (+ (expt x0 2) (expt y0 2))))
;    (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
;    0)
;|#
;;;; by hand = -sint d/dx + cost d/dy = 1/r d/dtheta
;;;; Indeed.
;
;(pec (((* d/dtheta d/dr) identity) C3-point))
;#| Result:
;(up (* -1 (sin theta0)) (cos theta0) 0)
;|#
;;;; So \Gamma^r_{r\theta} = 0, \Gamma^\theta_{r\theta} = 1/r
;
;(pec (((* d/dr d/dtheta) identity) R3-point))
;#| Result:
;(up (/ (* -1 y0) (sqrt (+ (expt x0 2) (expt y0 2))))
;    (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
;    0)
;|#
;;;; by hand = -sint d/dx + cost d/dy = 1/r d/dtheta
;
;(pec (((* d/dr d/dtheta) identity) C3-point))
;#| Result:
;(up (* -1 (sin theta0)) (cos theta0) 0)
;|#
;;;; So \Gammar_{\theta r} = 0, \Gamma\theta_{\theta r} = 1/r
;
;(pec (((* d/dtheta d/dtheta) identity) R3-point))
;#| Result:
;(up (* -1 x0) (* -1 y0) 0)
;|#
;;;; by hand = -r cost d/dx - r sint d/dy = -r d/dr
;
;(pec (((* d/dtheta d/dtheta) identity) C3-point))
;#| Result:
;(up (* -1 r0 (cos theta0)) (* -1 r0 (sin theta0)) 0)
;|#
;;;; So \Gammar_{\theta \theta} = -r, \Gamma\theta_{\theta \theta} = 0
;
;;;; These are correct Christoffel symbols...
;|#
