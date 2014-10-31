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

;;; Riemann curvature "tensor" is pretty easy

;;; Hawking and Ellis page 34.

(define ((torsion Cartan) X Y)
  (let ((nabla (covariant-derivative Cartan)))
    (+ ((nabla X) Y)
       (* -1 ((nabla Y) X))
       (* -1 (commutator X Y)))))

;;; Hawking and Ellis equation 2.18, page 35.

(define ((Riemann-curvature Cartan) u v)
  (let ((nabla (covariant-derivative Cartan)))
    (- (commutator (nabla u) (nabla v))
       (nabla (commutator u v)))))


;;; The traditional Riemann tensor R^i_jkl:

(define ((Riemann Cartan) w x u v)
  (assert (and (form-field? w)
	       (vector-field? u)
	       (vector-field? v)
	       (vector-field? x)))
  (w (((Riemann-curvature Cartan) u v) x)))

;#|
;(define 2-sphere (S2 'r))
;(instantiate-coordinates 2-sphere '(theta phi))
;(define 2-sphere-basis (coordinate-system->basis 2-sphere))
;
;(define a-point ((2-sphere '->point) (up 'theta 'phi)))
;
;(define a-function (literal-scalar-field 'f 2-sphere))
;
;;;; the Christoffel symbols (for r=1) (p.341 mtw) are:
; 
;;;; (the up-down-down Christoffel symbols do not depend on R)
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
;(pec (((commutator d/dtheta d/dphi) a-function) a-point))
;#| result:
;0
;|#
;
;(let ((nabla (covariant-derivative (Christoffel->Cartan G-S2-1))))
;  (pec ((((nabla d/dtheta) d/dphi)
;	 a-function)
;	a-point)))
;#| result:
;(/ (* (cos theta)
;      (((partial 1) f) (up theta phi)))
;   (sin theta))
;|#
;
;(let ((nabla (covariant-derivative (Christoffel->Cartan G-S2-1))))
;  (pec ((((nabla d/dphi) ((nabla d/dtheta) d/dphi))
;	 a-function)
;	a-point)))
;#| result:
;(* -1 (((partial 0) f) (up theta phi)) (expt (cos theta) 2))
;|#
;
;(for-each
; (lambda (x)
;   (for-each
;    (lambda (y)
;      (pec ((((torsion (Christoffel->Cartan G-S2-1)) x y)
;	     a-function)
;	    a-point)))
;    (list  d/dtheta d/dphi)))
; (list  d/dtheta d/dphi))
;
;#| result:
;0					;four of these
;|#
;(pec (((Riemann (Christoffel->Cartan G-S2-1))
;       dphi d/dtheta d/dphi d/dtheta)
;      a-point))
;#| Result:
;1
;|#
;
;|#

;#|
;;;; We can work without embedding the sphere in R^3
;
;(define M (rectangular 2))
;(instantiate-coordinates M '(theta phi))
;(define M-basis (coordinate-system->basis M))
;(define a-point ((M '->point) (up 'theta0 'phi0)))
;(define a-function (literal-scalar-field 'f M))
;
;(define G-S2-1
;  (make-Christoffel
;   (let ((zero  (lambda (point) 0))) 
;     (down (down (up zero zero)
;                 (up zero (/ 1 (tan theta))))
;           (down (up zero (/ 1 (tan theta)))
;                 (up (- (* (sin theta) (cos theta))) zero))))
;   M-basis))
;
;(for-each
; (lambda (x)
;   (for-each
;    (lambda (y)
;      (pec ((((torsion (Christoffel->Cartan G-S2-1)) x y)
;	     a-function)
;	    a-point)))
;    (list  d/dtheta d/dphi)))
; (list  d/dtheta d/dphi))
;#| Result:
;0
;|#
;#| Result:
;0
;|#
;#| Result:
;0
;|#
;#| Result:
;0
;|#
;|#

;#|
;(define M (rectangular 2))
;(instantiate-coordinates M '(theta phi))
;(define M-basis (coordinate-system->basis M))
;
;(define a-point ((M '->point) (up 'theta^0 'phi^0)))
;
;(define G-S2-1
;  (make-Christoffel
;   (let ((zero  (lambda (point) 0))) 
;     (down (down (up zero zero)
;		 (up zero (/ 1 (tan theta))))
;	   (down (up zero (/ 1 (tan theta)))
;		 (up (- (* (sin theta) (cos theta))) zero))))
;   M-basis))
;
;(pec (((Riemann (Christoffel->Cartan G-S2-1))
;       dphi d/dtheta d/dphi d/dtheta)
;      a-point))
;#| Result:
;1
;|#
;;;; Computes instantly, with little memory.
;|#

;#|
;(set! *divide-out-terms* #f)
;
;;;; R^alpha_{beta gamma delta}
;
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
;	    (pe (((Riemann (Christoffel->Cartan G-S2-1))
;		  alpha beta gamma delta)
;		 a-point)))
;	  (list d/dtheta d/dphi)))
;       (list d/dtheta d/dphi)))
;    (list d/dtheta d/dphi)))
; (list dtheta dphi))
;
;;;; p351 MTW has efficient method for computing curvature (eq 14.18)
;
;;;alpha  beta     gamma    delta
;
;(dtheta d/dtheta d/dtheta d/dtheta)
;0
;
;(dtheta d/dtheta d/dtheta d/dphi)
;0
;
;(dtheta d/dtheta d/dphi d/dtheta)
;0
;
;(dtheta d/dtheta d/dphi d/dphi)
;0
;
;(dtheta d/dphi d/dtheta d/dtheta)
;0
;
;(dtheta d/dphi d/dtheta d/dphi)
;(expt (sin theta^0) 2)
;
;(dtheta d/dphi d/dphi d/dtheta)
;(* -1 (expt (sin theta^0) 2))
;
;(dtheta d/dphi d/dphi d/dphi)
;0
;
;(dphi d/dtheta d/dtheta d/dtheta)
;0
;
;(dphi d/dtheta d/dtheta d/dphi)
;-1
;
;(dphi d/dtheta d/dphi d/dtheta)
;1
;
;(dphi d/dtheta d/dphi d/dphi)
;0
;
;(dphi d/dphi d/dtheta d/dtheta)
;0
;
;(dphi d/dphi d/dtheta d/dphi)
;0
;
;(dphi d/dphi d/dphi d/dtheta)
;0
;
;(dphi d/dphi d/dphi d/dphi)
;0
;|#

;#|
;;;; Equation of geodesic deviation (MTW p275 eq.11.10) has a type
;;;; error.  The variation is ambiguously a vector-field over a map and
;;;; a vector field.  Riemann must take uniform stuff, and U is a
;;;; vector field on N (the-real-line), however variation is defined
;;;; only over the map.  The following does not work!
;
;(define M (rectangular 2))
;(instantiate-coordinates M '(theta phi))
;
;(pec (let ((U (components->vector-field (lambda (x) 1) the-real-line 'U))
;	   (mu:N->M (compose (M '->point)
;			     (up (literal-function 'f^theta)
;				 (literal-function 'f^phi)))))
;       (let* ((basis-over-mu (basis->basis-over-map mu:N->M 2-sphere-basis))
;	      (1form-basis (basis->1form-basis basis-over-mu))
;	      (vector-basis (basis->vector-basis basis-over-mu))
;
;	      (Cartan-over-mu
;	       (Christoffel->Cartan-over-map G-S2-1 mu:N->M))
;	      (variation (basis-components->vector-field
;			  (up (literal-function 'd_theta)
;			      (literal-function 'd_phi))
;			  vector-basis))
;	      (nablau
;	       ((covariant-derivative Cartan-over-mu) U))
;	      (d1 (nablau (nablau variation)))
;	      (d2 (((Riemann-curvature Cartan-over-mu) variation U) U))
;	      (deviation (+ d1 d2)))
;	 (s:map/r 
;	  (lambda (w)
;	    ((w deviation) ((the-real-line '->point) 'tau)))
;	  1form-basis))))
;
;;;; OK, in considering the variational problem, the map is actually
;;;; two dimensional, time is one direction and variation the other.
;
;(define M (rectangular 2))
;(instantiate-coordinates M '(theta phi))
;(define M-basis (coordinate-system->basis M))
;
;;;; The Christoffel symbols (for R=1) (p.341 MTW) are:
;;;; (the up-down-down Christoffel symbols do not depend on R)
;
;(define G-S2-1
;  (make-Christoffel
;   (let ((zero  (lambda (point) 0))) 
;     (down (down (up zero zero)
;		 (up zero (/ 1 (tan theta))))
;	   (down (up zero (/ 1 (tan theta)))
;		 (up (- (* (sin theta) (cos theta))) zero))))
;   M-basis))
;
;
;
;
;(define rectangular-plane (rectangular 2))
;(instantiate-coordinates rectangular-plane '(t n))
;
;(define f^theta
;  (literal-function 'f^theta (-> (UP Real Real) Real)))
;
;(define f^phi
;  (literal-function 'f^phi (-> (UP Real Real) Real)))
;
;(define s0
;  (simplify
;   (let* ( ;; d/dt and d/dn exist
;	  (mu:N->M (compose (M '->point)
;			    (up f^theta f^phi)
;			    (rectangular-plane '->coords)))
;	  (basis-over-mu (basis->basis-over-map mu:N->M M-basis))
;	  (1form-basis (basis->1form-basis basis-over-mu))
;	  (Cartan-over-mu
;	   (Christoffel->Cartan-over-map G-S2-1 mu:N->M))
;	  (nablau
;	   ((covariant-derivative Cartan-over-mu) 
;	    d/dt))
;	  (d1 (nablau (nablau ((differential mu:N->M) d/dn))))
;	  (d2 (((Riemann-curvature Cartan-over-mu) d/dn d/dt)
;	       ((differential mu:N->M) d/dt)))
;	  (deviation (+ d1 d2)))
;     (s:map/r 
;      (lambda (w)
;	((w deviation) ((rectangular-plane '->point) (up 'tau 0))))
;      1form-basis))))
;
;(define s1
;  (substitute 'xidotdot '(((partial 0) ((partial 0) ((partial 1) f^theta))) (up tau 0)) s0))
;
;(define s2
;  (substitute 'etadotdot '(((partial 0) ((partial 0) ((partial 1) f^phi))) (up tau 0)) s1))
;
;(define s3
;  (substitute 'phidotdot '(((partial 0) ((partial 0) f^phi)) (up tau 0)) s2))
;
;(define s4
;  (substitute 'thetadotdot '(((partial 0) ((partial 0) f^theta)) (up tau 0)) s3))
;
;(define s5
;  (substitute 'etadot '(((partial 0) ((partial 1) f^phi)) (up tau 0)) s4))
;
;(define s6
;  (substitute 'xidot '(((partial 0) ((partial 1) f^theta)) (up tau 0)) s5))
;
;(define s7
;  (substitute 'xi '(((partial 1) f^theta) (up tau 0)) s6))
;
;(define s8
;  (substitute 'eta '(((partial 1) f^phi) (up tau 0)) s7))
;
;(define s9
;  (substitute 'thetadot '(((partial 0) f^theta) (up tau 0)) s8))
;
;(define s10
;  (substitute 'phidot '(((partial 0) f^phi) (up tau 0)) s9))
;
;(define s11
;  (substitute 'theta '(f^theta (up tau 0)) s10))
;
;(define s12
;  (substitute 'phi '(f^phi (up tau 0)) s11))
;
;
;;;; Substituting from the geodesic equation (equation of motion) to
;;;; make make use of the fact that the trajectory is a geodesic.
;
;(define s13
;  (substitute '(* -2 thetadot phidot (/ (cos theta) (sin theta))) 'phidotdot s12))
;
;(define s14
;  (substitute '(* phidot phidot (cos theta) (sin theta)) 'thetadotdot s13))
;
;(pec s14)
;#| Result:
;(up
; (+ (* -2 (cos theta) (sin theta) phidot etadot)
;    (* (+ 1 (* -2 (expt (cos theta) 2))) xi (expt phidot 2))
;    xidotdot)
; (/ (+ (* 2 (cos theta) (sin theta) thetadot etadot)
;       (* (expt (sin theta) 2) etadotdot)
;       (* (+ (* -2 xi thetadot) (* 2 (cos theta) (sin theta) xidot)) phidot))
;    (expt (sin theta) 2)))
;|#
;
;;;; These geodesic deviation equations are the variational equations
;;;; driven by the geodesic equation.
;|#

;#|
;;;; Testing equation 3 on MTW p272
;
;(define s0
;  (simplify
;   (let* ( ;; d/dt and d/dn exist
;	  (mu:N->M (compose 
;		    (M '->point)
;		    (up f^theta f^phi)
;		    (rectangular-plane '->coords)))
;	  (basis-over-mu (basis->basis-over-map mu:N->M M-basis))
;	  (1form-basis (basis->1form-basis basis-over-mu))
;	  (Cartan-over-mu
;	   (Christoffel->Cartan-over-map G-S2-1 mu:N->M))
;	  (nablau
;	   ((covariant-derivative Cartan-over-mu) d/dt))
;	  (nablan
;	   ((covariant-derivative Cartan-over-mu) d/dn))
;	  (deviation (nablan (nablau ((differential mu:N->M) d/dt)))))
;     (s:map/r 
;      (lambda (w)
;	((w deviation) ((rectangular-plane '->point) (up 'tau 0))))
;      1form-basis))))
;
;do all substitutions again...
;(pec s12)
;#| Result:
;(up
; (+ (* (+ (* -2 (expt (cos theta) 2) thetadot phidot)
;	  (* -1 (cos theta) (sin theta) phidotdot))
;       eta)
;    (* -2 (cos theta) (sin theta) phidot etadot)
;    (* (+ 1 (* -2 (expt (cos theta) 2))) xi (expt phidot 2))
;    xidotdot)
; (/ (+ (* (+ (* -1 (expt (cos theta) 2) (sin theta) (expt phidot 2))
;	     (* (cos theta) thetadotdot))
;	  eta)
;       (* 2 (cos theta) thetadot etadot)
;       (* (sin theta) etadotdot)
;       (* (+ (* -2 (sin theta) xi thetadot) (* 2 (cos theta) xidot)) phidot)
;       (* (cos theta) xi phidotdot))
;    (sin theta)))
;|#
;
;(pec s14)
;#| Result:
;(up
; (+ (* -2 (cos theta) (sin theta) phidot etadot)
;    (* (+ 1 (* -2 (expt (cos theta) 2))) xi (expt phidot 2))
;    xidotdot)
; (/ (+ (* 2 (cos theta) (sin theta) thetadot etadot)
;       (* (expt (sin theta) 2) etadotdot)
;       (* (+ (* -2 xi thetadot) (* 2 (cos theta) (sin theta) xidot)) phidot))
;    (expt (sin theta) 2)))
;|#
;
;agrees with Riemann calculation
;
;shouldn't this be zero?
;
;|#

;#|
;;;; parallel transport of vector about a loop
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
;
;;;; Ordinary Lagrange Equations (= Geodesic Equations)
;
;(pec (let ((U d/dt)
;	   (mu:N->M (compose (S '->point)
;			     (up (literal-function 'f^theta)
;				 (literal-function 'f^phi))
;			     (the-real-line '->coords))))
;       (let* ((basis-over-mu (basis->basis-over-map mu:N->M 2-sphere-basis))
;	      (1form-basis (basis->1form-basis basis-over-mu))
;	      (Cartan-over-mu
;	       (Christoffel->Cartan-over-map G-S2-1 mu:N->M)))
;	 (s:map/r 
;	  (lambda (w)
;	    ((w (((covariant-derivative Cartan-over-mu) U)
;		 ((differential mu:N->M) U)))
;	     ((the-real-line '->point) 'tau)))
;	  1form-basis))))
;#| Result:
;(up
; (+ (((expt D 2) f^theta) tau)
;    (* -1 (cos (f^theta tau)) (sin (f^theta tau)) (expt ((D f^phi) tau) 2)))
; (/ (+ (* (sin (f^theta tau)) (((expt D 2) f^phi) tau))
;       (* 2 (cos (f^theta tau)) ((D f^phi) tau) ((D f^theta) tau)))
;    (sin (f^theta tau))))
;|#
;
;;;; Parallel transport of vector W over path mu
;
;(pec (let ((U d/dt)
;	   (mu:N->M (compose (S '->point)
;			     (up (literal-function 'f^theta)
;				 (literal-function 'f^phi))
;			     (the-real-line '->coords))))
;       (let* ((basis-over-mu (basis->basis-over-map mu:N->M 2-sphere-basis))
;	      (1form-basis (basis->1form-basis basis-over-mu))
;	      (vector-basis (basis->vector-basis basis-over-mu))
;	      (Cartan-over-mu
;	       (Christoffel->Cartan-over-map G-S2-1 mu:N->M))
;	      (transported-vector-over-map 
;	       (basis-components->vector-field
;		(up (compose (literal-function 'w^0)
;			     (the-real-line '->coords))
;		    (compose (literal-function 'w^1)
;			     (the-real-line '->coords)))
;		vector-basis)))
;	 (s:map/r 
;	  (lambda (w)
;	    ((w
;	      (((covariant-derivative Cartan-over-mu) U)
;	       transported-vector-over-map))
;	     ((the-real-line '->point) 'tau)))
;	  1form-basis))))
;#| Result:
;(up
; (+ ((D w^0) tau)
;    (* -1 (cos (f^theta tau)) ((D f^phi) tau) (w^1 tau) (sin (f^theta tau))))
; (/ (+ (* (sin (f^theta tau)) ((D w^1) tau))
;       (* (cos (f^theta tau)) ((D f^phi) tau) (w^0 tau))
;       (* (cos (f^theta tau)) (w^1 tau) ((D f^theta) tau)))
;    (sin (f^theta tau))))
;|#
; 
;#| was  ...  looks like right hand side
;
;(up (* (sin (theta tau)) (cos (theta tau)) (w^1 tau)
;       ((D phi) tau))
;    (/ (+ (* -1 (w^0 tau) (cos (theta tau)) ((D phi) tau))
;	  (* -1 ((D theta) tau) (cos (theta tau)) (w^1 tau)))
;       (sin (theta tau))))
;
;|#
;
;;;; To set up for solving for the derivatives, we lift off of the path
;
;(pec (let ((U d/dt)
;	   (mu:N->M (compose (S '->point)
;			     (up (literal-function 'f^theta)
;				 (literal-function 'f^phi))
;			     (the-real-line '->coords))))
;       (let* ((basis-over-mu (basis->basis-over-map mu:N->M 2-sphere-basis))
;	      (1form-basis (basis->1form-basis basis-over-mu))
;	      (vector-basis (basis->vector-basis basis-over-mu))
;	      (Cartan-over-mu
;	       (Christoffel->Cartan-over-map G-S2-1 mu:N->M))
;	      (transported-vector-over-map 
;	       (basis-components->vector-field
;		(up (compose (osculating-path (up 'tau 'w^0 'dw^0/dt))
;			     (the-real-line '->coords))
;		    (compose (osculating-path (up 'tau 'w^1 'dw^1/dt))
;			     (the-real-line '->coords)))
;		vector-basis)))
;	 (s:map/r 
;	  (lambda (w)
;	    ((w
;	      (((covariant-derivative Cartan-over-mu)
;		U)
;	       transported-vector-over-map))
;	     ((the-real-line '->point) 'tau)))
;	  1form-basis))))
;#| Result:
;(up (+ dw^0/dt
;       (* -1 (cos (f^theta tau)) ((D f^phi) tau) (sin (f^theta tau)) w^1))
;    (/ (+ (* (sin (f^theta tau)) dw^1/dt)
;	  (* (cos (f^theta tau)) ((D f^phi) tau) w^0)
;	  (* (cos (f^theta tau)) ((D f^theta) tau) w^1))
;       (sin (f^theta tau))))
;|#
;
;;;; Loaded solve by (load "/usr/local/scmutils/src/solve/linreduce")
;
;(set! *divide-out-terms* #f)
;;Value: #t
;
;(let ((tau 'tau)
;      (theta (literal-function 'f^theta))
;      (phi (literal-function 'f^phi))
;      (w^0 (literal-function 'w^0))
;      (w^1 (literal-function 'w^1)))
;  (pec (solve
;	(lambda (v)
;	  (let ((dw^0/dt (ref v 0))
;		(dw^1/dt (ref v 1)))
;	    (up
;	     (+ (* -1 (w^1 tau) (sin (theta tau)) (cos (theta tau)) ((D phi) tau))
;		dw^0/dt)
;	     (+ (/ (* (w^0 tau) (cos (theta tau)) ((D phi) tau)) (sin (theta tau)))
;		(/ (* (w^1 tau) ((D theta) tau) (cos (theta tau))) (sin (theta tau)))
;		dw^1/dt))))
;	2 2)))
;#| Result:
;(up (* (w^1 tau) (sin (f^theta tau)) (cos (f^theta tau)) ((D f^phi) tau))
;    (/ (+ (* -1 (w^1 tau) (cos (f^theta tau)) ((D f^theta) tau))
;	  (* -1 (cos (f^theta tau)) ((D f^phi) tau) (w^0 tau)))
;       (sin (f^theta tau))))
;|#
;
;(pec (let ((U d/dt)
;	   (mu:N->M (compose (S '->point)
;			     (up (literal-function 'f^theta)
;				 (literal-function 'f^phi))
;			     (the-real-line '->coords))))
;       (solve 
;	(lambda (v)
;	  (let ((dw^0/dt (ref v 0))
;		(dw^1/dt (ref v 1)))
;	    (let* ((basis-over-mu (basis->basis-over-map mu:N->M 2-sphere-basis))
;		   (1form-basis (basis->1form-basis basis-over-mu))
;		   (vector-basis (basis->vector-basis basis-over-mu))
;		   (Cartan-over-mu
;		    (Christoffel->Cartan-over-map G-S2-1 mu:N->M))
;		   (transported-vector-over-map 
;		    (basis-components->vector-field
;		     (up (compose (osculating-path (up 'tau 'w^0 dw^0/dt))
;				  (the-real-line '->coords))
;			 (compose (osculating-path (up 'tau 'w^1 dw^1/dt))
;				  (the-real-line '->coords)))
;		     vector-basis)))
;	      (s:map/r 
;	       (lambda (w)
;		 ((w
;		   (((covariant-derivative Cartan-over-mu)
;		     U)
;		    transported-vector-over-map))
;		  ((the-real-line '->point) 'tau)))
;	       1form-basis))))
;	(S 'dimension)
;	(S 'dimension))))
;#| Result:
;(up
; (* (sin (f^theta tau)) ((D f^phi) tau) (cos (f^theta tau)) w^1)
; (/
;  (+ (* -1 ((D f^phi) tau) (cos (f^theta tau)) w^0)
;     (* -1 (cos (f^theta tau)) ((D f^theta) tau) w^1))
;  (sin (f^theta tau))))
;|#
;|#

;#|
;;;; Computing parallel transport without the embedding
;
;(instantiate-coordinates the-real-line 't)
;
;(define M (rectangular 2))
;(instantiate-coordinates M '(theta phi))
;(define M-basis (coordinate-system->basis M))
; 
;(define G-S2-1
;  (make-Christoffel
;   (let ((zero  (lambda (point) 0))) 
;     (down (down (up zero zero)
;		 (up zero (/ 1 (tan theta))))
;	   (down (up zero (/ 1 (tan theta)))
;		 (up (- (* (sin theta) (cos theta))) zero))))
;   M-basis))
;
;
;;;; Parallel transport of vector w over path mu
;
;(define mu:N->M
;  (compose (M '->point)
;	   (up (literal-function 'mu^theta)
;	       (literal-function 'mu^phi))
;	   (the-real-line '->coords)))
;
;(define basis-over-mu
;  (basis->basis-over-map mu:N->M M-basis))
;
;(define w
;  (basis-components->vector-field
;   (up (compose (literal-function 'w^0)
;		(the-real-line '->coords))
;       (compose (literal-function 'w^1)
;		(the-real-line '->coords)))
;   (basis->vector-basis basis-over-mu)))
;
;(pec (let ((Cartan-over-mu
;	   (Christoffel->Cartan-over-map G-S2-1 mu:N->M)))
;	(s:map/r 
;	 (lambda (omega)
;	   ((omega
;	     (((covariant-derivative Cartan-over-mu) d/dt) w))
;	    ((the-real-line '->point) 'tau)))
;	 (basis->1form-basis basis-over-mu))))
;
;#| Result:
;(up (+ ((D w^0) tau)
;       (* -1
;	  (sin (mu^theta tau))
;	  (cos (mu^theta tau))
;	  ((D mu^phi) tau)
;	  (w^1 tau)))
;    (/ (+ (* (sin (mu^theta tau)) ((D w^1) tau))
;	  (* (cos (mu^theta tau)) (w^1 tau) ((D mu^theta) tau))
;	  (* (cos (mu^theta tau)) ((D mu^phi) tau) (w^0 tau)))
;       (sin (mu^theta tau))))
;|#
;;;; These are the equations of the coordinates of a vector being
;;;; parallel transported along the path defined by f.
;|#

;#|
;;;; To integrate these equations of the coordinates of the vector
;;;; being transported along a path (mu^theta(tau), mu^phi(tau)), defined
;;;; by differential equations we need to make a state space that
;;;; represents both the path and the coordinates of the vector being
;;;; transported.  The states are s=(sigma, w)=((theta, phi), (w0, w1))
;;;; and the differential equations for the path are Dsigma(tau) =
;;;; b(sigma(tau)).  The differential equations for the coordinates of
;;;; the vector are driven by this path.
;
;;;; To represent these states we make a new manifold with 4
;;;; coordinates.  The first two coordinates are tha coordinates of the
;;;; path.  The second two coordinates are the components of the vector
;;;; to be transported, relative to the coordinate directions in the
;;;; original manifold.  The right-hand side of the composite
;;;; differential equation is a vector field on this manifold.
;
;(define states (rectangular 4))
;(instantiate-coordinates states '(theta phi w0 w1))
;
;(define initial-state-d/dphi
;  ((states '->point) (up 'theta0 'phi0 0 1)))
;(define initial-state-d/dtheta
;  ((states '->point) (up 'theta0 'phi0 1 0)))
;
;
;;;; Assuming that the paths are integral curves of a vector field v,
;;;; we supply the vector field:
;
;(define (G v)
;  (let ((alphadot (dtheta v)) (betadot (dphi v)))
;    (+ v
;       (* (compose (* sin cos) theta) betadot w1 d/dw0)
;       (* -1
;	  (compose (/ cos sin) theta)
;	  (+ (* w0 betadot) (* w1 alphadot))
;	  d/dw1))))
;
;(define Gu (G d/dtheta))
;
;(define Gv (G d/dphi))
;
;(define (initial-state initial-coords w)
;  (let ((theta0 (ref initial-coords 0))
;	(phi0 (ref initial-coords 1)))
;    (let ((dummy
;	   ((states '->point)
;	    (up theta0 phi0 'foo 'bar))))
;      ((states '->point)
;       (up theta0 phi0
;	   ((dw0 w) dummy)
;	   ((dw1 w) dummy))))))
;
;
;(pec ((dw0 (commutator Gu Gv))
;      (initial-state (up 'theta0 'phi0) d/dw1)))
;#| Result:
;(* -1 (expt (sin theta0) 2))
;|#
;
;(pec ((dw1 (commutator Gu Gv))
;      (initial-state (up 'theta0 'phi0) d/dw0)))
;#| Result:
;1
;|#
;;;; Gee, this gets the right answer.
;|#

;#|
;;;; To integrate these equations of the coordinates of the vector
;;;; being transported along a path (mu^theta(tau), mu^phi(tau)), defined
;;;; by differential equations we need to make a state space that
;;;; represents both the path and the coordinates of the vector being
;;;; transported.  The states are s=(sigma, w)=((theta, phi), (w0, w1))
;;;; and the differential equations for the path are Dsigma(tau) =
;;;; b(sigma(tau)).  The differential equations for the coordinates of
;;;; the vector are driven by this path.
;
;;;; To represent these states we make a new manifold with 4
;;;; coordinates.  The first two coordinates are tha coordinates of the
;;;; path.  The second two coordinates are the components of the vector
;;;; to be transported, relative to the coordinate directions in the
;;;; original manifold.  The right-hand side of the composite
;;;; differential equation is a vector field on this manifold.
;
;(define M (rectangular 2))
;(instantiate-coordinates M '(theta phi))
;
;(define states (rectangular 4))
;(instantiate-coordinates states '(Theta Phi w0 w1))
;
;;;; Assuming that the paths are integral curves of a vector field v,
;;;; we supply the vector field:
;
;(define (G v)
;  (let ((alphadot (dTheta v)) (betadot (dPhi v)))
;    (+ v
;       (* (compose (* sin cos) Theta) betadot w1 d/dw0)
;       (* -1
;	  (compose (/ cos sin) Theta)
;	  (+ (* w0 betadot) (* w1 alphadot))
;	  d/dw1))))
;
;(define Gu (G d/dTheta))
;(define Gv (G d/dPhi))
;
;(define (initial-state initial-coords w)
;  (let ((Theta0 (ref initial-coords 0))
;	(Phi0 (ref initial-coords 1)))
;    (let ((m ((M '->point) (up Theta0 Phi0))))
;      ((states '->point)
;       (up Theta0 Phi0
;	   ((dtheta w) m) ((dphi w) m))))))
;
;
;(pec ((dw0 (commutator Gu Gv))
;      (initial-state (up 'Theta0 'Phi0) d/dphi)))
;#| Result:
;(* -1 (expt (sin Theta0) 2))
;|#
;
;(pec ((dw1 (commutator Gu Gv))
;      (initial-state (up 'Theta0 'Phi0) d/dtheta)))
;#| Result:
;1
;|#
;;;; Gee, this gets the right answer.
;|#


;;;----------------------------------------------------------------
;;; try to improve this

;#|
;
;let gamma be the path that we are transporting along
;gamma(t)->M
;
;dgamma(d/dt)(f)(t) is the velocity vector, a vector over the map gamma
;
;when gamma is an integral curve of v, then 
;v(f)(gamma(t)) = dgamma(d/dt)(f)(t)
;
;let w be an arbitrary vector over the map
;w(f)(t) = d/dtheta (f)(gamma(t)) a_0(t) + d/dphi (f)(gamma(t)) a_1(t) 
;
;
;
;|#