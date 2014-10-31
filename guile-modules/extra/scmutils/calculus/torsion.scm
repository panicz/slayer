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

;;; use the connection derived from Lagrange equations on a sphere
;;; compute torsion for the non-symmetrized connection
;;; compute curvature to see if different from the symmetrized connection

;;; find the formula for the connection from the Lagrangian

(define ((Lfree m) s)
  (* 1/2 m (square (velocity s))))

(define ((F R) s)
  (let ((q (coordinate s)))
    (let ((theta (ref q 0))
	  (phi (ref q 1)))
      (up (* R (sin theta) (cos phi))
	  (* R (sin theta) (sin phi))
	  (* R (cos theta))))))

(define (Lsphere m R)
  (compose (Lfree m) (F->C (F R))))

;#|
;
;(pe (((Lagrange-equations (Lsphere 'm 'R))
;      (up (literal-function 'theta)
;	  (literal-function 'phi)))
;     't))
;(down
; (+ (* -1 (expt R 2) m (cos (theta t)) (sin (theta t)) (expt ((D phi) t) 2))
;    (* (expt R 2) m (((expt D 2) theta) t)))
; (+
;  (* 2 (expt R 2) m ((D theta) t) (cos (theta t)) (sin (theta t)) ((D phi) t))
;  (* (expt R 2) m (((expt D 2) phi) t) (expt (sin (theta t)) 2))))
;
;solving for the highest order terms...
;
;(up
; (+ (((expt D 2) theta) t)
;    (* -1  (cos (theta t)) (sin (theta t)) (expt ((D phi) t) 2)))
; (+ (((expt D 2) phi) t)
;    (/ (* 2 (cos (theta t)) ((D theta) t) ((D phi) t)) (sin (theta t)))))
;|#

;#|
;(instantiate-coordinates the-real-line 't)
;(define M (rectangular 2))
;(instantiate-coordinates M '(theta phi))
;(define M-basis (coordinate-system->basis M))
;
;(define Gamma-sphere
;  (make-Christoffel
;   (let ((zero (lambda (point) 0)))
;     (down (down (up zero zero)
;		 (up zero (/ 2 (tan theta))))
;	   (down (up zero zero)
;		 (up (- (* (sin theta) (cos theta))) zero))))
;   M-basis))
;
;(define gamma
;  (compose (M '->point)
;	   (up (literal-function 'theta)
;	       (literal-function 'phi))
;	   (the-real-line '->coords)))
;
;(define basis-over-gamma
;  (basis->basis-over-map gamma M-basis))
;
;(define sphere-Cartan-over-gamma
;  (Christoffel->Cartan-over-map Gamma-sphere gamma))
;(define sphere-Cartan
;  (Christoffel->Cartan Gamma-sphere))
;
;;;; geodesic equations
;
;(pe
; (((((covariant-derivative sphere-Cartan-over-gamma) d/dt)
;    ((differential gamma) d/dt))
;   (M '->coords))
;  ((the-real-line '->point) 't)))
;(up
; (+ (((expt D 2) theta) t)
;    (* -1 (sin (theta t)) (cos (theta t)) (expt ((D phi) t) 2)))
; (+ (((expt D 2) phi) t)
;    (/ (* 2 ((D theta) t) (cos (theta t)) ((D phi) t))
;       (sin (theta t)))))
;
;agrees with the Lagrange equations.
;|#

;#|
;torsion for this connection
;
;(define a-function
;  (compose
;   (literal-function 'f (-> (UP Real Real) Real))
;   (M '->coords)))
;
;(for-each
; (lambda (x)
;   (for-each
;    (lambda (y)
;      (pe
;       ((((torsion sphere-Cartan) x y) a-function)
;	((M '->point) (up 'theta 'phi)))))
;    (list d/dtheta d/dphi)))
; (list d/dtheta d/dphi))
;0
;(/ (* 2 (((partial 1) f) (up theta phi)) (cos theta)) (sin theta))
;(/ (* -2 (((partial 1) f) (up theta phi)) (cos theta)) (sin theta))
;0
;
;nonzero torsion
;
;|#



;#|
;
;now compute curvature
;
;(pe
; (((Riemann sphere-Cartan) dphi d/dtheta d/dphi d/dtheta)
;  ((M '->point) (up 'theta 'phi))))
;0
;is one for the symmetric connection
;
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
;	    (pe (((Riemann sphere-Cartan) 
;		  alpha beta gamma delta)
;		 ((M '->point) (up 'theta 'phi)))))
;	  (list d/dtheta d/dphi)))
;       (list d/dtheta d/dphi)))
;    (list d/dtheta d/dphi)))
; (list dtheta dphi))
;;;; nonzero elements are...
;(dtheta d/dphi d/dtheta d/dphi)
;1
;
;(dtheta d/dphi d/dphi d/dtheta)
;-1
;
;;;; NOT SAME AS SYMMETRIC CASE ....
;
;|#

;#|
;
;check with usual connection...
;
;(define symmetric-Gamma-sphere
;  (make-Christoffel
;   (let ((zero (lambda (point) 0)))
;     (down (down (up zero zero)
;		 (up zero (/ 1 (tan theta))))
;	   (down (up zero (/ 1 (tan theta)))
;		 (up (- (* (sin theta) (cos theta))) zero))))
;   M-basis))
;
;(define symmetric-sphere-Cartan-over-gamma
;  (Christoffel->Cartan-over-map symmetric-Gamma-sphere gamma))
;(define symmetric-sphere-Cartan
;  (Christoffel->Cartan symmetric-Gamma-sphere))
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
;	    (pe (((Riemann symmetric-sphere-Cartan) 
;		  alpha beta gamma delta)
;		 ((M '->point) (up 'theta 'phi)))))
;	  (list d/dtheta d/dphi)))
;       (list d/dtheta d/dphi)))
;    (list d/dtheta d/dphi)))
; (list dtheta dphi))
;;;; nonzero components
;
;(dtheta d/dphi d/dtheta d/dphi)
;(expt (sin theta) 2)
;
;(dtheta d/dphi d/dphi d/dtheta)
;(* -1 (expt (sin theta) 2))
;
;(dphi d/dtheta d/dtheta d/dphi)
;-1
;
;(dphi d/dtheta d/dphi d/dtheta)
;1
;
;|#

;#|
;
;modified from ricci.scm
;
;(define ((Ricci Cartan) u v)
;  (let ((R (Riemann-curvature Cartan)))
;    (contract-2 
;     (lambda (d/dx dx) (dx ((R d/dx v) u)))
;     (Cartan->basis Cartan))))
;
;(for-each
; (lambda (alpha)
;   (for-each
;    (lambda (beta)
;      (pe `(,alpha ,beta))
;      (pe (((Ricci symmetric-sphere-Cartan) 
;		  alpha beta)
;	   ((M '->point) (up 'theta 'phi)))))
;    (list d/dtheta d/dphi)))
; (list d/dtheta d/dphi))
;
;(d/dtheta d/dtheta)
;1
;(d/dtheta d/dphi)
;0
;(d/dphi d/dtheta)
;0
;(d/dphi d/dphi)
;(expt (sin theta) 2)
;
;(for-each
; (lambda (alpha)
;   (for-each
;    (lambda (beta)
;      (pe `(,alpha ,beta))
;      (pe (((Ricci sphere-Cartan) 
;		  alpha beta)
;	   ((M '->point) (up 'theta 'phi)))))
;    (list d/dtheta d/dphi)))
; (list d/dtheta d/dphi))
;(d/dtheta d/dtheta)
;0
;(d/dtheta d/dphi)
;0
;(d/dphi d/dtheta)
;0
;(d/dphi d/dphi)
;1
;
;;;; Ricci curvatures are different
;
;;;; to get the scalar curvature we need to raise one index 
;;;; and then contract =>  so need a metric
;
;
;|#


;#|
;Derive equations of parallel transport so we can go around loops 
;and compare to the Riemann tensor ...
;
;(define w
;  (basis-components->vector-field
;   (up (compose (literal-function 'w0)
;		(the-real-line '->coords))
;       (compose (literal-function 'w1)
;		(the-real-line '->coords)))
;   (basis->vector-basis basis-over-gamma)))
;
;(pe 
; (s:map/r
;  (lambda (omega)
;    ((omega 
;      (((covariant-derivative sphere-Cartan-over-gamma)
;	d/dt)
;       w))
;      ((the-real-line '->point) 'tau)))
;  (basis->1form-basis basis-over-gamma)))
;(up
; (+ (* -1 (sin (theta tau)) (cos (theta tau)) ((D phi) tau) (w1 tau))
;    ((D w0) tau))
; (+ (/ (* 2 ((D theta) tau) (cos (theta tau)) (w1 tau))
;       (sin (theta tau)))
;    ((D w1) tau)))
;
;(define states (rectangular 4))
;(instantiate-coordinates states '(Theta Phi w0 w1))
;
;(define (G v)
;  (let ((adot (dTheta v))
;	(bdot (dPhi v)))
;    (+ v 
;       (* (compose (* sin cos) Theta) bdot w1 d/dw0)
;       (* -1 
;	  (compose (/ cos sin) Theta)
;	  (* 2 w1 adot)
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
;       (up Theta0 Phi0 ((dtheta w) m) ((dphi w) m))))))
;
;(pe
; ((dw0 (commutator Gu Gv))
;  (initial-state (up 'Theta0 'Phi0) d/dphi)))
;-1
;
;(pe
; ((dw0 (commutator Gv Gu))
;  (initial-state (up 'Theta0 'Phi0) d/dphi)))
;1
;
;(pe
; ((dw1 (commutator Gu Gv))
;  (initial-state (up 'Theta0 'Phi0) d/dtheta)))
;0
;
;(pe
; ((dw1 (commutator Gv Gu))
;  (initial-state (up 'Theta0 'Phi0) d/dtheta)))
;0
;
;|#



;#|
;;;; redo transport for symmetric case to compare
;
;(define (Gs v)
;  (let ((adot (dTheta v))
;	(bdot (dPhi v)))
;    (+ v 
;       (* (compose (* sin cos) Theta) bdot w1 d/dw0)
;       (* -1 
;	  (compose (/ cos sin) Theta)
;	  (+ (* w0 bdot) (* w1 adot))
;	  d/dw1))))
;	  
;(define Gsu (Gs d/dTheta))
;(define Gsv (Gs d/dPhi))
;
;(define (initial-state initial-coords w)
;  (let ((Theta0 (ref initial-coords 0))
;	(Phi0 (ref initial-coords 1)))
;    (let ((m ((M '->point) (up Theta0 Phi0))))
;      ((states '->point)
;       (up Theta0 Phi0 ((dtheta w) m) ((dphi w) m))))))
;
;(pe
; ((dw0 (commutator Gsu Gsv))
;  (initial-state (up 'Theta0 'Phi0) d/dphi)))
;(* -1 (expt (sin Theta0) 2))
;;;; nonsymmetric = -1
;
;(pe
; ((dw0 (commutator Gsv Gsu))
;  (initial-state (up 'Theta0 'Phi0) d/dphi)))
;(expt (sin Theta0) 2)
;;;; nonsymmetric = 1
;
;(pe
; ((dw1 (commutator Gsu Gsv))
;  (initial-state (up 'Theta0 'Phi0) d/dtheta)))
;1
;;;; nonsymmetric = 0
;
;(pe
; ((dw1 (commutator Gsv Gsu))
;  (initial-state (up 'Theta0 'Phi0) d/dtheta)))
;-1
;;;; nonsymmetric = 0
;
;
;|#
