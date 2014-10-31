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

;;;; Chapter 2

;;; Generalized coordinates to angular velocities.

(define (m:antisymmetric? A)
  (m:zero? ((m:elementwise simplify)
	    (matrix+matrix (m:transpose A) A))))

(define (antisymmetric->column-matrix A)
  (assert (m:antisymmetric? A))
  (column-matrix (matrix-ref A 2 1)
		 (matrix-ref A 0 2)
		 (matrix-ref A 1 0)))

(define (3vector-components->antisymmetric v)
  (matrix-by-rows
   (list 0 (- (ref v 2)) (ref v 1))
   (list (ref v 2) 0 (- (ref v 0)))
   (list (- (ref v 1)) (ref v 0) 0)))

(define (((M-of-q->omega-of-t M-of-q) q) t)
  (define M-on-path (compose M-of-q q))
  (define (omega-cross t)
    (* ((D M-on-path) t)
       (m:transpose (M-on-path t))))
  (antisymmetric->column-matrix (omega-cross t)))

(define (((M-of-q->omega-body-of-t M-of-q) q) t)
  (* (m:transpose (M-of-q (q t)))
     (((M-of-q->omega-of-t M-of-q) q) t)))

(define (M->omega M-of-q)
  (Gamma-bar (M-of-q->omega-of-t M-of-q)))

(define (M->omega-body M-of-q)
  (Gamma-bar (M-of-q->omega-body-of-t M-of-q)))


;;; Assuming omega-body is on principal axes,
;;; and A, B, C are the principal moments.
;;; Angular velocity to kinetic energy and angular momenta

(define ((T-rigid A B C) omega-body)
  (* 1/2
     (+ (* A (square (ref omega-body 0)))
	(* B (square (ref omega-body 1)))
	(* C (square (ref omega-body 2))))))

(define ((L-body A B C) omega-body)
  (column-matrix (* A (ref omega-body 0))
		 (* B (ref omega-body 1))
		 (* C (ref omega-body 2))))

;;; Euler Angles

(define (rotate-z-matrix angle)
  (matrix-by-rows
    (list (cos angle) (- (sin angle))               0)
    (list (sin angle)     (cos angle)               0)
    (list           0               0               1)))

(define (rotate-x-matrix angle)
  (matrix-by-rows 
    (list           1               0               0)
    (list           0     (cos angle) (- (sin angle)))
    (list           0     (sin angle)     (cos angle))))

;;; Not used for Euler.
(define (rotate-y-matrix angle)
  (matrix-by-rows 
    (list     (cos angle)     0  (sin angle))
    (list               0     1            0)
    (list (- (sin angle))     0  (cos angle))))


(define (Euler->M angles)
  (let ((theta (ref angles 0))
	(phi (ref angles 1))
	(psi (ref angles 2)))
    (* (rotate-z-matrix phi)
       (* (rotate-x-matrix theta)
	  (rotate-z-matrix psi)))))

(define ((Euler->omega angles-path) t)
  (define (M-on-path t)
    (Euler->M (angles-path t)))
  (define (w-cross t)
    (* ((D M-on-path) t)
       (m:transpose (M-on-path t))))
  (antisymmetric->column-matrix (w-cross t)))

(define ((Euler->omega-body angles-path) t)
  (* (m:transpose (Euler->M (angles-path t)))
     ((Euler->omega angles-path) t)))

;#|
;(show-expression
;  ((Euler->omega-body
;    (coordinate-tuple (literal-function 'theta)
;		      (literal-function 'phi)
;		      (literal-function 'psi)))
;   't))
;(matrix-by-rows
; (list
;  (+ (* ((D phi) t) (sin (theta t)) (sin (psi t)))
;     (* (cos (psi t)) ((D theta) t))))
; (list
;  (+ (* ((D phi) t) (sin (theta t)) (cos (psi t)))
;     (* -1 (sin (psi t)) ((D theta) t))))
; (list (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t))))
;|#

;#|
;(show-expression
; (((M-of-q->omega-body-of-t Euler->M)
;   (coordinate-tuple (literal-function 'theta)
;		     (literal-function 'phi)
;		     (literal-function 'psi)))
;  't))
;(matrix-by-rows
; (list
;  (+ (* ((D phi) t) (sin (theta t)) (sin (psi t)))
;     (* (cos (psi t)) ((D theta) t))))
; (list
;  (+ (* ((D phi) t) (sin (theta t)) (cos (psi t)))
;     (* -1 (sin (psi t)) ((D theta) t))))
; (list (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t))))
;
;(show-expression
; ((M->omega-body Euler->M)
;  (->local 't 
;           (coordinate-tuple 'theta 'phi 'psi)
;           (velocity-tuple 'thetadot 'phidot 'psidot))))
;(matrix-by-rows
; (list (+ (* phidot (sin psi) (sin theta)) (* thetadot (cos psi))))
; (list (+ (* phidot (sin theta) (cos psi)) (* -1 thetadot (sin psi))))
; (list (+ (* phidot (cos theta)) psidot)))
;|#

;;; Assuming Euler angles rotate principal axes from reference
;;; orientation.

(define (Euler-state->omega-body local)
  (let ((t (time local))
        (q (coordinate local))
        (qdot (velocity local)))
    (let ((theta (ref q 0))
          (psi (ref q 2))
          (thetadot (ref qdot 0))
          (phidot (ref qdot 1))
          (psidot (ref qdot 2)))
      (let ((omega-a (+ (* thetadot (cos psi))
                        (* phidot (sin theta) (sin psi))))
            (omega-b (+ (* -1 thetadot (sin psi))
                        (* phidot (sin theta) (cos psi))))
            (omega-c (+ (* phidot (cos theta)) psidot)))
        (column-matrix omega-a omega-b omega-c)))))

(define ((T-rigid-body A B C) local)
  (let ((omega-body (Euler-state->omega-body local)))
    (* 1/2
       (+ (* A (square (ref omega-body 0)))
	  (* B (square (ref omega-body 1)))
	  (* C (square (ref omega-body 2)))))))

(define ((Euler-state->L-body A B C) local)
  (let ((omega-body (Euler-state->omega-body local)))
    (column-matrix (* A (ref omega-body 0))
		   (* B (ref omega-body 1))
		   (* C (ref omega-body 2)))))

(define ((Euler-state->L-space A B C) local)
  (let ((angles (coordinate local)))
    (* (Euler->M angles)
       ((Euler-state->L-body A B C) local))))

;#|
;(define an-Euler-state
;  (->local 't
;           (coordinate-tuple 'theta 'phi 'psi)
;           (velocity-tuple 'thetadot 'phidot 'psidot)))
;
;(show-expression
; (ref
;   (((partial 2) (T-rigid-body 'A 'B 'C))
;    an-Euler-state)
;   1))
;(+ (* A phidot (expt (sin theta) 2) (expt (sin psi) 2))
;   (* B phidot (expt (cos psi) 2) (expt (sin theta) 2))
;   (* A thetadot (cos psi) (sin theta) (sin psi))
;   (* -1 B thetadot (cos psi) (sin theta) (sin psi))
;   (* C phidot (expt (cos theta) 2))
;   (* C psidot (cos theta)))
;
;(print-expression
; (- (ref ((Euler-state->L-space 'A 'B 'C) an-Euler-state) 2)        ;$L_z$
;    (ref (((partial 2) (T-rigid-body 'A 'B 'C)) an-Euler-state) 1)  ;$p_\phi$
;    ))
;0
;
;(print-expression
; (determinant
;  (((compose (partial 2) (partial 2)) 
;    (T-rigid-body 'A 'B 'C))
;   an-Euler-state)))
;(* A B C (expt (sin theta) 2))
;|#

(define (relative-error value reference-value)
  (if (zero? reference-value)
      (error "Zero reference value -- RELATIVE-ERROR")
      (/ (- value reference-value) reference-value)))

;#|
;(define (rigid-sysder A B C)
;  (Lagrangian->state-derivative (T-rigid-body A B C)))
;
;(define ((monitor-errors win A B C L0 E0) state)
;  (let ((t (time state))
;	(L ((Euler-state->L-space A B C) state))
;	(E ((T-rigid-body A B C) state)))
;    (plot-point win t (relative-error (ref L 0) (ref L0 0)))
;    (plot-point win t (relative-error (ref L 1) (ref L0 1)))
;    (plot-point win t (relative-error (ref L 2) (ref L0 2)))
;    (plot-point win t (relative-error E E0))))
;
;
;;;; rkqc4
;;;;(set! *ode-integration-method* 'qcrk4)
;;;;(define win (frame 0. 100. -1.e-12 1.e-12))
;
;;;; bulirsch-stoer
;(set! *ode-integration-method* 'bulirsch-stoer)
;(define win (frame 0. 100. -2.e-13 2.e-13))
;
;(let ((A 1.) (B (sqrt 2.)) (C 2.)
;      (state0 (up 0.0
;		  (up 1. 0. 0.)
;		  (up 0.1 0.1 0.1))))
;  (let ((L0 ((Euler-state->L-space A B C) state0))
;	(E0 ((T-rigid-body A B C) state0)))
;    ((evolve rigid-sysder A B C)
;     state0
;     (monitor-errors win A B C L0 E0)
;     0.1
;     100.0
;     1.0e-12)))
;
;(graphics-close win)
;(graphics-clear win)
;|#

;#|
;(show-expression
; ((T-rigid-body 'A 'A 'C) 
;   (->local 't 
;            (coordinate-tuple 'theta 'phi 'psi)
;            (velocity-tuple 'thetadot 'phidot 'psidot))))
;(+ (* 1/2 A (expt phidot 2) (expt (sin theta) 2))
;   (* 1/2 C (expt phidot 2) (expt (cos theta) 2))
;   (* C phidot psidot (cos theta))
;   (* 1/2 A (expt thetadot 2))
;   (* 1/2 C (expt psidot 2)))
;
;;;; Transformation of A(v):
;;;;  M^T A(Mv) M = A(v) for arbitrary v orthogonal M
;
;(print-expression
;  (let ((Euler (coordinate-tuple 'theta 'phi 'psi))
;	(v (coordinate-tuple 'x 'y 'z)))
;    (let ((M (Euler->M Euler)))
;      (- (* (3vector-components->antisymmetric (* M v))
;	    M)
;	 (* M
;	    (3vector-components->antisymmetric v))))))
;(matrix-by-rows (list 0 0 0) (list 0 0 0) (list 0 0 0))
;|#

;#| 
;;;; Configuration equations for Euler's equations with Euler angles
;   
;(print-expression
;  (let ((Euler (coordinate-tuple (literal-function 'theta)
;				 (literal-function 'phi)
;				 (literal-function 'psi))))
;    (antisymmetric->column-matrix 
;     (* (m:transpose ((Euler->M Euler) 't))
;	((D (Euler->M Euler)) 't)))))
;(matrix-by-rows
; (list
;  (+ (* ((D phi) t) (sin (psi t)) (sin (theta t)))
;     (* ((D theta) t) (cos (psi t)))))
; (list
;  (+ (* ((D phi) t) (sin (theta t)) (cos (psi t)))
;     (* -1 (sin (psi t)) ((D theta) t))))
; (list (+ (* (cos (theta t)) ((D phi) t)) ((D psi) t))))
;|#

;#|
;(define ((L-axisymmetric-top A C gMR) local)
;  (let ((q (coordinate local))
;        (qdot (velocity local)))
;    (let ((theta (ref q 0))
;          (thetadot (ref qdot 0))
;          (phidot (ref qdot 1))
;          (psidot (ref qdot 2)))
;      (+ (* 1/2 A
;            (+ (square thetadot)
;               (square (* phidot (sin theta)))))
;         (* 1/2 C
;            (square (+ psidot (* phidot (cos theta)))))
;         (* -1 gMR (cos theta))))))
;
;
;(define ((ueff p A C gMR) theta)
;  (+ (/ (square p) (* 2 C))
;     (* (/ (square p) (* 2 A))
;	(square (tan (/ theta 2))))
;     (* gMR (cos theta))))
;
;
;;;; Critical value of bifurcation when D^2 Ueff (0) = 0
;
;(print-expression
; (((square derivative) (ueff 'p_c 'A 'C 'gMR)) 0))
;(+ (* -1 gMR) (/ (* 1/4 (expt p_c 2)) A))
;
;;;; critical angular speed in RPM is:
;(* (/ 60 2pi) (/ 7.734804457773965e-3 6.6e-5))
;;Value: 1119.1203302763215
;|#
