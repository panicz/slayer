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

;;;; By a judicious choice of memoization, I got the Ricci tensor to
;;;; converge for the Schwarzschild metric!

(define-manifold 'spacetime Real 4 (up Real Real Real Real))

(define-coordinate-system spacetime 'rectangular (up 't 'x 'y 'z)
  (lambda (coords)
    (if (and (up? coords) (fix:= (s:dimension coords) 4))
	coords
	(error "Bad coordinates: spacetime-rectangular" coords)))
  (lambda (point)
    (if (and (up? point) (fix:= (s:dimension point) 4))
	point
	(error "Bad point: spacetime-rectangular" point))))

(define-coordinate-system spacetime 'spherical (up 't 'r 'theta 'phi)
  (lambda (coords)
    (if (and (up? coords) (fix:= (s:dimension coords) 4))
	(let ((t (ref coords 0))
	      (r (ref coords 1))
	      (theta (ref coords 2))
	      (phi   (ref coords 3)))
	  (up t
	      (* r (sin theta) (cos phi)) 
	      (* r (sin theta) (sin phi)) 
	      (* r (cos theta))))
  	(error "Bad coordinates: spacetime-spherical" coords)))
  (lambda (point)
    (if (and (up? point) (fix:= (s:dimension point) 4))
	(let ((t (ref point 0))
	      (x (ref point 1))
	      (y (ref point 2))
	      (z (ref point 3)))
	  (let ((r (sqrt (+ (square x) (square y) (square z)))))
	    (up t
		r
		(acos (/ z r))
		(atan y x))))
	(error "Bad point: spacetime-spherical" point))))

(define spacetime-rectangular (spacetime 'rectangular))
(define spacetime-spherical (spacetime 'spherical))

(install-coordinates spacetime-rectangular)
(install-coordinates spacetime-spherical)

(define r-event ((spacetime-rectangular '->point) (up 't0 'x0 'y0 'z0)))
(define s-event ((spacetime-spherical '->point) (up 't0 'r0 'theta0 'phi0)))

(define dT (1form-field->tensor-field dt))

(define dX (1form-field->tensor-field dx))
(define dY (1form-field->tensor-field dy))
(define dZ (1form-field->tensor-field dz))

(define dR (1form-field->tensor-field dr))
(define dTheta (1form-field->tensor-field dtheta))
(define dPhi (1form-field->tensor-field dphi))

(define (Schwarzschild-metric M G c)
  (let ((a (- 1 (/ (* 2 G M) (* (square c) r)))))
    (+ (*  -1 (square c) a (square dT))
       (* (/ 1 a) (square dR))
       (* (square r)
	  (+ (square dTheta)
	     (square (* (sin theta) dPhi)))))))

(define S-metric (Schwarzschild-metric 'M 'G 'c))


(define Gam
  (Christoffel S-metric spacetime-spherical))
;Value: Gamma

(show-time
 (lambda ()
   (pec ((tensor-field->coefficient-structure Gam spacetime-spherical)
	 s-event))))
;#| Result:
;(down
; (down
;  (up
;   0
;   (+ (/ (* G M) (expt r0 2))
;      (/ (* -2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r0 3))))
;   0
;   0)
;  (up (/ (* -1/2 G M) (+ (* -1/2 (expt c 2) (expt r0 2)) (* G M r0))) 0 0 0)
;  (up 0 0 0 0)
;  (up 0 0 0 0))
; (down (up (/ (* -1/2 G M) (+ (* -1/2 (expt c 2) (expt r0 2)) (* G M r0))) 0 0 0)
;       (up 0 (/ (* 1/2 G M) (+ (* -1/2 (expt c 2) (expt r0 2)) (* G M r0))) 0 0)
;       (up 0 0 (/ 1 r0) 0)
;       (up 0 0 0 (/ 1 r0)))
; (down (up 0 0 0 0)
;       (up 0 0 (/ 1 r0) 0)
;       (up 0 (+ (* -1 r0) (/ (* 2 G M) (expt c 2))) 0 0)
;       (up 0 0 0 (/ (cos theta0) (sin theta0))))
; (down
;  (up 0 0 0 0)
;  (up 0 0 0 (/ 1 r0))
;  (up 0 0 0 (/ (cos theta0) (sin theta0)))
;  (up
;   0
;   (+ (* -1 r0 (expt (sin theta0) 2)) (/ (* 2 G M (expt (sin theta0) 2)) (expt c 2)))
;   (* -1 (sin theta0) (cos theta0))
;   0)))
;|#
;; process time: 29300 (27390 RUN + 1910 GC); real time: 30922
;;; copied result, edited r0->r, theta0->theta

(define Gamma1
  (coefficient-structure->tensor-field
   (let ((G 'G)
	 (M 'M)
	 (c 'c))
     (down
      (down
       (up
	0
	(+ (/ (* G M) (expt r 2))
	   (/ (* -2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r 3))))
	0
	0)
       (up (/ (* -1/2 G M) (+ (* -1/2 (expt c 2) (expt r 2)) (* G M r))) 0 0 0)
       (up 0 0 0 0)
       (up 0 0 0 0))
      (down (up (/ (* -1/2 G M) (+ (* -1/2 (expt c 2) (expt r 2)) (* G M r))) 0 0 0)
	    (up 0 (/ (* 1/2 G M) (+ (* -1/2 (expt c 2) (expt r 2)) (* G M r))) 0 0)
	    (up 0 0 (/ 1 r) 0)
	    (up 0 0 0 (/ 1 r)))
      (down (up 0 0 0 0)
	    (up 0 0 (/ 1 r) 0)
	    (up 0 (+ (* -1 r) (/ (* 2 G M) (expt c 2))) 0 0)
	    (up 0 0 0 (/ (cos theta) (sin theta))))
      (down
       (up 0 0 0 0)
       (up 0 0 0 (/ 1 r))
       (up 0 0 0 (/ (cos theta) (sin theta)))
       (up
	0
	(+ (* -1 r (expt (sin theta) 2)) (/ (* 2 G M (expt (sin theta) 2)) (expt c 2)))
	(* -1 (sin theta) (cos theta))
	0))))
   spacetime-spherical))

(define Gamma-comma
  (show-time
   (lambda ()
     (t:comma Gamma1 spacetime-spherical))))
;; process time: 373300 (330080 RUN + 43220 GC); real time: 374217
;; process time: 178640 (159430 RUN + 19210 GC); real time: 180508
;; This time is mysteriously unstable, varying from 100000 to 350000.
;; It seems to depend on the phase of the moon.
;Value: Gamma-comma

(show-time
 (lambda ()
   (pec ((tensor-field->coefficient-structure Gamma-comma spacetime-spherical)
	 s-event))))
;#| Result:
;(down
; (down (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0)))
; (down
;  (down
;   (up
;    0
;    (+ (/ (* -2 G M) (expt r0 3))
;       (/ (* 6 (expt G 2) (expt M 2)) (* (expt c 2) (expt r0 4))))
;    0
;    0)
;   (up
;    (+
;     (/
;      (* -1/2 G M (expt c 2))
;      (+ (* 1/4 (expt c 4) (expt r0 3))
;         (* -1 G M (expt c 2) (expt r0 2))
;         (* (expt G 2) (expt M 2) r0)))
;     (/
;      (* 1/2 (expt G 2) (expt M 2))
;      (+ (* 1/4 (expt c 4) (expt r0 4))
;         (* -1 G M (expt c 2) (expt r0 3))
;         (* (expt G 2) (expt M 2) (expt r0 2)))))
;    0
;    0
;    0)
;   (up 0 0 0 0)
;   (up 0 0 0 0))
;  (down
;   (up
;    (+
;     (/
;      (* -1/2 G M (expt c 2))
;      (+ (* 1/4 (expt c 4) (expt r0 3))
;         (* -1 G M (expt c 2) (expt r0 2))
;         (* (expt G 2) (expt M 2) r0)))
;     (/
;      (* 1/2 (expt G 2) (expt M 2))
;      (+ (* 1/4 (expt c 4) (expt r0 4))
;         (* -1 G M (expt c 2) (expt r0 3))
;         (* (expt G 2) (expt M 2) (expt r0 2)))))
;    0
;    0
;    0)
;   (up
;    0
;    (+
;     (/
;      (* 1/2 G M (expt c 2))
;      (+ (* 1/4 (expt c 4) (expt r0 3))
;         (* -1 G M (expt c 2) (expt r0 2))
;         (* (expt G 2) (expt M 2) r0)))
;     (/
;      (* -1/2 (expt G 2) (expt M 2))
;      (+ (* 1/4 (expt c 4) (expt r0 4))
;         (* -1 G M (expt c 2) (expt r0 3))
;         (* (expt G 2) (expt M 2) (expt r0 2)))))
;    0
;    0)
;   (up 0 0 (/ -1 (expt r0 2)) 0)
;   (up 0 0 0 (/ -1 (expt r0 2))))
;  (down (up 0 0 0 0) (up 0 0 (/ -1 (expt r0 2)) 0) (up 0 -1 0 0) (up 0 0 0 0))
;  (down (up 0 0 0 0)
;        (up 0 0 0 (/ -1 (expt r0 2)))
;        (up 0 0 0 0)
;        (up 0 (* -1 (expt (sin theta0) 2)) 0 0)))
; (down
;  (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;  (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;  (down (up 0 0 0 0)
;        (up 0 0 0 0)
;        (up 0 0 0 0)
;        (up 0 0 0 (/ -1 (expt (sin theta0) 2))))
;  (down
;   (up 0 0 0 0)
;   (up 0 0 0 0)
;   (up 0 0 0 (/ -1 (expt (sin theta0) 2)))
;   (up
;    0
;    (+ (* -2 r0 (cos theta0) (sin theta0))
;       (/ (* 4 G M (cos theta0) (sin theta0)) (expt c 2)))
;    (+ 1 (* -2 (expt (cos theta0) 2)))
;    0)))
; (down (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))))
;|#
;; process time: 38300 (35000 RUN + 3300 GC); real time: 38393
;;; copied result, edited r0->r, theta0->theta

(define Gamma-comma1
  (coefficient-structure->tensor-field
   (let ((G 'G)
	 (M 'M)
	 (c 'c))
     (down
      (down (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
	    (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
	    (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
	    (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0)))
      (down
       (down
	(up
	 0
	 (+ (/ (* -2 G M) (expt r 3))
	    (/ (* 6 (expt G 2) (expt M 2)) (* (expt c 2) (expt r 4))))
	 0
	 0)
	(up
	 (+
	  (/
	   (* -1/2 G M (expt c 2))
	   (+ (* 1/4 (expt c 4) (expt r 3))
	      (* -1 G M (expt c 2) (expt r 2))
	      (* (expt G 2) (expt M 2) r)))
	  (/
	   (* 1/2 (expt G 2) (expt M 2))
	   (+ (* 1/4 (expt c 4) (expt r 4))
	      (* -1 G M (expt c 2) (expt r 3))
	      (* (expt G 2) (expt M 2) (expt r 2)))))
	 0
	 0
	 0)
	(up 0 0 0 0)
	(up 0 0 0 0))
       (down
	(up
	 (+
	  (/
	   (* -1/2 G M (expt c 2))
	   (+ (* 1/4 (expt c 4) (expt r 3))
	      (* -1 G M (expt c 2) (expt r 2))
	      (* (expt G 2) (expt M 2) r)))
	  (/
	   (* 1/2 (expt G 2) (expt M 2))
	   (+ (* 1/4 (expt c 4) (expt r 4))
	      (* -1 G M (expt c 2) (expt r 3))
	      (* (expt G 2) (expt M 2) (expt r 2)))))
	 0
	 0
	 0)
	(up
	 0
	 (+
	  (/
	   (* 1/2 G M (expt c 2))
	   (+ (* 1/4 (expt c 4) (expt r 3))
	      (* -1 G M (expt c 2) (expt r 2))
	      (* (expt G 2) (expt M 2) r)))
	  (/
	   (* -1/2 (expt G 2) (expt M 2))
	   (+ (* 1/4 (expt c 4) (expt r 4))
	      (* -1 G M (expt c 2) (expt r 3))
	      (* (expt G 2) (expt M 2) (expt r 2)))))
	 0
	 0)
	(up 0 0 (/ -1 (expt r 2)) 0)
	(up 0 0 0 (/ -1 (expt r 2))))
       (down (up 0 0 0 0) (up 0 0 (/ -1 (expt r 2)) 0) (up 0 -1 0 0) (up 0 0 0 0))
       (down (up 0 0 0 0)
	     (up 0 0 0 (/ -1 (expt r 2)))
	     (up 0 0 0 0)
	     (up 0 (* -1 (expt (sin theta) 2)) 0 0)))
      (down
       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
       (down (up 0 0 0 0)
	     (up 0 0 0 0)
	     (up 0 0 0 0)
	     (up 0 0 0 (/ -1 (expt (sin theta) 2))))
       (down
	(up 0 0 0 0)
	(up 0 0 0 0)
	(up 0 0 0 (/ -1 (expt (sin theta) 2)))
	(up
	 0
	 (+ (* -2 r (cos theta) (sin theta))
	    (/ (* 4 G M (cos theta) (sin theta)) (expt c 2)))
	 (+ 1 (* -2 (expt (cos theta) 2)))
	 0)))
      (down (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
	    (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
	    (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
	    (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0)))))
   spacetime-spherical))

(define (Riemann-tensor1 metric-tensor Gamma dGamma coordinate-system)
  (assert (tensor-field? metric-tensor)
	  "The metric-tensor must be a tensor field.")
  (assert (eq? (tensor-manifold metric-tensor)
	       (coordinate-system 'manifold))
	  "Metric and coordinate system must have the same manifold.")
  (let ((GG (contract (tensor-product Gamma Gamma) 0 3 coordinate-system)))
    (define (Riemann w x u v)
      (+ (- (dGamma w x v u) (dGamma w x u v))
	 (- (GG w x v u) (GG w x u v))))
    (make-tensor-field 1 3
		       Riemann
		       `(Riemann ,(diffop-name metric-tensor))
		       (tensor-manifold metric-tensor))))

(define R
  (Riemann-tensor1 S-metric Gamma1 Gamma-comma1 spacetime-spherical))

(show-time
 (lambda ()
   (pec ((tensor-field->coefficient-structure R spacetime-spherical)
	 s-event))))
;#| Result:
;(down
; (down
;  (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;  (down
;   (up
;    0
;    (+ (/ (* -2 G M) (expt r0 3))
;       (/ (* 4 (expt G 2) (expt M 2)) (* (expt c 2) (expt r0 4))))
;    0
;    0)
;   (up (/ (* G M) (+ (* -1/2 (expt c 2) (expt r0 3)) (* G M (expt r0 2)))) 0 0 0)
;   (up 0 0 0 0)
;   (up 0 0 0 0))
;  (down
;   (up
;    0
;    0
;    (+ (/ (* G M) (expt r0 3))
;       (/ (* -2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r0 4))))
;    0)
;   (up 0 0 0 0)
;   (up (/ (* G M) (* (expt c 2) r0)) 0 0 0)
;   (up 0 0 0 0))
;  (down
;   (up
;    0
;    0
;    0
;    (+ (/ (* G M) (expt r0 3))
;       (/ (* -2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r0 4)))))
;   (up 0 0 0 0)
;   (up 0 0 0 0)
;   (up (/ (* G M (expt (sin theta0) 2)) (* (expt c 2) r0)) 0 0 0)))
; (down
;  (down
;   (up
;    0
;    (+ (/ (* 2 G M) (expt r0 3))
;       (/ (* -4 (expt G 2) (expt M 2)) (* (expt c 2) (expt r0 4))))
;    0
;    0)
;   (up (/ (* -1 G M) (+ (* -1/2 (expt c 2) (expt r0 3)) (* G M (expt r0 2)))) 0 0 0)
;   (up 0 0 0 0)
;   (up 0 0 0 0))
;  (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;  (down
;   (up 0 0 0 0)
;   (up 0 0 (/ (* 1/2 G M) (+ (* -1/2 (expt c 2) (expt r0 3)) (* G M (expt r0 2)))) 0)
;   (up 0 (/ (* G M) (* (expt c 2) r0)) 0 0)
;   (up 0 0 0 0))
;  (down
;   (up 0 0 0 0)
;   (up 0 0 0 (/ (* 1/2 G M) (+ (* -1/2 (expt c 2) (expt r0 3)) (* G M (expt r0 2)))))
;   (up 0 0 0 0)
;   (up 0 (/ (* G M (expt (sin theta0) 2)) (* (expt c 2) r0)) 0 0)))
; (down
;  (down
;   (up
;    0
;    0
;    (+ (/ (* -1 G M) (expt r0 3))
;       (/ (* 2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r0 4))))
;    0)
;   (up 0 0 0 0)
;   (up (/ (* -1 G M) (* (expt c 2) r0)) 0 0 0)
;   (up 0 0 0 0))
;  (down
;   (up 0 0 0 0)
;   (up 0
;       0
;       (/ (* -1/2 G M) (+ (* -1/2 (expt c 2) (expt r0 3)) (* G M (expt r0 2))))
;       0)
;   (up 0 (/ (* -1 G M) (* (expt c 2) r0)) 0 0)
;   (up 0 0 0 0))
;  (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;  (down (up 0 0 0 0)
;        (up 0 0 0 0)
;        (up 0 0 0 (/ (* 2 G M) (* (expt c 2) r0)))
;        (up 0 0 (/ (* -2 G M (expt (sin theta0) 2)) (* (expt c 2) r0)) 0)))
; (down
;  (down
;   (up
;    0
;    0
;    0
;    (+ (/ (* -1 G M) (expt r0 3))
;       (/ (* 2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r0 4)))))
;   (up 0 0 0 0)
;   (up 0 0 0 0)
;   (up (/ (* -1 G M (expt (sin theta0) 2)) (* (expt c 2) r0)) 0 0 0))
;  (down
;   (up 0 0 0 0)
;   (up 0
;       0
;       0
;       (/ (* -1/2 G M) (+ (* -1/2 (expt c 2) (expt r0 3)) (* G M (expt r0 2)))))
;   (up 0 0 0 0)
;   (up 0 (/ (* -1 G M (expt (sin theta0) 2)) (* (expt c 2) r0)) 0 0))
;  (down (up 0 0 0 0)
;        (up 0 0 0 0)
;        (up 0 0 0 (/ (* -2 G M) (* (expt c 2) r0)))
;        (up 0 0 (/ (* 2 G M (expt (sin theta0) 2)) (* (expt c 2) r0)) 0))
;  (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))))
;|#
;; process time: 109950 (99430 RUN + 10520 GC); real time: 110346

(define R1
  (coefficient-structure->tensor-field
   (let ((G 'G)
	 (M 'M)
	 (c 'c))
     (down
      (down
       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
       (down
	(up
	 0
	 (+ (/ (* -2 G M) (expt r 3))
	    (/ (* 4 (expt G 2) (expt M 2)) (* (expt c 2) (expt r 4))))
	 0
	 0)
	(up (/ (* G M) (+ (* -1/2 (expt c 2) (expt r 3)) (* G M (expt r 2)))) 0 0 0)
	(up 0 0 0 0)
	(up 0 0 0 0))
       (down
	(up
	 0
	 0
	 (+ (/ (* G M) (expt r 3))
	    (/ (* -2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r 4))))
	 0)
	(up 0 0 0 0)
	(up (/ (* G M) (* (expt c 2) r)) 0 0 0)
	(up 0 0 0 0))
       (down
	(up
	 0
	 0
	 0
	 (+ (/ (* G M) (expt r 3))
	    (/ (* -2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r 4)))))
	(up 0 0 0 0)
	(up 0 0 0 0)
	(up (/ (* G M (expt (sin theta) 2)) (* (expt c 2) r)) 0 0 0)))
      (down
       (down
	(up
	 0
	 (+ (/ (* 2 G M) (expt r 3))
	    (/ (* -4 (expt G 2) (expt M 2)) (* (expt c 2) (expt r 4))))
	 0
	 0)
	(up (/ (* -1 G M) (+ (* -1/2 (expt c 2) (expt r 3)) (* G M (expt r 2)))) 0 0 0)
	(up 0 0 0 0)
	(up 0 0 0 0))
       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
       (down
	(up 0 0 0 0)
	(up 0 0 (/ (* 1/2 G M) (+ (* -1/2 (expt c 2) (expt r 3)) (* G M (expt r 2)))) 0)
	(up 0 (/ (* G M) (* (expt c 2) r)) 0 0)
	(up 0 0 0 0))
       (down
	(up 0 0 0 0)
	(up 0 0 0 (/ (* 1/2 G M) (+ (* -1/2 (expt c 2) (expt r 3)) (* G M (expt r 2)))))
	(up 0 0 0 0)
	(up 0 (/ (* G M (expt (sin theta) 2)) (* (expt c 2) r)) 0 0)))
      (down
       (down
	(up
	 0
	 0
	 (+ (/ (* -1 G M) (expt r 3))
	    (/ (* 2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r 4))))
	 0)
	(up 0 0 0 0)
	(up (/ (* -1 G M) (* (expt c 2) r)) 0 0 0)
	(up 0 0 0 0))
       (down
	(up 0 0 0 0)
	(up 0
	    0
	    (/ (* -1/2 G M) (+ (* -1/2 (expt c 2) (expt r 3)) (* G M (expt r 2))))
	    0)
	(up 0 (/ (* -1 G M) (* (expt c 2) r)) 0 0)
	(up 0 0 0 0))
       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
       (down (up 0 0 0 0)
	     (up 0 0 0 0)
	     (up 0 0 0 (/ (* 2 G M) (* (expt c 2) r)))
	     (up 0 0 (/ (* -2 G M (expt (sin theta) 2)) (* (expt c 2) r)) 0)))
      (down
       (down
	(up
	 0
	 0
	 0
	 (+ (/ (* -1 G M) (expt r 3))
	    (/ (* 2 (expt G 2) (expt M 2)) (* (expt c 2) (expt r 4)))))
	(up 0 0 0 0)
	(up 0 0 0 0)
	(up (/ (* -1 G M (expt (sin theta) 2)) (* (expt c 2) r)) 0 0 0))
       (down
	(up 0 0 0 0)
	(up 0
	    0
	    0
	    (/ (* -1/2 G M) (+ (* -1/2 (expt c 2) (expt r 3)) (* G M (expt r 2)))))
	(up 0 0 0 0)
	(up 0 (/ (* -1 G M (expt (sin theta) 2)) (* (expt c 2) r)) 0 0))
       (down (up 0 0 0 0)
	     (up 0 0 0 0)
	     (up 0 0 0 (/ (* -2 G M) (* (expt c 2) r)))
	     (up 0 0 (/ (* 2 G M (expt (sin theta) 2)) (* (expt c 2) r)) 0))
       (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0)))))
   spacetime-spherical))

(define (Ricci-tensor1 Riemann-tensor coordinate-system)
  ;; u, v vector fields, m is a point
  (let ((R^i_jkl Riemann-tensor))
    (let ((R_jl (contract R^i_jkl 0 1 coordinate-system)))
      R_jl)))

(define Ricci
  (Ricci-tensor1 R1 spacetime-spherical))

(show-time
 (lambda ()
   (pec ((tensor-field->coefficient-structure Ricci spacetime-spherical)
	 s-event))))
;#| Result:
;(down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
;|#
;; process time: 4490 (4020 RUN + 470 GC); real time: 4523
;;; The following statistics are pretty typical

(gc-flip)
;Value: 5346657

(head *interpolate-primes-stream*)
;Value: 113

canonicalizer-hits
;Value: 859367

canonicalizer-misses
;Value: 93185

(show-memoizer-statistics)
;#|
;(520145 1375 (??? value-expression))
;(1391509 2156 (??? coords))
;(1849352 2632 (??? point))
;(0 1 (??? coords))
;(0 0 (??? point))
;(0 0 Riemann-tensor)
;(0 0 t:covariant-derivative)
;(0 1 Christoffel)
;(0 1 invert-metric)
;(0 2 contract)
;(0 569 tensor-product)
;(188 73 coefficient-structure->tensor-field)
;(256 6 tensor-field->coefficient-structure)
;(0 2 t:comma)
;(292 7 ff->tf)
;(288 4 vf->tf)
;(521519 1 memoized-abstract-structure-function)
;(5044311 5337 memoized-simplify)
;(0 0 complex-rules)
;(0 0 exp-expand)
;(0 0 exp-contract)
;(0 0 exp->sincos)
;(0 0 sincos->exp2)
;(0 0 sincos->exp1)
;(800 1134 sincos-random)
;(1176 753 flush-obvious-ones)
;(1088 841 split-high-degree-sines)
;(803 1126 split-high-degree-cosines)
;(0 0 cos^2->sin^2)
;(803 1126 sin^2->cos^2)
;(0 0 contract-expt-trig)
;(0 0 contract-multiangle)
;(0 0 expand-multiangle)
;(1054 3985 angular-parity)
;(803 1126 sincos->trig)
;(6 5396 trig->sincos)
;(0 0 canonicalize-partials)
;(0 0 log-expand)
;(0 0 log-contract)
;(0 0 logexp->specfun)
;(0 0 specfun->logexp)
;(1333 3402 sqrt-contract)
;(5608 7148 sqrt-expand)
;(0 0 special-trig)
;(15267 7583 universal-reductions)
;(0 0 diff:cosh)
;(0 0 diff:sinh)
;(706 790 diff:atan2)
;(0 0 diff:atan1)
;(1256 240 diff:acos)
;(0 0 diff:asin)
;(0 1009 diff:cos)
;(523 1009 diff:sin)
;(0 0 diff:log)
;(0 0 diff:exp)
;(0 0 diff:expt)
;(483 49 diff:power)
;(1236 440 diff:sqrt)
;(0 0 diff:invert)
;(0 0 diff:negate)
;(550 1126 diff:/)
;(1379 9490 diff:*)
;(0 0 diff:-)
;(1091 2525 diff:+)
;|#
