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

(define ((Gamma-bar f) local)
  ((f (osculating-path local)) (time local)))

;;; An alternative method allows taking derivatives in the
;;; construction of the Lagrangian.

(define ((osculating-path state0) t)
  (let ((t0 (time state0))
	(q0 (coordinate state0))
	(k (vector-length state0)))
    (let ((dt (- t t0)))
      (let loop ((n 2) (sum q0) (dt^n/n! dt))
	(if (fix:= n k)
	    sum
	    (loop (+ n 1)
		  (+ sum (* (vector-ref state0 n) dt^n/n!))
		  (/ (* dt^n/n! dt) n)))))))

;;; "total time derivative"

;#|
;(define (Dt-procedure F)
;  (define (DF-on-path q)
;    (D (compose F (Gamma q))))
;  (Gamma-bar DF-on-path))
;|#

(define ((Dt-procedure F) state)
  (let ((n (vector-length state)))
    (define (DF-on-path q)
      (D (compose F (Gamma q n))))
    ((Gamma-bar DF-on-path) state)))

(define Dt
  (make-operator Dt-procedure 'Dt))

;#|
;(print-expression
; ((Dt
;   (lambda (state)
;     (let ((t (time state))
;	   (q (coordinate state)))
;       (square q))))
;  (->local
;   't
;   (coordinate-tuple 'x 'y) 
;   (velocity-tuple 'vx 'vy))))
;(+ (* 2 vx x) (* 2 vy y))
;
;
;(print-expression
; ((Dt (Dt (lambda (state) (coordinate state))))
;  (->local 't 'x 'v 'a 'j)))
;a
;
;(print-expression
; ((Dt (Dt (lambda (state)
;	    (square (coordinate state)))))
;  (->local 't 'x 'v 'a 'j)))
;(+ (* 2 a x) (* 2 (expt v 2)))
;|#

(define (Euler-Lagrange-operator Lagrangian)
  (- (Dt ((partial 2) Lagrangian))
     (compose ((partial 1) Lagrangian) trim-last-argument)))

(define (trim-last-argument local)
  (apply up (except-last-pair (vector->list (up->vector local)))))

(define LE Euler-Lagrange-operator)
(define Lagrange-equations-operator LE)

;#|
;(print-expression
; ((LE (L-harmonic 'm 'k))
;  (->local 't 'x 'v 'a)))
;(+ (* a m) (* k x))
;
;(print-expression
; ((LE (L-harmonic 'm 'k))
;  (->local 't
;	   #(x y)
;	   #(vx vy)
;	   #(ax ay))))
;(down (+ (* ax m) (* k x))
;      (+ (* ay m) (* k y)))
;
;
;;;; Adding extra state components is harmless.
;
;(print-expression
; ((LE (L-harmonic 'm 'k))
;  (->local 't 'x 'v 'a 'j)))
;(+ (* a m) (* k x))
;
;;;; But watch out.  If not enuf local componenents
;;;;  are specified we lose.
;
;(print-expression
; ((LE (L-harmonic 'm 'k))
;  (->local 't 'x 'v)))
;;Cannot extract velocity from #((*diff* ... ...) x)
;;;; error
;
;(print-expression
; ((LE (L-central-polar 'm (literal-function 'V)))
;  (up 't
;      (up 'r 'phi)
;      (up 'rdot 'phidot)
;      (up 'rdotdot 'phidotdot))))
;(down (+ (* -1 m (expt phidot 2) r) (* m rdotdot) ((D V) r))
;      (+ (* 2 m phidot r rdot) (* m phidotdot (expt r 2))))
;
;(print-expression
; ((compose (LE (L-central-polar 'm (literal-function 'V)))
;	   (Gamma
;	    (coordinate-tuple (literal-function 'r)
;			      (literal-function 'phi))
;	    4))
;  't))
;(down
; (+ (* -1 m (expt ((D phi) t) 2) (r t))
;    (* m (((expt D 2) r) t))
;    ((D V) (r t)))
; (+ (* 2 m ((D r) t) ((D phi) t) (r t))
;    (* m (((expt D 2) phi) t) (expt (r t) 2))))
;|#

;#|
;;;; This stuff is undebugged.
;
;(define ((generalized-LE Lagrangian) state)
;  (let lp ((i 1))
;    (if (= i (s:length state))
;	Lagrangian
;	(- (Dt (lp (+ i 1)))
;	   (compose ((partial i) Lagrangian)
;		    trim-last-argument)))))
;
;
;(define ((generalized-LE Lagrangian) state)
;  (let ((m (s:length state)))
;    (assert (and (fix:> m 3) (even? m))
;	    "Incorrect state size for Lagrange Equations")
;    (let lp ((i (quotient m 2)))
;      (if (fix:= i 0)
;	  0
;	  (- ((compose ((expt Dt (fix:- i 1))
;			((partial i) Lagrangian))
;		       (iterated trim-last-argument
;				 (fix:- (quotient m 2) i)))
;	      state)
;	     (lp (fix:- i 1)))))))
;
;(define ((L2harmonic m k) state)
;  (let ((x (coordinate state))
;	(a (acceleration state)))
;    (+ (* 1/2 m x a) (* 1/2 k (square x)))))
;
;(print-expression
; ((generalized-LE (L2harmonic 'm 'k))
;  (->local 't 'x 'v 'a 'j 'p)))
;(+ (* -1 a m) (* -1 k x))
;
;(define L (literal-function 'L (Lagrangian)))
;
;(pe ((generalized-LE L) (->local 't 'x 'v 'a)))
;(+ (* a (((partial 2) ((partial 2) L)) (up t x v a)))
;   (* v (((partial 1) ((partial 2) L)) (up t x v a)))
;   (((partial 0) ((partial 2) L)) (up t x v a))
;   (* -1 (((partial 1) L) (up t x v))))
;
;(pe ((generalized-LE L) (->local 't 'x 'v 'a 'j 'p)))
;(+ (* (expt a 2) (((partial 2) ((partial 2) ((partial 3) L))) (up t x v a j p)))
;   (* 2 a j (((partial 2) ((partial 3) ((partial 3) L))) (up t x v a j p)))
;   (* 2 a p (((partial 2) ((partial 3) ((partial 4) L))) (up t x v a j p)))
;   (* 2 a v (((partial 1) ((partial 2) ((partial 3) L))) (up t x v a j p)))
;   (* (expt j 2) (((partial 3) ((partial 3) ((partial 3) L))) (up t x v a j p)))
;   (* 2 j p (((partial 3) ((partial 3) ((partial 4) L))) (up t x v a j p)))
;   (* 2 j v (((partial 1) ((partial 3) ((partial 3) L))) (up t x v a j p)))
;   (* (expt p 2) (((partial 3) ((partial 4) ((partial 4) L))) (up t x v a j p)))
;   (* 2 p v (((partial 1) ((partial 3) ((partial 4) L))) (up t x v a j p)))
;   (* (expt v 2) (((partial 1) ((partial 1) ((partial 3) L))) (up t x v a j p)))
;   (* 2 a (((partial 0) ((partial 2) ((partial 3) L))) (up t x v a j p)))
;   (* a (((partial 1) ((partial 3) L)) (up t x v a j p)))
;   (* -1 a (((partial 2) ((partial 2) L)) (up t x v a j)))
;   (* 2 j (((partial 0) ((partial 3) ((partial 3) L))) (up t x v a j p)))
;   (* j (((partial 2) ((partial 3) L)) (up t x v a j p)))
;   (* -1 j (((partial 2) ((partial 3) L)) (up t x v a j)))
;   (* 2 p (((partial 0) ((partial 3) ((partial 4) L))) (up t x v a j p)))
;   (* p (((partial 3) ((partial 3) L)) (up t x v a j p)))
;   (* 2 v (((partial 0) ((partial 1) ((partial 3) L))) (up t x v a j p)))
;   (* -1 v (((partial 1) ((partial 2) L)) (up t x v a j)))
;   (((partial 1) L) (up t x v a))
;   (((partial 0) ((partial 0) ((partial 3) L))) (up t x v a j p))
;   (* -1 (((partial 0) ((partial 2) L)) (up t x v a j))))
;|#
