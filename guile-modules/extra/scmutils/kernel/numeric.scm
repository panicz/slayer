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

;;;; Extensions to Scheme numbers

;(declare (usual-integrations))

;;; Everybody wants to know about these.

(define zero 0)
(define one 1)
(define -one -1)
(define two 2)
(define three 3)

(define pi (* 4 (atan 1 1)))
(define -pi (- pi))
(define pi/6 (/ pi 6))
(define -pi/6 (- pi/6))
(define pi/4 (/ pi 4))
(define -pi/4 (- pi/4))
(define pi/3 (/ pi 3))
(define -pi/3 (- pi/3))
(define pi/2 (/ pi 2))
(define -pi/2 (- pi/2))
(define 2pi (+ pi pi))
(define -2pi (- 2pi))

(define :zero zero)
(define :one one)
(define :-one -one)
(define :two two)
(define :three three)

(define :pi pi)
(define :+pi pi)
(define :-pi -pi)
(define :pi/6 pi/6)
(define :+pi/6 pi/6)
(define :-pi/6 -pi/6)
(define :pi/4 pi/4)
(define :+pi/4 pi/4)
(define :-pi/4 -pi/4)
(define :pi/3 pi/3)
(define :+pi/3 pi/3)
(define :-pi/3 -pi/3)
(define :pi/2 pi/2)
(define :+pi/2 pi/2)
(define :-pi/2 -pi/2)
(define :2pi 2pi)
(define :+2pi 2pi)
(define :-2pi -2pi)

(define *machine-epsilon*
  (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
         (* 2 e)
         (loop (/ e 2)))))

(define *sqrt-machine-epsilon* 
  (sqrt *machine-epsilon*))

(define :euler 0.57721566490153286)

(define :phi (/ (+ 1 (sqrt 5)) 2))

(define (exact-zero? x)
  (and (number? x) (exact? x) (= x 0)))

(define (exact-one? x)
  (and (number? x) (exact? x) (= x 1)))

(define :ln2 (log 2.0))
(define :ln10 (log 10.0))

(define (log10 x)
  (/ (log x) :ln10))

(define (log2 x)
  (/ (log x) :ln2))

(define (exp10 x)
  (expt 10 x))

(define (exp2 x)
  (expt 2 x))

(define :minlog -1000.0)

(define (safelog x)
  (if (and (real? x) (> x 0))
      (max (log x) :minlog)
      (error "Out of range -- SAFELOG" x)))

(define (principal-value cuthigh)
  (let ((cutlow (- cuthigh :+2pi)))
    (define (the-principal-value x)
      (if (and (<= cutlow x) (< x cuthigh))
	  x
	  (let ((y (- x (* :+2pi (floor (/ x :+2pi))))))
	    (if (< y cuthigh) 
		y
		(- y :+2pi)))))
    the-principal-value))


(define (principal-value-minus-pi-to-pi x)
  (if (or (<= x :-pi) (> x :+pi))
      (let ((y (- x (* :+2pi (floor (/ x :2pi))))))
	(if (< y :+pi) 
	    y
	    (- y :+2pi)))
      x))


(define (principal-value-zero-to-2pi x)
  (if (or (< x 0.0) (>= x :+2pi))
      (- x (* :+2pi (floor (/ x :+2pi))))
      x))

(define ((principal-range period) index)
  (let ((t (- index (* period (floor (/ index period))))))
    (if (< t (/ period 2.))
        t
        (- t period))))


(define (one? x) (= x 1))		; Exactness?

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (negate x) (- x))
(define (invert x) (/ x))

(define (sec x)
  (/ 1.0 (cos x)))

(define (csc x)
  (/ 1.0 (sin x)))

(define (sinh x)
  (/ (- (exp x) (exp (- x))) 2.0))

(define (cosh x)
  (/ (+ (exp x) (exp (- x))) 2.0))

(define (tanh x)
  (/ (sinh x) (cosh x)))

(define (sech x)
  (/ 1.0 (cosh x)))

(define (csch x)
  (/ 1.0 (sinh x)))

(define (factorial n)
  (define (f n)
    (if (= n 0)
	1
	(* n (f (- n 1)))))
  (assert (and (exact-integer? n) (not (negative? n))))
  (f n))


(define (exact-quotient n d)
  (let ((qr (integer-divide n d)))
    (assert (= 0 (integer-divide-remainder qr)))
    (integer-divide-quotient qr)))


(define (binomial-coefficient n m)
  (assert (and (exact-integer? n) (exact-integer? m) (<= 0 m n)))
  (let ((d (- n m)))
    (let ((t (max m d)) (s (min m d)))
      (define (lp count prod)
	(if (= count t)
	    (exact-quotient prod (factorial s))
	    (lp (- count 1) (* count prod))))
      (lp n 1))))


(define (sigma f low high)
  (let lp ((i low) (sum 0))
    (if (fix:> i high)
	sum
	(lp (fix:+ i 1) (+ sum (f i))))))

(define (close-enuf? h1 h2 tolerance)
  (<= (magnitude (- h1 h2))
      (* .5 (max tolerance *machine-epsilon*)
	 (+ (magnitude h1) (magnitude h2) 2.0))))

;;; The following is arbitrary, but chosen to make Euclid's algorithm 
;;; for polynomials over the rationals (defined with pseudo-division)
;;; have only small fractions.


;(define make-rational
;  (access make-rational (->environment '(runtime number))))
(define make-rational /)		;; for guile
    
(define (gcd-for-rationals q1 q2)
  (let ((n (gcd (numerator q1) (numerator q2)))
	(d (gcd (denominator q1) (denominator q2))))
    (make-rational n d)))

(define (scheme-number-gcd x y)
  (cond ((or (inexact? x) (inexact? y)) 1)
	((and (exact-integer? x) (exact-integer? y))
	 (gcd x y))
	((and (exact-rational? x) (exact-rational? y))
	 (gcd-for-rationals x y))
	(else 1)))


(define *no-rationals-in-divide* #f)

(define (scheme-number-divide n d c)
  (if (and *no-rationals-in-divide*
	   (exact-integer? n)
	   (exact-integer? d))
      (let ((qr (integer-divide n d)))
	(c (integer-divide-quotient qr)
	   (integer-divide-remainder qr)))
      (c (/ n d) 0)))



;;; From Hamming, gives roots of quadratic without bad roundoff.

(define (quadratic a b c cont)
  (let ((q (* -1/2
	      (+ b (* (sgn b)
		      (sqrt (- (* b b) (* 4 a c))))))))
    (cont (/ q a) (/ c q))))

(define (sgn x)
  (if (negative? x) -1 1))
