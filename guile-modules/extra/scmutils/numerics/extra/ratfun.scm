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

;;;; Rational Functions interpolated from procedures.

;;; This file contains:
;;;
;;; (procedure->ratfun <procedure> <degree>) -> <rcf>

;;; The following procedure, takes a procedure that computes a function 
;;; of a complex variable of known degree, and returns a rational function.
;;;
;;;                  a2*s^2 + a1*s + a0
;;;              --------------------------
;;;              b3s^3 + b2*s^2 + b1*s + b0
;;;
;;; The degree supplied, n, corresponds to the max degree of the
;;; denominator or (which is the same thing) to 1 + the max degree of
;;; the numerator.  If n is too small, the procedure will return the
;;; rational function that agrees with the values of the given
;;; procedure at 2n test points generated by TEST-POINTS.  If n is too
;;; large, an error is signalled, indicating that the equations are
;;; not independent.  The answer is normalized so that the highest
;;; non-zero power in the denominator has coefficient 1. 

(define (procedure->ratfun procedure n)
  (define (list->srcf lst)
    (list '*rcf*
	  (list->spcf (car lst))
	  (list->spcf (cadr lst))))
  (define (list->spcf lst)
    (cons '*spcf*
	  (cons 1
		(let lp ((lst lst) (order 0))
		  (cond ((null? lst) '())
			((= (car lst) 0)
			 (lp (cdr lst) (int:+ order 1)))
			(else
			 (cons (cons order (car lst))
			       (lp (cdr lst) (int:+ order 1)))))))))
  (if (fix:< n 1) (error "Bad maximum degree -- PROCEDURE->RAT-FUNC" n))
  (let ((coeffs (rat-fun-vector procedure n)))
    (let ((a-part
	   (generate-list n
			  (lambda (i)
			    (vector-ref coeffs i))))
	  (b-part
	   (generate-list n
			  (lambda (i)
			    (vector-ref coeffs (+ i n))))))
      (list->srcf (list (reverse (truncate-leading-zeros (reverse a-part)))
			(append b-part '(1))))))) ;b[n]=1


;;; Below this point we implement the PROCEDURE->RAT-FUNC procedure.

(define (truncate-leading-zeros l)
  (cond ((null? l) (list 0))
	((= (car l) 0) (truncate-leading-zeros (cdr l)))
	(else l)))


;;; We generate the 2n by 2n matrix M of coefficients of the a[i]
;;; and b[i] obtained by evaluating f at 2n points and setting
;;; this equal to the desired rational form.  

(define (rat-fun-vector f n)
  (let ((e (assemble-matrix-equation f n)))
    (let ((A (car e)) (B (cadr e)))	;AX=B
      (lu-decompose A
		    (lambda (lumatrix luperm lusign)
		      (let ((rank (lu-rank lumatrix)))
			(if (fix:= rank (fix:* 2 n)) ;ok!
			    (lu-backsubstitute lumatrix luperm B)
			    (error "N is too large -- PROCEDURE->RAT-FUNC" n))))
		    allow-zero-pivot))))

;;; This assembles the matrix of coefficients and the drive vector.
;;;  The unknowns are in the order a[0], ... a[n-1], b[0], ... b[n-1].
;;; The rows are produced by evaluating f at 2n+1 test points.

(define (assemble-matrix-equation f n)
  (let ((equations (map (lambda (i) (eqn i f n)) (test-points (fix:* n 2)))))
    (list (matrix-by-rows (map cadr equations))
	  (list->vector (map car equations)))))


;;; Test points are generated along a line parallel to the imaginary axis
;;; and spaced out to fit within a range near the unit circle.

(define (test-points num)
  (map (lambda (imag) (make-rectangular test-point-real imag))
       (let ((space (/ test-point-imag (- num 1))))
	 (let loop ((n num))
	   (cond ((= n 0) '())
		 ((odd? n)
		  (cons (* n space) (loop (- n 1))))
		 (else
		  (cons (* -1 n space) (loop (- n 1)))))))))

;;; The following magic numbers are arbitrary

(define test-point-real (/ 1 10))
(define test-point-imag 1.0)

;;; Returns a representation of an equation:
;;;  x^n*f(x) = 
;;;     a[0] + ... + x^(n-1)a[n-1] - f(x)*b[0] - ... - x^(n-1)*f(x)*b[n-1]
;;; as
;;; ( x^n*f(x)  ( 1 ... x^(n-1) -f(x) ... - x^(n-1)*f(x) ) )

(define (eqn x f n)
  ((with-reasonable-object x
     (lambda (x)
       (values x (f x))))
   (lambda (x fx)
     (let lp ((i 0) (xp 1) (acoeffs '()) (bcoeffs '()))
       (if (fix:= i n)
	   (list (* fx xp)		       ;rhs
		 (append (reverse acoeffs)     ;lhs
			 (reverse bcoeffs)))
	   (lp (fix:1+ i)
	       (* xp x)
	       (cons xp acoeffs)
	       (cons (* -1 xp fx) bcoeffs)))))))

(define ((values . list) receiver)
  (apply receiver list))


;;; To be replaced in 6.003 system *****************

(define (with-reasonable-object obj proc) (proc obj))


