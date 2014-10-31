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

;;;; Split up a polynomial into factors of various multiplicities.
;;;    Nicely worked out by Mira Wilczek, June 2002.

;(declare (usual-integrations))

;;; The result is a list of factors.  The first element of the list 
;;; the constant factor, and each successive factor is to be raised 
;;; to the (zero-based) index of that element of the list.

(define (split-polynomial p)
  (define (answer tracker const)
    (if (number? (car (last-pair tracker)))
	(cons (car (last-pair tracker))
	      (append (cdr (except-last-pair tracker)) (list 1)))
	(cons const (cdr tracker))))
  (let lp ((m poly:zero) (h p) (Q poly:one)
	   (tracker '()) (old-s p) (old-m 1) )
    (if (poly:one? m)
	(answer tracker h)
	(let* ((dvis
		(map (lambda (i)
		       (poly:partial-derivative h (list i)))
		     (iota (poly:arity h))))
	       (gG (reduce poly:gcd poly:zero dvis))
	       (gg  (if (poly:zero? gG) poly:one gG))
	       (new-s (poly:quotient h (poly:gcd h gg)      ))
	       (new-m (poly:gcd gg new-s))
	       (h (poly:quotient h (poly:* new-m new-s))) 

	       ;; now h has all factpors to the 1 or 2 power
	       ;; completely removed, others now to the power-2.

	       (facts (poly:quotient old-s new-s))

	       ;; facts gets all the ones that were completely
	       ;; removed last step, i.e. all those that were to
	       ;; the 1 or 2 power.  the first loop through will
	       ;; get a totally wrong facts, but its gcd with the
	       ;; initial old-m=1 will be 1, so it won't result in
	       ;; incorrect doublefacts or singlefacts.

	       (doublefacts (poly:gcd facts old-m))

	       ;; doublefacts gets all the ones which were to the power 
	       ;; x>1, x<=2, (ergo x=2), in the last step.

	       (singlefacts (poly:quotient new-s new-m))

	       ;; takes out p = all factors only to the 1st power.

	       ;; want to check here whether singlefacts is a
	       ;; constant, if so put in "1" instead and (* h
	       ;; singlefacts)

	       ;; changed mind, am going to kluge in answer procedure,

	       (new-tracker
		(append tracker (list doublefacts singlefacts)))

	       ;; tracker of the form
	       ;;   h(vi) = (* (exponent (list-ref tracker k) k))

	       (old-s new-s)
	       (old-m new-m)
	       (m new-m)
	       (tracker new-tracker)
	       )
	  (lp m h Q tracker old-s old-m)))))

;;; Reconstruction

(define (split-polynomial->expression P)
  (let ((factors (factor-polynomial-expression P)))
    (cons '*
	  (filter (lambda (f)
		    (or (not (number? f))
			(not (= f 1))))
		  (cons (car factors)
			(map symb:expt
			     (cdr factors)
			     (iota (length factors) 1)))))))


(define (factor-polynomial-expression P)
  (poly:expression-> (expression P)
		     (lambda (p v)
		       (map (lambda (factor)
			      (default-simplify (poly:->expression factor v)))
			    (split-polynomial p)))))

;#| ;;; Simple test cases.
;(split-polynomial->expression
; (* (square (- 'x 'y)) (cube (+ 'x 'y))))
;;Value: (* (expt (+ x (* -1 y)) 2) (expt (+ x y) 3))
;
;(factor-polynomial-expression (* (square (- 'x 'y)) (cube (+ 'x 'y))))
;;Value: (1 1 (+ x (* -1 y)) (+ x y))
;
;(factor-polynomial-expression (square (- 'x 'y)))
;;Value: (1 1 (+ x (* -1 y)) 1)
;
;(factor-polynomial-expression (* 3 (cube 'z) (+ (square 'x) 'y)))
;;Value: (3 (+ (expt x 2) y) 1 z)
;
;(factor-polynomial-expression (* 3 (square 'z) (+ (square 'x) 'y)))
;;Value: (3 (+ (expt x 2) y) z 1)
;|#

;;; Recursive generalization

(define (poly:->factors p v)
  (let ((factors (map (lambda (factor)
			(poly:->expression factor v))
		      (split-polynomial p))))
    (let ((ff
	   (filter (lambda (f)
		     (or (not (number? f))
			 (not (= f 1))))
		   (cons (car factors)
			 (map symb:expt
			      (cdr factors)
			      (iota (length factors) 1))))))
      (cond ((null? ff) 1)
	    ((null? (cdr ff)) (car ff))
	    (else (cons '* ff))))))


(define poly:factor-analyzer
  (make-analyzer poly:->factors
		 poly:expression->
		 poly:operators-known))

(define poly:factor (default-simplifier poly:factor-analyzer))


;#|
;(define test-poly
;  (let ((x 'x) (y 'y))
;    (let ((z (square (+ x (* x (expt y 2))))))
;      (simplify
;       (expression
;	(* (expt (+ (cos z) y) 2)
;	   (expt (- (cos z) y) 3)))))))
;
;(pp test-poly)
;(+ (* -1 (expt y 5))
;   (* (expt y 4)
;      (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))))
;   (* 2
;      (expt y 3)
;      (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 2))
;   (* -2
;      (expt y 2)
;      (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 3))
;   (* -1
;      y
;      (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 4))
;   (expt (cos (+ (* (expt x 2) (expt y 4)) (* 2 (expt x 2) (expt y 2)) (expt x 2))) 5))
;
;
;(pp (poly:factor test-poly))
;(* -1
;   (expt (+ y (cos (expt (* (+ 1 (expt y 2)) x) 2))) 2)
;   (expt (+ y (* -1 (cos (expt (* (+ 1 (expt y 2)) x) 2)))) 3))
;|#

;;; Take perfect squares out of square roots.

(define (root-out-squares expression)
  (define (walk expr)
    (if (pair? expr)
	(if (eq? (operator expr) 'sqrt)
	    (process-sqrt expr)
	    (cons (walk (car expr))
		  (walk (cdr expr))))
	expr))
  (define (process-sqrt expr)
    (let ((fact-exp (poly:factor (car (operands expr)))))
      (if (product? fact-exp)
	  (let lp ((factors (operands fact-exp)) (odds 1) (evens 1))
	    (cond ((null? factors)
		   (symb:* (symb:sqrt odds) evens))
		  ((expt? (car factors))
		   (let ((b (car (operands (car factors)))) (e (cadr (operands (car factors)))))
		     (lp (cdr factors)
			 (if (odd? e) (symb:* odds b) odds)
			 (let ((power (quotient e 2)))
			   (cond ((fix:> power 1) (symb:* evens (symb:expt b power)))
				 ((fix:= power 1) (symb:* evens b))
				 (else evens))))))
		  (else
		   (lp (cdr factors) (symb:* (car factors) odds) evens))))
	  (symb:sqrt fact-exp))))
  (walk expression))

;#|
;(pe (root-out-squares
;     (sqrt (* (square (+ 'x 'y)) (cube (- 'x 'y))))))
;(+ (* (expt x 2) (sqrt (+ x (* -1 y)))) (* -1 (expt y 2) (sqrt (+ x (* -1 y)))))
;
;
;(pe
; (root-out-squares
;  (simplify
;   '(/ (+ (* -1
;	     (expt R 2)
;	     (((partial 0) f)
;	      (up (* R (cos phi) (sin theta))
;		  (* R (sin phi) (sin theta))
;		  (* R (cos theta))))
;	     (cos phi)
;	     (expt (cos theta) 3)
;	     (sin theta))
;	  (* -1
;	     (expt R 2)
;	     (((partial 1) f)
;	      (up (* R (cos phi) (sin theta))
;		  (* R (sin phi) (sin theta))
;		  (* R (cos theta))))
;	     (expt (cos theta) 3)
;	     (sin phi)
;	     (sin theta))
;	  (* (((partial 2) f)
;	      (up (* R (cos phi) (sin theta))
;		  (* R (sin phi) (sin theta))
;		  (* R (cos theta))))
;	     (sqrt
;	      (+ (* (expt R 4) (expt (cos theta) 4))
;		 (* -2 (expt R 4) (expt (cos theta) 2))
;		 (expt R 4)))
;	     (expt (cos theta) 2)))
;       (* R (sin theta))))))
;(+ (* -1
;      R
;      (((partial 0) f)
;       (up (* R (cos phi) (sin theta))
;	   (* R (sin phi) (sin theta))
;	   (* R (cos theta))))
;      (cos phi)
;      (expt (cos theta) 3))
;   (* -1
;      R
;      (((partial 1) f)
;       (up (* R (cos phi) (sin theta))
;	   (* R (sin phi) (sin theta))
;	   (* R (cos theta))))
;      (sin phi)
;      (expt (cos theta) 3))
;   (* -1
;      R
;      (((partial 2) f)
;       (up (* R (cos phi) (sin theta))
;	   (* R (sin phi) (sin theta))
;	   (* R (cos theta))))
;      (expt (cos theta) 2)
;      (sin theta)))
;
;;;; Win! Yields the hand-simplified result!
;|#
