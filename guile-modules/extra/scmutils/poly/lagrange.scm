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

;;;;                       LAGRANGE.SCM
;;; produces an interpolation polynomial expression for a given
;;; set of values at a given set of points.
;;;             (lagrange ys xs) |--> (lambda (t) ...)
;;;    For example, if we do:
;;; (define xs '(.1 .2 .3 .4 .5 .6))
;;; (define bar (lagrange (map sin xs) xs))
;;; (define foo (lambda->numerical-procedure bar))
;;; Then BAR is a lambda-expression, and FOO is the procedure that 
;;; evaluates the polynomial interpolating SIN at the given points.
;;; 

(declare (usual-integrations + - * /))

;;; Needs: ENCLOSE/COMCON (for LAMBDAFY, LETIFY utilities)
;;;        ENCLOSE/ENCLOSE for LAMBDA->NUMERICAL-PROCEDURE
;;;                               ENCLOSE/MAGIC for FLONUMIZE, etc. 


(define (lagrange ys xs)
  (lambdafy 1
	    (lambda (var)
	      (letify (map (lambda (x) (- var x)) xs)
		      (lambda (diffs)
			(triangle-iterate xs ys
					  (make-linear-interpolator
					   (table-of eqv? xs diffs))))))))


;#|
;(define (lagrange ys xs)
;  (lambda (var)
;    (triangle-iterate xs ys
;		      (make-linear-interpolator
;		       (table-of eqv? xs (map (lambda (x) (- var x)) xs))))))
;|#

(define (triangle-iterate xs v f)	;(f x0 x1 v0 v1)
  (define (all-except-ends l)
    (reverse (cdr (reverse (cdr l)))))
  (define map-consec-pairs
    (lambda (x0s x1s vs)
      (if (null? x1s)
	  '()
	  (cons (f (car x0s) (car x1s) (car vs) (cadr vs))
		(map-consec-pairs (cdr x0s) (cdr x1s) (cdr vs))))))
  (let level ((x1s (cdr xs)) (vs v))
    (if (null? (cdr vs))
	(car vs)
	(let ((nvs (map-consec-pairs xs x1s vs)))
	  (if (null? (cdr nvs))
	      (car nvs)
	      (letify (all-except-ends nvs)
		      (lambda (names)
			(level (cdr x1s)
			       (append (list (car nvs))
				       names
				       (last-pair nvs))))))))))

(define (make-linear-interpolator lookup)
  (lambda (x0 x1 v0 v1)
    (vector->vector-constructor
     (/ (- (* v1 (lookup x0))
	   (* v0 (lookup x1)))
	(- x1 x0)))))

(define (vector->vector-constructor exp)
  (if (vector? exp)
      (cons 'vector
	    (map vector->vector-constructor
		 (vector->list exp)))
      exp))
	     

(define (lagrange-interpolation-function ys xs)
  (lambda->interpreted-generic-procedure
   (lagrange ys xs)))

;#|
;(define (lagrange-interpolation-function ys xs)
;  (lagrange (vector->list ys)
;	    (vector->list xs)))      
;|#










