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

;;;; Manipulation of units

;(declare (usual-integrations))

;;; The units of a quantity are represented as a combination of a
;;; (labeled) vector of exponents, one for each base unit in a system,
;;; and a scale factor.  The unit objects, such as meter or joule are
;;; such objects.  Multiplication, division, and exponentiation are
;;; extended to combine units from the same system.

;#|
;;;; in types.scm
;
;(define unit-type-tag '*unit*)
;
;(define (units? x)
;  (and (pair? x) (eq? (car x) unit-type-tag)))
;|#

(define (unitless? x)
  (equal? x unitless))

(define (make-unit system exponents scale-factor)
  (list unit-type-tag system exponents scale-factor))

(define (unit-system u) (cadr u))
(define (unit-exponents u) (caddr u))
(define (unit-scale u) (cadddr u))

(define (same-units? u1 u2)
  (assert (and (units? u1) (units? u2)))
  (or (eq? u1 u2)
      (and (eq? (unit-system u1) (unit-system u2))
	   (let ((v1 (unit-exponents u1)) (v2 (unit-exponents u2)))
	     (let ((n (vector-length v1)))
	       (let lp ((i 0))
		 (or (fix:= i n)
		     (and (n:= (vector-ref v1 i) (vector-ref v2 i))
			  (lp (fix:+ i 1)))))))
	   (n:= (unit-scale u1) (unit-scale u2)))))

(define (*units u1 u2)
  (cond ((unitless? u1) u2)
	((unitless? u2) u1)
	(else (assert (and (units? u1) (units? u2)))
	      (assert (and (eq? (unit-system u1) (unit-system u2))))
	      (let ((v1 (unit-exponents u1)) (v2 (unit-exponents u2)))
		(make-unit (unit-system u1)
			   (make-initialized-vector (vector-length v1)
						    (lambda (i)
						      (n:+ (vector-ref v1 i)
							   (vector-ref v2 i))))
			   (n:* (unit-scale u1) (unit-scale u2)))))))

(define (invert-units u)
  (assert (units? u))
  (let ((v (unit-exponents u)))
    (make-unit (unit-system u)
	       (make-initialized-vector (vector-length v)
					(lambda (i)
					  (n:* -1 (vector-ref v i))))
	       (n:/ 1 (unit-scale u)))))


(define (/units u1 u2)
  (assert (and (units? u1) (units? u2)))
  (cond ((unitless? u1)
	 (let ((v2 (unit-exponents u2)))
	   (make-unit (unit-system u2)
		      (make-initialized-vector (vector-length v2)
					       (lambda (i)
						 (n:* -1 (vector-ref v2 i))))
		      (n:/ 1 (unit-scale u2)))))
	((unitless? u2) u1)
	(else (assert (and (eq? (unit-system u1) (unit-system u2))))
	      (let ((v1 (unit-exponents u1)) (v2 (unit-exponents u2)))
		(make-unit (unit-system u1)
			   (make-initialized-vector (vector-length v1)
						    (lambda (i)
						      (n:- (vector-ref v1 i)
							   (vector-ref v2 i))))
			   (n:/ (unit-scale u1) (unit-scale u2)))))))

(define (expt-units u p)
  (cond ((unitless? u) u)
	(else (assert (units? u) "Not a unit -- EXPT")
	      (let ((v (unit-exponents u)))
		(make-unit (unit-system u)
			   (make-initialized-vector (vector-length v)
						    (lambda (i)
						      (n:* p (vector-ref v i))))
			   (n:expt (unit-scale u) p))))))


(assign-operation '* *units units? units?)
(assign-operation 'invert invert-units units?)
(assign-operation '/ /units units? units?)
(assign-operation 'expt expt-units units? number?)


