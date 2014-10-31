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

;;; For symbolic expressions in operators.

(define (diffop-name form)
  (cond ((operator? form) (operator-name form))
	((literal-function? form) (f:expression form))
	(else (expression form))))


;;; The following mappers only make sense if, when there is more than
;;; one structure they are all isomorphic.

(define (s:sigma/r proc . structures)
  (s:sigma/r/l proc structures))

(define (s:sigma/r/l proc structures)
  (s:sigma/l (lambda elements
	     (if (structure? (car elements))
		 (s:sigma/r/l proc elements)
		 (apply proc elements)))
	   structures))

(define (s:sigma proc . structures)
  (s:sigma/l proc structures))

(define (s:sigma/l proc structures)
  (sigma (lambda (i)
	   (apply proc
		  (map (lambda (s) (s:ref s i))
		       structures)))
	 0
	 (- (s:length (car structures)) 1)))

;#|
;(define R2 (rectangular 2))
;(instantiate-coordinates R2 '(x y))
;(define chi-R2 (R2 '->coords))
;(define chi-inverse-R2 (R2 '->point))
;(define R2-basis (coordinate-system->basis R2))
;
;(pec (s:sigma/r (lambda (e) 
;		  ((e (compose (literal-function 'f (-> (UP Real Real) Real))
;			       chi-R2))
;		   (chi-inverse-R2 (up 'x0 'y0))))
;		(basis->vector-basis R2-basis)))
;#| Result:
;(+ (((partial 1) f) (up x0 y0)) (((partial 0) f) (up x0 y0)))
;|#
;|#



