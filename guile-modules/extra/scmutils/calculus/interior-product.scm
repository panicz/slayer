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

(define (interior-product X)
  (define (ix alpha)
    (assert (form-field? alpha) "alpha not a form field: interior-product")
    (let ((p (get-rank alpha)))
      (define (the-product . args)
	(assert (= (length args) (- p 1))
		"Wrong number of arguments to interior product")
	(apply alpha (cons X args)))
      (assert (> p 0) "Rank of form not greater than zero: interior-product")
      (procedure->nform-field the-product
			      `((interior-product ,(diffop-name X))
				,(diffop-name alpha))
			      (- p 1))))
  (assert (vector-field? X) "X not a vector field: interior-product")  

  ix)

;#|
;;;; Claim L_x omega = i_x d omega + d i_x omega (Cartan Homotopy Formula)
;
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;
;(define X (literal-vector-field 'X R3))
;(define Y (literal-vector-field 'Y R3))
;(define Z (literal-vector-field 'Z R3))
;(define W (literal-vector-field 'W R3))
;
;(define alpha
;  (compose (literal-function 'alpha (-> (UP Real Real Real) Real))
;	   (R3 '->coords)))
;(define beta
;  (compose (literal-function 'beta (-> (UP Real Real Real) Real))
;	   (R3 '->coords)))
;(define gamma
;  (compose (literal-function 'gamma (-> (UP Real Real Real) Real))
;	   (R3 '->coords)))
;
;(define omega
;  (+ (* alpha (wedge dx dy))
;     (* beta (wedge dy dz))
;     (* gamma (wedge dz dx))))
;
;(define ((L1 X) omega)
;  (+ ((interior-product X) (d omega))
;     (d ((interior-product X) omega))))
;
;
;(pec ((- (((Lie-derivative X) omega) Y Z)
;	 (((L1 X) omega) Y Z))
;      ((R3 '->point) (up 'x0 'y0 'z0))))
;#| Result:
;0
;|#
;
;(pec (let ((omega (literal-1form-field 'omega R3)))
;       ((- (((Lie-derivative X) omega) Y)
;	   (((L1 X) omega) Y))
;	((R3 '->point) (up 'x0 'y0 'z0)))))
;#| Result:
;0
;|#
;
;(pec (let ((omega (* alpha (wedge dx dy dz))))
;       ((- (((Lie-derivative X) omega) Y Z W)
;	   (((L1 X) omega) Y Z W))
;	((R3 '->point) (up 'x0 'y0 'z0)))))
;#| Result:
;0
;|#
;
;|#