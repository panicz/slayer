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

;;; A 1form is an operator that takes a vector field to a real-valued
;;; function on the manifold.

(define (form-field? fop)
  (and (operator? fop)
       (eq? (operator-subtype fop) wedge)))

;;; A 1form field multiplies by wedge.

(define (procedure->1form-field fp name)
  (make-operator fp name wedge))

;;; Dummy... forward reference so I can define dx, dy before wedge.
(define (wedge f1 f2)
  (error "Wedge not yet defined"))

(define (ff:zero vf) zero-manifold-function)

(define (ff:zero-like op)
  (assert (form-field? op) "ff:zero-like")
  (make-op ff:zero
	   'ff:zero
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(assign-operation 'zero-like ff:zero-like form-field?)


(define (ff:zero? ff)
  (assert (form-field? ff) "ff:zero?")
  (eq? (operator-procedure ff) ff:zero))

(assign-operation 'zero? ff:zero? form-field?)


;;; A 1form is specified by a function that gives components, in a
;;; down tuple, relative to a coordinate system.

(define ((1form-field-procedure components coordinate-system) vf)
  (define (internal vf)
    (assert (vector-field? vf))
    (compose (* components
		(vector-field->components vf coordinate-system))
	     (coordinate-system '->coords)))
  (s:map/r internal vf))


(define* (components->1form-field components coordinate-system #:optional name)
  (if (default-object? name) (set! name `(1form-field ,components)))
  (procedure->1form-field
   (1form-field-procedure components coordinate-system)
   name))


;;; We can extract the components function for a form, given a
;;; coordinate system.

(define (1form-field->components form coordinate-system)
  (assert (form-field? form) "Bad form field: 1form-field->components")
  (let ((X (coordinate-system->vector-basis coordinate-system)))
    (compose (form X) (coordinate-system '->point))))


;;; It is often useful to construct a 1form field

(define (literal-1form-field name coordinate-system)
  (let ((n (coordinate-system 'dimension)))
    (let ((function-signature
	   (if (fix:= n 1) (-> Real Real) (-> (UP* Real n) Real))))
      (let ((components
	     (s:generate n 'down
			 (lambda (i)
			   (literal-function (string->symbol
					      (string-append
					       (symbol->string name)
					       "_"
					       (number->string i)))
					     function-signature)))))
	(components->1form-field components coordinate-system name)))))

;;; To get the elements of a coordinate basis for the 1-form fields

(define ((coordinate-basis-1form-field-procedure coordinate-system . i) vf)
  (define (internal vf)
    (assert (vector-field? vf)
	    "Bad vector field: coordinate-basis-1form-field")
    (vf (compose (apply component i) (coordinate-system '->coords))))
  (s:map/r internal vf))

(define (coordinate-basis-1form-field coordinate-system name . i)
  (procedure->1form-field
   (apply coordinate-basis-1form-field-procedure coordinate-system i)
   name))

(define (coordinate-system->1form-basis coordinate-system)
  (s:map (lambda (chain)
	   (apply coordinate-basis-1form-field
		  coordinate-system
		  `(w ,@chain)
		  chain))
	 (coordinate-system 'access-chains)))

;#|
;(define ((coordinate-system->1form-basis-procedure coordinate-system) vf)
;  (vf (coordinate-system '->coords)))
;|#

;;; Given component functions defined on manifold points and a 1-form
;;; basis, to produce the 1-form field as a linear combination.

(define ((basis-components->1form-field components 1form-basis) f)
  (* components (1form-basis f)))

(define (1form-field->basis-components w vector-basis)
  (s:map/r w vector-basis))



;;; This is one of the two incompatible definitions of "differential".
;;; The other one appears in maps.scm.

(define (function->1form-field f)
  (procedure->1form-field
   (lambda (v)
     (assert (vector-field? v))
     (lambda (m)
       ((v f) m)))
   `(d ,(diffop-name f))))

(define differential-of-function function->1form-field)

;#|
;(define rectangular-3space (rectangular 3))
;(instantiate-coordinates rectangular-3space '(x y z))
;
;(define mr ((rectangular-3space '->point) (up 'x0 'y0 'z0)))
;
;(define a-1form
;  (components->1form-field
;   (down (literal-function 'ax (-> (UP* Real) Real))
;	 (literal-function 'ay (-> (UP* Real) Real))
;	 (literal-function 'az (-> (UP* Real) Real)))
;   rectangular-3space))
;
;(define a-vector-field
;  (components->vector-field
;   (up (literal-function 'vx (-> (UP* Real) Real))
;       (literal-function 'vy (-> (UP* Real) Real))
;       (literal-function 'vz (-> (UP* Real) Real)))
;   rectangular-3space))
;
;(pec ((a-1form a-vector-field) mr))
;#| Result:
;(+ (* (vx (up x0 y0 z0)) (ax (up x0 y0 z0)))
;   (* (vy (up x0 y0 z0)) (ay (up x0 y0 z0)))
;   (* (vz (up x0 y0 z0)) (az (up x0 y0 z0))))
;|#
;
;(pec ((1form-field->components a-1form rectangular-3space) (up 'x0 'y0 'z0)))
;#| Result:
;(down (ax (up x0 y0 z0)) (ay (up x0 y0 z0)) (az (up x0 y0 z0)))
;|#
;
;
;(define cylindrical (polar/cylindrical 3))
;
;(instantiate-coordinates cylindrical '(r theta zeta))
;
;(pec ((1form-field->components a-1form cylindrical) (up 'r0 'theta0 'z0)))
;#| Result:
;(down
; (+ (* (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)) (cos theta0))
;    (* (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)) (sin theta0)))
; (+ (* -1 r0 (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)) (sin theta0))
;    (* r0 (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)) (cos theta0)))
; (az (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
;|#
;
;|#

;#|
;(define mr ((rectangular-3space '->point) (up 'x0 'y0 'z0)))
;
;((dx d/dx) mr)
;;Value 1
;
;(define mp ((cylindrical '->point) (up 'r0 'theta0 'z0)))
;
;(pec ((1form-field->components dr rectangular-3space) mp))
;#| Result:
;(down (cos theta0) (sin theta0) 0)
;|#
;
;(pec ((1form-field->components dr rectangular-3space) mr))
;#| Result:
;(down (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
;      (/ y0 (sqrt (+ (expt x0 2) (expt y0 2))))
;      0)
;|#
;
;(pec ((1form-field->components dtheta rectangular-3space) mp))
;#| Result:
;(down (/ (* -1 (sin theta0)) r0) (/ (cos theta0) r0) 0)
;|#
;
;(pec ((1form-field->components dtheta rectangular-3space) mr))
;#| Result:
;(down (/ (* -1 y0) (+ (expt x0 2) (expt y0 2)))
;      (/ x0 (+ (expt x0 2) (expt y0 2)))
;      0)
;|#
;
;
;(pec (((+ (* 'w_0 dr) (* 'w_1 dtheta)) (+ (* 'V^0 d/dx) (* 'V^1 d/dy))) mp))
;#| Result:
;(+ (* V^0 w_0 (cos theta0))
;   (* V^1 w_0 (sin theta0))
;   (/ (* -1 V^0 w_1 (sin theta0)) r0)
;   (/ (* V^1 w_1 (cos theta0)) r0))
;|#
;
;(pec
; (((components->1form-field (1form-field->components
;			     (+ (* 'w_0 dr) (* 'w_1 dtheta))
;			     rectangular-3space)
;			    rectangular-3space)
;   (+ (* 'V^0 d/dx) (* 'V^1 d/dy)))
;  mp))
;#| Result:
;(+ (* V^0 w_0 (cos theta0))
;   (* V^1 w_0 (sin theta0))
;   (/ (* -1 V^0 w_1 (sin theta0)) r0)
;   (/ (* V^1 w_1 (cos theta0)) r0))
;|#
;
;
;
;(define counter-clockwise (- (* x d/dy) (* y d/dx)))
;
;(define outward (+ (* x d/dx) (* y d/dy)))
;
;
;(pec ((dx counter-clockwise) mr))
;#| Result:
;(* -1 y0)
;|#
;
;(pec ((dx outward) mr))
;#| Result:
;x0
;|#
;
;(pec ((dr counter-clockwise) mp))
;#| Result:
;0
;|#
;
;(pec ((dr outward) mp))
;#| Result:
;r0
;|#
;
;(pec ((dr outward) mr))
;#| Result:
;(sqrt (+ (expt x0 2) (expt y0 2)))
;|#
;
;(pec (((* x dy) (+ (* 'u d/dx) (* 'v d/dy))) mr))
;#| Result:
;(* v x0)
;|#
;
;(pec ((dr d/dr) ((rectangular-3space '->point) (up 'x^0 'y^0 'z^0))))
;#| Result:
;(+
; (/ (expt x^0 2)
;    (sqrt (+ (expt x^0 4) (* 2 (expt x^0 2) (expt y^0 2)) (expt y^0 4))))
; (/ (expt y^0 2)
;    (sqrt (+ (expt x^0 4) (* 2 (expt x^0 2) (expt y^0 2)) (expt y^0 4)))))
; ;; = 1
;|#
;
;(pec ((dr d/dtheta) ((rectangular-3space '->point) (up 'x^0 'y^0 'z^0))))
;#| Result:
;0
;|#
;
;(pec ((dtheta d/dr) ((rectangular-3space '->point) (up 'x^0 'y^0 'z^0))))
;#| Result:
;0
;|#
;
;(pec ((dtheta d/dtheta) ((rectangular-3space '->point) (up 'x^0 'y^0 'z^0))))
;#| Result:
;1
;|#
;|#
