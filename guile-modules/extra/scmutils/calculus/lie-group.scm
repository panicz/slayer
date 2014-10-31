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

(define (define-Lie-group group-name
	  element-type
	  group-multiplication
	  group-inverse
	  group-identity
	  manifold-type manifold-dimension)
  (let ((manifold-name (symbol group-name ': 'manifold)))
    (define-manifold manifold-name
      manifold-type manifold-dimension
      element-type)
    ;; Connecting the group to its manifold.
    (eq-put! group-name 'manifold-name manifold-name)
    (eq-put! manifold-name 'group-name group-name)
    (let ((element-prototype
	   (structure->prototype group-name element-type))
	  (element-chains
	   (structure->access-chains element-type)))
      ;; Specifying the group
      (define (the-group m)
	(case m
	  ((name group-name) group-name)

	  ((element-type) element-type)
	  ((element-dimension) element-dimension)

	  ((typical-element) (typical-object element-prototype))
	  ((element-prototype) element-prototype)
	  ((element-chains) element-chains)
	  ((group-multiplication) group-multiplication)
	  ((group-inverse) group-inverse)
	  ((group-identity) group-identity)
	  ((group-manifold-name) manifold-name)
	  ((group-manifold)
	   (eval manifold-name generic-environment))
	  (else
	   (error "Unknown message: Lie-group" group-name m))))
      (if (environment-bound? generic-environment group-name)
	  (write-line `(clobbering ,group-name)))
      (environment-define generic-environment group-name the-group)))
  group-name)

;#|
;;;; Structures, not matrices.
;
;(define-Lie-group 'SO3
;  ;; Rotations are represented by 3x3 matrix structures.
;  (down (up Real Real Real)
;	(up Real Real Real)
;	(up Real Real Real))
;  *
;  invert
;  (down (up 1 0 0)
;	(up 0 1 0)
;	(up 0 0 1))
;  Real 3)
;
;(define-coordinate-system (SO3 'group-manifold)
;  'pavel-angles
;  (up 'theta 'phi 'psi)
;
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 3))
;	(let ((theta (ref coords 0))
;	      (phi (ref coords 1))
;	      (psi (ref coords 2)))
;	  (m->s (down 'x 'y 'z)
;		(* (rotate-y-matrix phi)
;		   (* (rotate-x-matrix theta)
;		      (rotate-z-matrix psi)))
;		(up 'u 'v 'w)))
;	(error "Bad coordinates: real-tuple" coords)))
;  (lambda (point)
;    (if (and (structure? point)
;	     (fix:= (s:dimension point) 9))
;	(up (asin (- (ref point 2 1)))
;	    (atan (ref point 2 0) (ref point 2 2))
;	    (atan (ref point 0 1) (ref point 1 1)))
;	(error "Bad point: " point)))
;  )
;
;(install-coordinates ((SO3 'group-manifold) 'pavel-angles))
;
;(pec ((((SO3 'group-manifold) 'pavel-angles) '->point) (up 'theta 'phi 'psi)))
;#| Result:
;(down
; (up (+ (* (sin phi) (sin psi) (sin theta)) (* (cos psi) (cos phi)))
;     (* (sin psi) (cos theta))
;     (+ (* (sin psi) (sin theta) (cos phi)) (* -1 (sin phi) (cos psi))))
; (up (+ (* (sin phi) (sin theta) (cos psi)) (* -1 (sin psi) (cos phi)))
;     (* (cos psi) (cos theta))
;     (+ (* (sin theta) (cos psi) (cos phi)) (* (sin phi) (sin psi))))
; (up (* (sin phi) (cos theta))
;     (* -1 (sin theta))
;     (* (cos phi) (cos theta))))
;|#
;
;(let ((ge (((SO3 'group-manifold) 'pavel-angles) '->point))
;      (g* (SO3 'group-multiplication))
;      (I (SO3 'group-identity)))
;  (pec (g* (ge (up 0 0 pi/2))
;	   (g* (ge (up (- pi/2) 0 0))
;	       (g* (ge (up 0 pi/2 0))
;		   (ge (up pi/2 0 0)))))))
;#| Result:
;(down (up 1. 0. -6.123031769111886e-17)
;      (up 0. 1. -6.123031769111886e-17)
;      (up 6.123031769111886e-17 6.123031769111886e-17 1.))
;|#
;
;
;(let ((ge (((SO3 'group-manifold) 'pavel-angles) '->point))
;      (gc (((SO3 'group-manifold) 'pavel-angles) '->coords))
;      (g* (SO3 'group-multiplication))
;      (I (SO3 'group-identity)))
;  (pec (gc 
;	(g* (ge (up 0 0 pi/2))
;	    (g* (ge (up (- pi/2) 0 0))
;		(g* (ge (up 0 pi/2 0))
;		    (ge (up pi/2 0 0))))))))
;#| Result:
;(up -6.123031769111886e-17 6.123031769111886e-17 0.)
;|#
;
;
;
;(define f
;  (literal-manifold-function 'F
;			     ((SO3 'group-manifold) 'pavel-angles)))
;;Value: f
;
;(define v
;  (literal-vector-field 'V
;			((SO3 'group-manifold) 'pavel-angles)))
;;Value: v
;
;(define m
;  ((((SO3 'group-manifold) 'pavel-angles) '->point)
;   (up 'theta 'phi 'psi)))
;;Value: m
;
;(pec ((v f) m))
;#| Result:
;(+ (* (V^0 (up theta phi psi)) (((partial 0) F) (up theta phi psi)))
;   (* (V^1 (up theta phi psi)) (((partial 1) F) (up theta phi psi)))
;   (* (V^2 (up theta phi psi)) (((partial 2) F) (up theta phi psi))))
;|#
;|#

;;; For the special case of Lie groups, this is a differential of the
;;; vector field v to the place m through the map phi_m.

;#|
;(define (((extend v G) f) m)
;  (let ((phi_m
;	 (lambda (n)
;	   ((G 'group-multiplication) m n))))
;    ((v (compose f phi_m)) (G 'group-identity))))
;
;(define (extend v G)
;  (define ((the-extension f) m)
;    (let ((phi_m
;	   (lambda (n)
;	     ((G 'group-multiplication) m n))))
;      ((((differential phi_m) v) f) (G 'group-identity))))
;  the-extension)
;|#

;;; Given a vector field on G this makes a new left-invariant vector
;;; field on G.

(define (left-extend v G)
  (define ((the-extension f) m)
    (let ((phi_m
	   (lambda (n)
	     ((G 'group-multiplication) m n))))
      ((v (compose f phi_m)) (G 'group-identity))))
  (procedure->vector-field the-extension
			   `(left-extend ,(diffop-name v) ,G)))
	

;;; From p.42 Olver.

(define (left-invariant? v G coord-system-name)
  (let ((phi_m 
	 (lambda (m)
	   (lambda (n)
	     ((G 'group-multiplication) m n))))
	(f
	 (literal-manifold-function 'F
	    ((G 'group-manifold) coord-system-name))))
    (lambda (m)
      (lambda (place)
	(- ((((differential (phi_m m)) v) f) 
	    place)
	   ((v f)
	    ((G 'group-multiplication) m place))
	   )))))    
;#|
;(define x
;  (up 1.2342 0.94357 1.325987))
;
;(define ((fi m) n) (* m n))
;(define ((fiinv m) n) (* (invert m) n))
;
;
;(define chi (((SO3 'group-manifold) 'pavel-angles) '->coords))
;(define chiinv (((SO3 'group-manifold) 'pavel-angles) '->point))
;
;(pe ((((pushforward-vector (fi (chiinv x))
;			   (fiinv (chiinv x)))
;       d/dtheta)
;      (lambda (x) x))			;depends on representation
;     (chiinv x)))
;(down
; (up -1.4844294792007465e-17 5.239610320725526e-17 -1.0760259840831576e-17)
; (up .26741104197545046 -.9438842835194433 .1938396088383152)
; (up .38417600301917765 -.08004951896031148 -.9197830576926449))
;
;(pe (((left-extend d/dtheta SO3) (lambda (x) x)) (chiinv x)))
;(down (up 0. 0. 0.)
;      (up .26741104197545046 -.9438842835194433 .1938396088383152)
;      (up .38417600301917765 -.08004951896031148 -.9197830576926449))
;
;
;
;(define Mx
;  (((left-extend d/dtheta SO3) (lambda (x) x))
;   (SO3 'group-identity)))
;
;(define My
;  (((left-extend d/dphi SO3) (lambda (x) x))
;   (SO3 'group-identity)))
;
;(define Mz
;  (((left-extend d/dpsi SO3) (lambda (x) x))
;   (SO3 'group-identity)))
;
;(pe Mx)
;(down (up 0 0 0) (up 0 0 1) (up 0 -1 0))
;
;(pe My)
;(down (up 0 0 -1) (up 0 0 0) (up 1 0 0))
;
;(pe Mz)
;(down (up 0 1 0) (up -1 0 0) (up 0 0 0))
;|#

;#|
;(let ((g (chiinv (up 'theta 'phi 'psi))))
;  (((left-extend d/dtheta SO3) 
;    (lambda (g2) 
;      (* (invert g) g2)))
;   g))
;;Value: (*down* #(#(0 0 0) #(0 0 1) #(0 -1 0)))
;|#

(define ((group->generator-matrix group tangent-vector matrix-function) g)
  (let ((ext-v (left-extend tangent-vector group)))
    (* (matrix-function (invert g))
       ((ext-v matrix-function) g))))


;#|
;
;(define-coordinate-system (SO3 'group-manifold)
;  'Euler-angles
;  (up 'theta 'phi 'psi)
;
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 3))
;	(let ((theta (ref coords 0))
;	      (phi (ref coords 1))
;	      (psi (ref coords 2)))
;	  (m->s (down 'x 'y 'z)
;		(* (rotate-z-matrix phi)
;		   (* (rotate-x-matrix theta)
;		      (rotate-z-matrix psi)))
;		(up 'u 'v 'w)))
;	(error "Bad coordinates: real-tuple" coords)))
;  (lambda (point)
;    (if (and (structure? point)
;	     (fix:= (s:dimension point) 9))
;	(up (acos (ref point 2 2))
;	    (atan (ref point 2 0) (- (ref point 2 1)))
;	    (atan (ref point 0 2) (ref point 1 2)))
;	(error "Bad point: " point)))
;  )
;
;(install-coordinates ((SO3 'group-manifold) 'Euler-angles))
;
;(pec
; (let ((g ((((SO3 'group-manifold) 'Euler-angles) '->point) 
;	   (up 'theta 'phi 'psi))))
;   ((((SO3 'group-manifold) 'Euler-angles) '->coords) 
;    g)))
;#| Result:
;(up theta phi psi)
;|#
;|#

;#|
;;;; trying to get the vector fields e_x, e_y, e_z by extension
;;;; in euler angles
;
;(pe ((dtheta (left-extend d/dtheta SO3))
;     ((((SO3 'group-manifold) 'Euler-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;;Division by zero signalled by /.
;
;(pe ((dtheta (left-extend d/dphi SO3))
;     ((((SO3 'group-manifold) 'Euler-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;;Division by zero signalled by /.
;
;(pe ((dtheta (left-extend d/dpsi SO3))
;     ((((SO3 'group-manifold) 'Euler-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;0
;
;|#

;#|
;;;; trying to get the vector fields e_x, e_y, e_z by extension
;;;; in pavel angles
;
;(install-coordinates ((SO3 'group-manifold) 'pavel-angles))
;
;(pe ((dtheta (left-extend d/dtheta SO3))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;(cos psi)
;
;(pe ((dphi (left-extend d/dtheta SO3))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;(/ (sin psi) (cos theta))
;
;(pe ((dpsi (left-extend d/dtheta SO3))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;(* (tan theta) (sin psi))
;
;(define pavel-e_x 
;  (+ (* (cos psi) d/dtheta)
;     (* (/ (sin psi) (cos theta)) d/dphi)
;     (* (* (tan theta) (sin psi)) d/dpsi)))
;
;(pe ((dtheta (left-extend d/dphi SO3))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;(* -1 (sin psi))
;
;(pe ((dphi (left-extend d/dphi SO3))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;(/ (cos psi) (cos theta))
;
;(pe ((dpsi (left-extend d/dphi SO3))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;(* (tan theta) (cos psi))
;
;(define pavel-e_z 
;  (+ (* (* -1 (sin psi)) d/dtheta)
;     (* (/ (cos psi) (cos theta)) d/dphi)
;     (* (* (tan theta) (cos psi)) d/dpsi)))
;
;(pe ((dtheta (left-extend d/dpsi SO3))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;0
;
;(pe ((dphi (left-extend d/dpsi SO3))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;0
;
;(pe ((dpsi (left-extend d/dpsi SO3))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;1
;
;(define pavel-e_y d/dpsi)
;
;(pe (((+ (commutator pavel-e_x pavel-e_y) pavel-e_z)
;      (literal-manifold-function 
;       'f ((SO3 'group-manifold) 'pavel-angles)))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;0
;
;(pe (((+ (commutator pavel-e_y pavel-e_z) pavel-e_x)
;      (literal-manifold-function 
;       'f ((SO3 'group-manifold) 'pavel-angles)))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;0
;
;(pe (((+ (commutator pavel-e_z pavel-e_x) pavel-e_y)
;      (literal-manifold-function 
;       'f ((SO3 'group-manifold) 'pavel-angles)))
;     ((((SO3 'group-manifold) 'pavel-angles) '->point) 
;      (up 'theta 'phi 'psi))))
;
;(pe (->poisson-form
;     '(+
;       (/ (* (expt (sin theta) 2) (((partial 2) f) (up theta phi psi)))
;	  (expt (cos theta) 2))
;       (((partial 2) f) (up theta phi psi))
;       (/ (* -1 (((partial 2) f) (up theta phi psi))) (expt (cos theta) 2)))))
;0
;
;|#

;#|
;
;(define-coordinate-system (SO3 'group-manifold)
;  'alternate-angles
;  (up 'theta 'phi 'psi)
;
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 3))
;	(let ((theta (ref coords 0))
;	      (phi (ref coords 1))
;	      (psi (ref coords 2)))
;	  (m->s (down 'x 'y 'z)
;		(* (rotate-z-matrix phi)
;		   (* (rotate-x-matrix theta)
;		      (rotate-y-matrix psi)))
;		(up 'u 'v 'w)))
;	(error "Bad coordinates: real-tuple" coords)))
;  (lambda (point)
;    (if (and (structure? point)
;	     (fix:= (s:dimension point) 9))
;	(up (asin (ref point 1 2))
;	    (atan (- (ref point 1 0)) (ref point 1 1))
;	    (atan (- (ref point 0 2)) (ref point 2 2)))
;	(error "Bad point: " point)))
;  )
;
;(install-coordinates ((SO3 'group-manifold) 'alternate-angles))
;
;(pec
; (let ((g ((((SO3 'group-manifold) 'alternate-angles) '->point)
;	   (up 'theta 'phi 'psi))))
;   ((((SO3 'group-manifold) 'alternate-angles) '->coords)
;    g)))
;
;#| Result:
;(up theta phi psi)
;|#
;
;|#


;#|
;;;; trying to get the vector fields eap_x, eap_y, eap_z by extension
;;;; in alternate angles
;
;(pe ((dtheta (left-extend d/dtheta SO3))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;(cos psi)
;
;(pe ((dphi (left-extend d/dtheta SO3))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;(/ (* -1 (sin psi)) (cos theta))
;
;(pe ((dpsi (left-extend d/dtheta SO3))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;(* (tan theta) (sin psi))
;
;(define eap_x
;  (+ (* (cos psi) d/dtheta)
;     (* (/ (* -1 (sin psi)) (cos theta)) d/dphi)
;     (* (* (tan theta) (sin psi)) d/dpsi)))
;
;(pe ((dtheta (left-extend d/dphi SO3))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;(sin psi)
;
;(pe ((dphi (left-extend d/dphi SO3))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;(/ (cos psi) (cos theta))
;
;(pe ((dpsi (left-extend d/dphi SO3))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;(* -1 (tan theta) (cos psi))
;
;(define eap_z
;  (+ (* (sin psi) d/dtheta)
;     (* (/ (cos psi) (cos theta)) d/dphi)
;     (* (* -1 (tan theta) (cos psi)) d/dpsi)))
;
;(pe ((dtheta (left-extend d/dpsi SO3))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;0
;
;(pe ((dphi (left-extend d/dpsi SO3))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;0
;
;(pe ((dpsi (left-extend d/dpsi SO3))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;1
;
;(define eap_y d/dpsi)
;
;(pe (((- (commutator eap_x eap_y) eap_z)
;      (literal-manifold-function
;       'f ((SO3 'group-manifold) 'alternate-angles)))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;0
;
;(pe (((- (commutator eap_y eap_z) eap_x)
;      (literal-manifold-function
;       'f ((SO3 'group-manifold) 'alternate-angles)))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;0
;
;(pe (((- (commutator eap_z eap_x) eap_y)
;      (literal-manifold-function
;       'f ((SO3 'group-manifold) 'alternate-angles)))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'theta 'phi 'psi))))
;mess, but
;(->poisson-form
; '(+
;   (/ (* -1 (((partial 2) f) (up theta phi psi)) (expt (sin theta) 2))
;      (expt (cos theta) 2))
;   (* -1 (((partial 2) f) (up theta phi psi)))
;   (/ (((partial 2) f) (up theta phi psi)) (expt (cos theta) 2))))
;;Value: 0
;|#


;#|
;(pe (((left-invariant? d/dpsi SO3 'alternate-angles)
;      ((((SO3 'group-manifold) 'alternate-angles) '->point)
;       (up 'a 'b 'c)))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 'c 'd 'e))))
;0
;
;
;(pe (((left-invariant? (left-extend d/dtheta SO3) SO3 'alternate-angles)
;      ((((SO3 'group-manifold) 'alternate-angles) '->point)
;       (up 1 2 3)))
;     ((((SO3 'group-manifold) 'alternate-angles) '->point)
;      (up 1/2 3/2 5/2))))
;0
;
;;;; Therefore, left-extend is correct!
;
;;;; The right-invariant field obtained by extend is the same as one
;;;; determined by solving for increments to the coordinates that
;;;; rotate the sphere about a given axis.
;
;;;; The left-invariant fields are obtained by converting the
;;;; right-invariant fields by some magic.
;|#