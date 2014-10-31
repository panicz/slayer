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


;#|
;(define-manifold 'Euclidean-plane Real 2 (up Real Real))
;
;(define-coordinate-system Euclidean-plane 'rectangular (up 'x 'y)
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 2))
;	coords
;	(error "Bad coordinates: real-tuple" coords)))
;  (lambda (point)
;    (if (and (up? point) (fix:= (s:dimension point) 2))
;	point
;	(error "Bad point: real-tuple" point))))
;
;(define-coordinate-system Euclidean-plane 'polar (up 'r 'theta)
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 2))
;	(let ((r (ref coords 0))
;	      (theta (ref coords 1)))
;	  (s:generate (s:dimension coords) 'up
;		      (lambda (i)
;			(cond ((= i 0) (* r (cos theta)))
;			      ((= i 1) (* r (sin theta)))))))
;	(error "Bad coordinates: polar" coords)))
;  (lambda (point)
;    (if (and (up? point) (fix:= (s:dimension point) 2))
;	(let ((x (ref point 0))
;	      (y (ref point 1)))
;	  (s:generate (s:dimension point) 'up
;		      (lambda (i)
;			(cond ((= i 0) (sqrt (+ (square x) (square y))))
;			      ((= i 1) (atan y x))))))
;	(error "Bad point: polar" point))))
;
;(install-coordinates (Euclidean-plane 'rectangular))
;(install-coordinates (Euclidean-plane 'polar))
;
;(define mr (((Euclidean-plane 'rectangular) '->point) (up 'x0 'y0)))
;(define mp (((Euclidean-plane 'polar) '->point) (up 'r0 'theta0)))
;
;(pe (((vector-field->tensor-field d/dx) dtheta) mp))
;(/ (* -1 (sin theta0)) r0)
;
;(pe (((1form-field->tensor-field dy) d/dtheta) mp))
;(* r0 (cos theta0))
;
;(pe (((vector-field->tensor-field d/dx) dr) mp))
;(cos theta0)
;
;
;(define V
;  (literal-vector-field 'v (Euclidean-plane 'rectangular)))
;
;(define F
;  (literal-manifold-function 'f (Euclidean-plane 'rectangular)))
;
;(pe ((V F) mr))
;(+ (* (((partial 0) f) (up x0 y0)) (v^0 (up x0 y0)))
;   (* (((partial 1) f) (up x0 y0)) (v^1 (up x0 y0))))
;
;(pe (((10tensor-field->vector-field
;       (vector-field->tensor-field V Euclidean-plane)
;       (Euclidean-plane 'rectangular))
;      F)
;     mr))
;(+ (* (v^0 (up x0 y0)) (((partial 0) f) (up x0 y0)))
;   (* (v^1 (up x0 y0)) (((partial 1) f) (up x0 y0))))
;
;(pe (((10tensor-field->vector-field
;       (vector-field->tensor-field V Euclidean-plane)
;       (Euclidean-plane 'polar))
;      F)
;     mr))
;(+ (* (((partial 0) f) (up x0 y0)) (v^0 (up x0 y0)))
;   (* (((partial 1) f) (up x0 y0)) (v^1 (up x0 y0))))
;|#

;#|
;(pe (((tensor-product (vector-field->tensor-field d/dx)
;		      (1form-field->tensor-field dy))
;      dr d/dtheta)
;     mp))
;(* r0 (expt (cos theta0) 2))
;
;(pe (((1form-field->tensor-field dr) d/dy) mp))
;(sin theta0)
;
;(pe (((tensor-product
;       (tensor-product (1form-field->tensor-field dy)
;		       (vector-field->tensor-field d/dx))
;       (1form-field->tensor-field dr))
;      dr d/dtheta d/dy)
;     mp))
;(* r0 (sin theta0) (expt (cos theta0) 2))
;
;(define T
;  (* (1form-field->tensor-field dy)
;     (vector-field->tensor-field d/dx)
;     (1form-field->tensor-field dr)))
;
;(tensor-type T)
;;Value: (1 . 2)
;
;(pe ((T dr d/dtheta d/dy) mp))
;(* r0 (sin theta0) (expt (cos theta0) 2))
;|#

;#|
;(define g
;  (+ (square (1form-field->tensor-field dx))
;     (square (1form-field->tensor-field dy))))
;   
;(pe ((g (literal-vector-field 'v (Euclidean-plane 'rectangular))
;	(literal-vector-field 'w (Euclidean-plane 'rectangular)))
;     mr))
;(+ (* (v^0 (up x0 y0)) (w^0 (up x0 y0)))
;   (* (v^1 (up x0 y0)) (w^1 (up x0 y0))))
;
;(pe ((g (literal-vector-field 'v (Euclidean-plane 'polar))
;	(literal-vector-field 'w (Euclidean-plane 'polar)))
;     mp))
;(+ (* (expt r0 2) (v^1 (up r0 theta0)) (w^1 (up r0 theta0)))
;   (* (v^0 (up r0 theta0)) (w^0 (up r0 theta0))))
;|#
	  
;#|
;(define T
;  (* (1form-field->tensor-field dy)
;     (vector-field->tensor-field d/dx)
;     (1form-field->tensor-field dr)))
;
;(define polar-coordinates (Euclidean-plane 'polar))
;
;(define rectangular-coordinates (Euclidean-plane 'rectangular))
;
;
;(pe (((contract T 0 1 polar-coordinates) d/dy) mp))
;(cos theta0)
;
;(pe (((contract T 0 1 rectangular-coordinates) d/dy) mp))
;(cos theta0)
;
;
;(pe (((contract T 0 1 polar-coordinates) d/dtheta) mp))
;(* r0 (expt (cos theta0) 2))
;
;(pe (((contract T 0 1 rectangular-coordinates) d/dtheta) mp))
;(* r0 (expt (cos theta0) 2))
;
;(pe (((contract T 0 0 polar-coordinates) d/dy) mp))
;0
;
;(pe (((contract T 0 0 rectangular-coordinates) d/dy) mp))
;0
;|#
;#|
;(pe ((tensor-field->coefficient-structure T (Euclidean-plane 'rectangular)) mp))
;(down (down (up 0 0)
;	    (up (cos theta0) 0))
;      (down (up 0 0)
;	    (up (sin theta0) 0)))
;
;
;(pe ((tensor-field->coefficient-structure g (Euclidean-plane 'rectangular))
;     mp))
;(down (down 1 0) (down 0 1))
;
;(pe ((tensor-field->coefficient-structure g (Euclidean-plane 'polar))
;     mp))
;(down (down 1 0) (down 0 (expt r0 2)))
;
;(pe ((tensor-field->coefficient-structure g (Euclidean-plane 'polar))
;     mr))
;(down (down 1 0) (down 0 (+ (expt x0 2) (expt y0 2))))
;
;(pe ((tensor-field->coefficient-structure g (Euclidean-plane 'rectangular))
;     mr))
;(down (down 1 0) (down 0 1))
;|#
;#|
;(pe (((contract (coefficient-structure->tensor-field
;		 (tensor-field->coefficient-structure T
;						      (Euclidean-plane 'rectangular))
;		 (Euclidean-plane 'rectangular))
;		0 1
;		rectangular-coordinates)
;      d/dtheta)
;     mp))
;(* r0 (expt (cos theta0) 2))
;
;(pe ((tensor-field->coefficient-structure
;      (coefficient-structure->tensor-field
;       (tensor-field->coefficient-structure T (Euclidean-plane 'rectangular))
;       (Euclidean-plane 'rectangular))
;      (Euclidean-plane 'rectangular))
;     mp))
;(down (down (up 0 0)
;	    (up (cos theta0) 0))
;      (down (up 0 0)
;	    (up (sin theta0) 0)))
;|#
;#|  
;(define g2
;  (+ (* (literal-manifold-function 'g_a (Euclidean-plane 'rectangular))
;	(square (1form-field->tensor-field dx)))
;     (* (literal-manifold-function 'g_b (Euclidean-plane 'rectangular))
;	(1form-field->tensor-field dx)
;	(1form-field->tensor-field dy))
;     (* (literal-manifold-function 'g_b (Euclidean-plane 'rectangular))
;	(1form-field->tensor-field dy)
;	(1form-field->tensor-field dx))
;     (* (literal-manifold-function 'g_c (Euclidean-plane 'rectangular))
;	(square (1form-field->tensor-field dy)))))
;
;(pe ((tensor-field->coefficient-structure g2 (Euclidean-plane 'rectangular)) mr))
;(down (down (g_a (up x0 y0)) (g_b (up x0 y0)))
;      (down (g_b (up x0 y0)) (g_c (up x0 y0))))
;
;(pe ((tensor-field->coefficient-structure
;      (invert-metric g2 (Euclidean-plane 'rectangular))
;       (Euclidean-plane 'rectangular))
;     mr))
;(up
; (up
;  (/ (* -1 (g_c (up x0 y0)))
;     (+ (* -1 (g_a (up x0 y0)) (g_c (up x0 y0))) (expt (g_b (up x0 y0)) 2)))
;  (/ (g_b (up x0 y0))
;     (+ (* -1 (g_a (up x0 y0)) (g_c (up x0 y0))) (expt (g_b (up x0 y0)) 2))))
; (up
;  (/ (g_b (up x0 y0))
;     (+ (* -1 (g_a (up x0 y0)) (g_c (up x0 y0))) (expt (g_b (up x0 y0)) 2)))
;  (/ (* -1 (g_a (up x0 y0)))
;     (+ (* -1 (g_a (up x0 y0)) (g_c (up x0 y0))) (expt (g_b (up x0 y0)) 2)))))
;|#
;#|
;(define rectangular-coordinates (Euclidean-plane 'rectangular))
;
;(define L
;  (+ (* (literal-manifold-function 'L^xx rectangular-coordinates)
;	(vector-field->tensor-field d/dx)
;	(vector-field->tensor-field d/dx))
;     (* (literal-manifold-function 'L^xy rectangular-coordinates)
;	(vector-field->tensor-field d/dx)
;	(vector-field->tensor-field d/dy))
;     (* (literal-manifold-function 'L^yx rectangular-coordinates)
;	(vector-field->tensor-field d/dy)
;	(vector-field->tensor-field d/dx))
;     (* (literal-manifold-function 'L^yy rectangular-coordinates)
;	(vector-field->tensor-field d/dy)
;	(vector-field->tensor-field d/dy))))
;
;(pe ((tensor-field->coefficient-structure L rectangular-coordinates) mr))
;(up (up (L^xx (up x0 y0))
;	(L^yx (up x0 y0)))
;    (up (L^xy (up x0 y0))
;	(L^yy (up x0 y0))))
;
;(pe ((tensor-field->coefficient-structure
;      (t:comma L rectangular-coordinates)
;      rectangular-coordinates)
;     mr))
;(down
; (up (up (((partial 0) L^xx) (up x0 y0))
;	 (((partial 0) L^yx) (up x0 y0)))
;     (up (((partial 0) L^xy) (up x0 y0))
;	 (((partial 0) L^yy) (up x0 y0))))
; (up (up (((partial 1) L^xx) (up x0 y0))
;	 (((partial 1) L^yx) (up x0 y0)))
;     (up (((partial 1) L^xy) (up x0 y0))
;	 (((partial 1) L^yy) (up x0 y0)))))
;
;(define M
;  (+ (* (literal-manifold-function 'M_xx rectangular-coordinates)
;	(1form-field->tensor-field dx)
;	(1form-field->tensor-field dx))
;     (* (literal-manifold-function 'M_xy rectangular-coordinates)
;	(1form-field->tensor-field dx)
;	(1form-field->tensor-field dy))
;     (* (literal-manifold-function 'M_yx rectangular-coordinates)
;	(1form-field->tensor-field dy)
;	(1form-field->tensor-field dx))
;     (* (literal-manifold-function 'M_yy rectangular-coordinates)
;	(1form-field->tensor-field dy)
;	(1form-field->tensor-field dy))))
;
;(pe ((tensor-field->coefficient-structure M rectangular-coordinates) mr))
;(down (down (M_xx (up x0 y0))
;	    (M_yx (up x0 y0)))
;      (down (M_xy (up x0 y0))
;	    (M_yy (up x0 y0))))
;
;(pe ((tensor-field->coefficient-structure
;      (t:comma M rectangular-coordinates)
;      rectangular-coordinates)
;     mr))
;(down
; (down (down (((partial 0) M_xx) (up x0 y0))
;	     (((partial 0) M_yx) (up x0 y0)))
;       (down (((partial 0) M_xy) (up x0 y0))
;	     (((partial 0) M_yy) (up x0 y0))))
; (down (down (((partial 1) M_xx) (up x0 y0))
;	     (((partial 1) M_yx) (up x0 y0)))
;       (down (((partial 1) M_xy) (up x0 y0))
;	     (((partial 1) M_yy) (up x0 y0)))))
;
;(define N
;  (+ (* (literal-manifold-function 'N^x_x rectangular-coordinates)
;	(vector-field->tensor-field d/dx)
;	(1form-field->tensor-field dx))
;     (* (literal-manifold-function 'N^x_y rectangular-coordinates)
;	(vector-field->tensor-field d/dx)
;	(1form-field->tensor-field dy))
;     (* (literal-manifold-function 'N^y_x rectangular-coordinates)
;	(vector-field->tensor-field d/dy)
;	(1form-field->tensor-field dx))
;     (* (literal-manifold-function 'N^y_y rectangular-coordinates)
;	(vector-field->tensor-field d/dy)
;	(1form-field->tensor-field dy))))
;
;(pe ((tensor-field->coefficient-structure N rectangular-coordinates) mr))
;(down (up (N^x_x (up x0 y0))
;	  (N^y_x (up x0 y0)))
;      (up (N^x_y (up x0 y0))
;	  (N^y_y (up x0 y0))))
;
;(pe ((tensor-field->coefficient-structure
;      (t:comma N rectangular-coordinates)
;      rectangular-coordinates)
;     mr))
;(down
; (down (up (((partial 0) N^x_x) (up x0 y0))
;	   (((partial 0) N^y_x) (up x0 y0)))
;       (up (((partial 0) N^x_y) (up x0 y0))
;	   (((partial 0) N^y_y) (up x0 y0))))
; (down (up (((partial 1) N^x_x) (up x0 y0))
;	   (((partial 1) N^y_x) (up x0 y0)))
;       (up (((partial 1) N^x_y) (up x0 y0))
;	   (((partial 1) N^y_y) (up x0 y0)))))
;|#
;#|
;(define-manifold 'S2M Real 2 (up Real Real Real))
;
;(define-coordinate-system S2M 'colatitude-longitude (up 'theta 'phi)
;  (lambda (coords)			;coordinates->point
;    (if (and (up? coords) (fix:= (s:dimension coords) 2))
;	(let ((theta (ref coords 0))
;	      (phi   (ref coords 1)))
;	  (up (* (sin theta) (cos phi)) ;x
;	      (* (sin theta) (sin phi)) ;y
;	      (cos theta)))		;z
;	(error "Bad coordinates: S2M" coords)))
;  (lambda (point)			;point->coordinates 
;    (if (and (up? point) (fix:= (s:dimension point) 3))
;	(let ((x (ref point 0))
;	      (y (ref point 1))
;	      (z (ref point 2)))
;	  ;;(assert (= 1
;	  ;;           (+ (square x) (square y) (square z))))
;	  (up (acos z)			;theta
;	      (atan y x)))		;phi
;	(error "Bad point: S2M" point))))
;
;
;(define spherical-coordinates (S2M 'colatitude-longitude))
;
;(install-coordinates spherical-coordinates)
;
;(define ms (((S2M 'colatitude-longitude) '->point) (up 'theta0 'phi0)))
;
;(define S2M-metric
;  (let ((dt (1form-field->tensor-field dtheta))
;	(dp (1form-field->tensor-field dphi)))
;    (+ (* dt dt)
;       (* (square (compose sin theta))
;	  dp dp))))    
;
;(pe ((tensor-field->coefficient-structure S2M-metric spherical-coordinates) ms))
;(down (down 1 0) (down 0 (expt (sin theta0) 2)))
;
;(pe ((tensor-field->coefficient-structure 
;      (t:comma S2M-metric spherical-coordinates)
;      spherical-coordinates)
;     ms))
;(down (down (down 0 0) (down 0 (* 2 (sin theta0) (cos theta0))))
;      (down (down 0 0) (down 0 0)))
;
;(pe ((tensor-field->coefficient-structure
;      (Christoffel S2M-metric spherical-coordinates)
;      spherical-coordinates)
;     ms))
;(down
; (down (up 0 0)
;       (up 0 (/ (cos theta0) (sin theta0))))
; (down (up 0 (/ (cos theta0) (sin theta0)))
;       (up (* -1 (sin theta0) (cos theta0)) 0)))
;
;
;;;; General case...
;(define R
;  (+ (* (literal-manifold-function 'M_tt spherical-coordinates)
;	(1form-field->tensor-field dtheta)
;	(1form-field->tensor-field dtheta))
;     (* (literal-manifold-function 'M_tp spherical-coordinates)
;	(1form-field->tensor-field dtheta)
;	(1form-field->tensor-field dphi))
;     (* (literal-manifold-function 'M_pt spherical-coordinates)
;	(1form-field->tensor-field dphi)
;	(1form-field->tensor-field dtheta))
;     (* (literal-manifold-function 'M_py spherical-coordinates)
;	(1form-field->tensor-field dphi)
;	(1form-field->tensor-field dphi))))
;
;(pe ((tensor-field->coefficient-structure R spherical-coordinates) ms))
;(down (down (M_tt (up theta0 phi0)) (M_pt (up theta0 phi0)))
;      (down (M_tp (up theta0 phi0)) (M_py (up theta0 phi0))))
;
;(pe ((tensor-field->coefficient-structure 
;      (t:comma R spherical-coordinates)
;      spherical-coordinates)
;     ms))
;(down
; (down
;  (down (((partial 0) M_tt) (up theta0 phi0))
;	(((partial 0) M_pt) (up theta0 phi0)))
;  (down (((partial 0) M_tp) (up theta0 phi0))
;	(((partial 0) M_py) (up theta0 phi0))))
; (down
;  (down (((partial 1) M_tt) (up theta0 phi0))
;	(((partial 1) M_pt) (up theta0 phi0)))
;  (down (((partial 1) M_tp) (up theta0 phi0))
;        (((partial 1) M_py) (up theta0 phi0)))))
;|#

;#|
;(define V
;  (vector-field->tensor-field
;   (literal-vector-field 'V spherical-coordinates)
;   S2M))
;
;(define del (t:covariant-derivative S2M-metric spherical-coordinates))
;
;(pe ((((del d/dtheta) V) dtheta) ms))
;(((partial 0) V^0) (up theta0 phi0))
;
;(pe ((((del d/dphi) V) dtheta) ms))
;(+ (* -1 (cos theta0) (sin theta0) (V^1 (up theta0 phi0)))
;   (((partial 1) V^0) (up theta0 phi0)))
;
;(pe ((((del d/dtheta) V) dphi) ms))
;(+ (((partial 0) V^1) (up theta0 phi0))
;   (/ (* (cos theta0) (V^1 (up theta0 phi0))) (sin theta0)))
;
;(pe ((((del d/dphi) V) dphi) ms))
;(+ (((partial 1) V^1) (up theta0 phi0))
;   (/ (* (cos theta0) (V^0 (up theta0 phi0))) (sin theta0)))
;|#
;#|
;(pe ((tensor-field->coefficient-structure
;      (Riemann-tensor S2M-metric spherical-coordinates)
;      spherical-coordinates)
;     ms))
;(down
; (down (down (up 0 0) (up 0 0))
;       (down (up 0 1) (up (* -1 (expt (sin theta0) 2)) 0)))
; (down (down (up 0 -1) (up (expt (sin theta0) 2) 0))
;       (down (up 0 0) (up 0 0))))
;
;(pe ((tensor-field->coefficient-structure
;      (Ricci-tensor S2M-metric spherical-coordinates)
;      spherical-coordinates)
;     ms))
;(down (down 1 0) (down 0 (expt (sin theta0) 2)))
;
;(pe (((Ricci-scalar S2M-metric spherical-coordinates)) ms))
;2
;|#

;#|
;;; Linux zohar 2.6.12.2pm0 #2 Fri Jul 8 16:45:59 EDT 2005 i686 GNU/Linux
;;; 1800 MIPS, 2048 kB cache, edwin -constant 2000 -heap 6000 
;;;Image saved on Saturday December 10, 2005 at 1:47:21 AM
;;;  Release 7.7.90.+                 || Microcode 14.16 || Runtime 15.6
;;;  SF 4.41                          || LIAR 4.117      || Edwin 3.116
;;;  ScmUtils Mechanics . Winter 2006
;
;(show-time
; (lambda ()
;   (pec ((tensor-field->coefficient-structure
;	  (Ricci-scalar S2M-metric spherical-coordinates)
;	  spherical-coordinates)
;	 ms))))
;#| Result:
;2
;;; process time: 59040 (55730 RUN + 3310 GC); real time: 62216
;;; process time: 43530 (40920 RUN + 2610 GC); real time: 43736
;;; process time: 7640 (7250 RUN + 390 GC); real time: 7653
;|#
;
;(show-time
; (lambda ()
;   (pec (((Ricci-scalar S2M-metric spherical-coordinates))
;	 ms))))
;#| Result:
;2
;;; process time: 44450 (41770 RUN + 2680 GC); real time: 44580
;;; almost all of the time is in final expression simplification.
;
;;; process time: 11580 (11140 RUN + 440 GC); real time: 11613
;;; with derivative idea...
;;; process time: 7590 (7200 RUN + 390 GC); real time: 7591
;;; Alternate definition of Riemann-tensor
;;; process time: 1320 (1230 RUN + 90 GC); real time: 1321
;|#
;
;(show-time
; (lambda ()
;   (pec ((tensor-field->coefficient-structure
;	  (Riemann-tensor S2M-metric spherical-coordinates)
;	  spherical-coordinates)
;	 ms))))
;#| Result:
;(down
; (down (down (up 0 0) (up 0 0))
;       (down (up 0 1) (up (* -1 (expt (sin theta0) 2)) 0)))
; (down (down (up 0 -1) (up (expt (sin theta0) 2) 0))
;       (down (up 0 0) (up 0 0))))
;;; No memoizer: garbage-collecting itself to death!
;;; process time: 2555260 (466710 RUN + 2088550 GC); real time: 2564412
;;; With simplification memoizers
;;; process time: 113500 (106570 RUN + 6930 GC); real time: 113784
;;; process time: 87950 (82870 RUN + 5080 GC); real time: 88338
;
;;; process time: 22750 (21910 RUN + 840 GC); real time: 22822
;;; with derivative idea!
;;; process time: 15160 (14430 RUN + 730 GC); real time: 15189
;;; Alternate definition of Riemann-tensor
;;; process time: 2670 (2500 RUN + 170 GC); real time: 2671
;|#
;
;|#

;#|
;(define-manifold 'Euclidean-plane Real 2 (up Real Real))
;
;(define-coordinate-system Euclidean-plane 'rectangular (up 'x 'y)
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 2))
;	coords
;	(error "Bad coordinates: real-tuple" coords)))
;  (lambda (point)
;    (if (and (up? point) (fix:= (s:dimension point) 2))
;	point
;	(error "Bad point: real-tuple" point))))
;
;(define 2space-rectangular (Euclidean-plane 'rectangular))
;
;(install-coordinates 2space-rectangular)
;
;(define mr ((2space-rectangular '->point) (up 'x0 'y0)))
;
;(define 2space-metric
;  (+ (square (1form-field->tensor-field dx))
;     (square (1form-field->tensor-field dy))))
;
;(show-time
; (lambda ()
;   (pec (((Ricci-scalar 2space-metric 2space-rectangular)) mr))))
;#| Result:
;0
;;; process time: 8670 (8380 RUN + 290 GC); real time: 8678
;;; with memoized points->coords and vs.
;;; process time: 9030 (8750 RUN + 280 GC); real time: 9077
;;; with derivative idea
;;; #process time: 9040 (8730 RUN + 310 GC); real time: 9050
;|#
;
;|#

;#|
;(define-manifold 'Euclidean-3space Real 3 (up Real Real Real))
;
;(define-coordinate-system Euclidean-3space 'rectangular (up 'x 'y 'z)
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 3))
;	coords
;	(error "Bad coordinates: real-tuple" coords)))
;  (lambda (point)
;    (if (and (up? point) (fix:= (s:dimension point) 3))
;	point
;	(error "Bad point: real-tuple" point))))
;
;(define 3space-rectangular (Euclidean-3space 'rectangular))
;
;(install-coordinates 3space-rectangular)
;
;(define mr ((3space-rectangular '->point) (up 'x0 'y0 'z0)))
;
;(define 3space-metric
;  (+ (square (1form-field->tensor-field dx))
;     (square (1form-field->tensor-field dy))
;     (square (1form-field->tensor-field dz))))
;
;(show-time
; (lambda ()
;   (pec (((Ricci-scalar 3space-metric 3space-rectangular)) mr))))
;#| Result:
;0
;;; process time: 529710 (515240 RUN + 14470 GC); real time: 593282
;;; with memoized points->coords and vs.
;;; process time: 602080 (584950 RUN + 17130 GC); real time: 604133
;;; with derivative idea -- makes no difference here.
;;; process time: 545960 (528250 RUN + 17710 GC); real time: 551045
;|#
;|#

;#|
;(define-manifold 'Euclidean-4space Real 4 (up Real Real Real Real))
;
;(define-coordinate-system Euclidean-4space 'rectangular (up 'x 'y 'z 'w)
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 4))
;	coords
;	(error "Bad coordinates: real-tuple" coords)))
;  (lambda (point)
;    (if (and (up? point) (fix:= (s:dimension point) 4))
;	point
;	(error "Bad point: real-tuple" point))))
;
;(define 4space-rectangular (Euclidean-4space 'rectangular))
;
;(install-coordinates 4space-rectangular)
;
;(define mr ((4space-rectangular '->point) (up 'x0 'y0 'z0 'w0)))
;
;(define 4space-metric
;  (+ (square (1form-field->tensor-field dx))
;     (square (1form-field->tensor-field dy))
;     (square (1form-field->tensor-field dz))
;     (square (1form-field->tensor-field dw))))
;
;(show-time
; (lambda ()
;   (pec (((Ricci-scalar 4space-metric 4space-rectangular)) mr))))
;
;#| Result:
;0
;;; process time: 11750210 (11440840 RUN + 309370 GC); real time: 15491124
;|#
;|#

;#|
;(define-manifold 'spacetime Real 4 (up Real Real Real Real))
;
;(define-coordinate-system spacetime 'rectangular (up 't 'x 'y 'z)
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 4))
;	coords
;	(error "Bad coordinates: spacetime-rectangular" coords)))
;  (lambda (point)
;    (if (and (up? point) (fix:= (s:dimension point) 4))
;	point
;	(error "Bad point: spacetime-rectangular" point))))
;
;(define-coordinate-system spacetime 'spherical (up 't 'r 'theta 'phi)
;  (lambda (coords)
;    (if (and (up? coords) (fix:= (s:dimension coords) 4))
;	(let ((t (ref coords 0))
;	      (r (ref coords 1))
;	      (theta (ref coords 2))
;	      (phi   (ref coords 3)))
;	  (up t
;	      (* r (sin theta) (cos phi)) 
;	      (* r (sin theta) (sin phi)) 
;	      (* r (cos theta))))
;  	(error "Bad coordinates: spacetime-spherical" coords)))
;  (lambda (point)
;    (if (and (up? point) (fix:= (s:dimension point) 4))
;	(let ((t (ref point 0))
;	      (x (ref point 1))
;	      (y (ref point 2))
;	      (z (ref point 3)))
;	  (let ((r (sqrt (+ (square x) (square y) (square z)))))
;	    (up t
;		r
;		(acos (/ z r))
;		(atan y x))))
;	(error "Bad point: spacetime-spherical" point))))
;
;
;(define spacetime-rectangular (spacetime 'rectangular))
;(define spacetime-spherical (spacetime 'spherical))
;
;(install-coordinates spacetime-rectangular)
;(install-coordinates spacetime-spherical)
;
;(define r-event ((spacetime-rectangular '->point) (up 't0 'x0 'y0 'z0)))
;(define s-event ((spacetime-spherical '->point) (up 't0 'r0 'theta0 'phi0)))
;
;(define dT (1form-field->tensor-field dt))
;
;(define dX (1form-field->tensor-field dx))
;(define dY (1form-field->tensor-field dy))
;(define dZ (1form-field->tensor-field dz))
;
;(define dR (1form-field->tensor-field dr))
;(define dTheta (1form-field->tensor-field dtheta))
;(define dPhi (1form-field->tensor-field dphi))
;
;
;
;(define (Minkowski-metric c)
;  (+ (* -1 (square c) (square dT)) (square dX) (square dY) (square dZ)))
;
;(define M-metric (Minkowski-metric 1))
;
;(define V (literal-vector-field 'V spacetime-rectangular))
;
;(pe ((M-metric V V) r-event))
;(+ (* -1 (expt (V^0 (up t0 x0 y0 z0)) 2))
;   (expt (V^1 (up t0 x0 y0 z0)) 2)
;   (expt (V^2 (up t0 x0 y0 z0)) 2)
;   (expt (V^3 (up t0 x0 y0 z0)) 2))
;
;(show-time
; (lambda ()
;   (pec ((tensor-field->coefficient-structure
;	  (Ricci-tensor M-metric spacetime-rectangular)
;	  spacetime-rectangular)
;	 r-event))))
;#| Result:
;(down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
;;;process time: 7216060 (6861050 RUN + 355010 GC); real time: 7272926
;|#
;
;(show-time
; (lambda ()
;   (pec (((Ricci-scalar M-metric spacetime-rectangular)) r-event))))
;
;#| Result:
;0
;;; process time: 11636930 (11323130 RUN + 313800 GC); real time: 11782524
;;; AARGH! about PI hours.
;;; derivative hack (good idea) and memoizing new simplify (bad idea)
;;; process time: 7608820 (7248100 RUN + 360720 GC); real time: 7884958
;;; Alternate definition of Riemann-tensor
;;; process time: 240640 (225720 RUN + 14920 GC); real time: 240716
;|#
;
;(define (Schwarzschild-metric M G c)
;  (let ((a (- 1 (/ (* 2 G M) (* (square c) r)))))
;    (+ (* (square c) a (square dT))
;       (* (/ 1 a) (square dR))
;       (* (square r)
;	  (+ (square dTheta)
;	     (square (* (sin theta) dPhi)))))))
;
;(define S-metric (Schwarzschild-metric 'M 'G 'c))
;
;(show-time
; (lambda ()
;   (pec ((tensor-field->coefficient-structure
;	  (Ricci-tensor S-metric spacetime-spherical)
;	  spacetime-spherical)
;	 s-event))))
;
;(show-time
; (lambda ()
;   (pec (((Ricci-scalar S-metric spacetime-spherical)) s-event))))
;;;; Did not converge after 756 minutes!  No memory problem...
;|#
;#|
;(clear-memoizer-tables)
;
;(define S-metric (Schwarzschild-metric 'M 'G 'c))
;
;(define del
;  (t:covariant-derivative S-metric spacetime-spherical))
;
;(define del_t (del d/dt))
;
;(define del_tT
;  (del_t (vector-field->tensor-field d/dt)))
;
;(define del_tTdt (del_tT dt))
;
;(pe (del_tTdt s-event))
;0
;
;(define v
;  (10tensor-field->vector-field del_tT spacetime-spherical))
;
;(set! *divide-out-terms* #f)
;;Value: #t
;
;(pe ((v (literal-manifold-function 'F spacetime-spherical))
;     s-event))
;(/ (+ (* -1 G M (expt c 2) r_0
;	 (((partial 1) F) (up t_0 r_0 theta_0 phi_0)))
;      (* 2 (expt G 2) (expt M 2)
;	 (((partial 1) F) (up t_0 r_0 theta_0 phi_0))))
;   (* (expt c 2) (expt r_0 3)))
;|#

;#|
;(define (pseudo-Schwarzschild-metric M a)
;  (+ (* a (square dT))
;     (* (/ 1 a) (square dR))
;     (* (square r)
;	(+ (square dTheta)
;	   (square (* (sin theta) dPhi))))))
;|#
