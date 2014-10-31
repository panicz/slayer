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

(define SR (rectangular 4))
(instantiate-coordinates SR '(t x y z))
(define SR-basis (coordinate-system->basis SR))
(define an-event ((SR '->point) (up 't0 'x0 'y0 'z0)))

(define ((g-Lorentz c) u v)
  (+ (* (dx u) (dx v))
     (* (dy u) (dy v))
     (* (dz u) (dz v))
     (* -1 (square c) (dt u) (dt v))))

(define (SR-sharp c)
  (sharpen (g-Lorentz c) SR-basis an-event))

(define (SR-star c)
  (* c (star (SR-sharp c) SR-basis)))

(define (SR-star c)
  (star (SR-sharp c) SR-basis -1))

;;; Vector fields E, B

(define ((Faraday c) E B)
  (let ((Ex (dx E)) (Ey (dy E)) (Ez (dz E))
	(Bx (dx B)) (By (dy B)) (Bz (dz B)))
    (+ (* Ex c (wedge dx dt))
       (* Ey c (wedge dy dt))
       (* Ez c (wedge dz dt))
       (* Bx (wedge dy dz))
       (* By (wedge dz dx))
       (* Bz (wedge dx dy)))))


(pec ((((SR-star 'c)
	(* (literal-manifold-function 'Bx SR)
	   (wedge dy dz)))
       d/dx
       (* (/ 1 'c) d/dt))
      an-event))
;#| Result:
;(* -1 (Bx (up t0 x0 y0 z0)))
;|#

(define E
  (+ (* (literal-manifold-function 'Ex SR) d/dx)
     (* (literal-manifold-function 'Ey SR) d/dy)
     (* (literal-manifold-function 'Ez SR) d/dz)))

(define B
  (+ (* (literal-manifold-function 'Bx SR) d/dx)
     (* (literal-manifold-function 'By SR) d/dy)
     (* (literal-manifold-function 'Bz SR) d/dz)))

(define ((*Faraday c) E B)
  (let ((Ex (dx E)) (Ey (dy E)) (Ez (dz E))
        (Bx (dx B)) (By (dy B)) (Bz (dz B)))
    (+ (* -1 Bx c (wedge dx dt))
       (* -1 By c (wedge dy dt))
       (* -1 Bz c (wedge dz dt))
       (* Ex (wedge dy dz))
       (* Ey (wedge dz dx))
       (* Ez (wedge dx dy)))))

;;; This is not MTW.  The scale factor "c" below fixes the problem
;;; that the volume element is not dx^dy^dz^dt but rather
;;; dx^dy^dz^cdt.

(pec (- ((((SR-star 'c)
	   ((Faraday 'c) E B))
	  (literal-vector-field 'u SR)
	  (literal-vector-field 'v SR))
	 an-event)
	((((*Faraday 'c) E B)
	  (literal-vector-field 'u SR)
	  (literal-vector-field 'v SR))
	 an-event)))
;#| Result:
;0
;|#
;;; Defining the 4-current density J.

;;; Charge density is a manifold function.  Current density is a
;;; vector field having only spatial components.

(define ((J c) charge-density current-density)
  (let ((Jx (dx current-density))
	(Jy (dy current-density))
	(Jz (dz current-density)))
    (- (+ (* Jx dx) (* Jy dy) (* Jz dz))
       (* charge-density c dt))))

(define I
  (* (/ 1 'c)
     (+ (* (literal-manifold-function 'Ix SR) d/dx)
	(* (literal-manifold-function 'Iy SR) d/dy)
	(* (literal-manifold-function 'Iz SR) d/dz))))

(define rho (literal-manifold-function 'rho SR))

(pec ((((SR-star 'c) ((J 'c) rho I))
       d/dx d/dy d/dz)
      an-event))
;#| Result:
;(rho (up t0 x0 y0 z0))
;|#

(pec ((((SR-star 'c) ((J 'c) rho I))
       (* (/ 1 'c) d/dt) d/dy d/dz)
      an-event))
;#| Result:
;(/ (* -1 (Ix (up t0 x0 y0 z0))) c)
;|#

(pec ((((SR-star 'c) ((J 'c) rho I))
       (* (/ 1 'c) d/dt) d/dz d/dx)
      an-event))
;#| Result:
;(/ (* -1 (Iy (up t0 x0 y0 z0))) c)
;|#

(pec ((((SR-star 'c) ((J 'c) rho I))
       (* (/ 1 'c) d/dt) d/dx d/dy)
      an-event))
;#| Result:
;(/ (* -1 (Iz (up t0 x0 y0 z0))) c)
;|#

;;; Maxwell's equations in the form language are:
;;; dF=0, d(*F)=4pi *J
  
;;; div B = 0
(pec (((d ((Faraday 'c) E B)) d/dx d/dy d/dz) an-event))

;#| Result: 
;(+ (((partial 1) Bx) (up t0 x0 y0 z0))
;   (((partial 2) By) (up t0 x0 y0 z0))
;   (((partial 3) Bz) (up t0 x0 y0 z0)))
;|#



;;; curl E = -1/c dB/dt

(pec (((d ((Faraday 'c) E B)) d/dt d/dy d/dz) an-event))
;#| Result:
;(+ (* c (((partial 2) Ez) (up t0 x0 y0 z0)))
;   (* -1 c (((partial 3) Ey) (up t0 x0 y0 z0)))
;   (((partial 0) Bx) (up t0 x0 y0 z0)))
;|#

(pec (((d ((Faraday 'c) E B)) d/dt d/dz d/dx) an-event))
;#| Result:
;(+ (* c (((partial 3) Ex) (up t0 x0 y0 z0)))
;   (* -1 c (((partial 1) Ez) (up t0 x0 y0 z0)))
;   (((partial 0) By) (up t0 x0 y0 z0)))
;|#

(pec (((d ((Faraday 'c) E B)) d/dt d/dx d/dy) an-event))
;#| Result:
;(+ (* c (((partial 1) Ey) (up t0 x0 y0 z0)))
;   (* -1 c (((partial 2) Ex) (up t0 x0 y0 z0)))
;   (((partial 0) Bz) (up t0 x0 y0 z0)))
;|#

;;; div E = 4pi rho

(pec (((- (d ((SR-star 'c) ((Faraday 'c) E B)))
	  (* '4pi ((SR-star 'c) ((J 'c) rho I))))
       d/dx d/dy d/dz)
      an-event))
;#| Result:
;(+ (* -1 4pi (rho (up t0 x0 y0 z0)))
;   (((partial 1) Ex) (up t0 x0 y0 z0))
;   (((partial 2) Ey) (up t0 x0 y0 z0))
;   (((partial 3) Ez) (up t0 x0 y0 z0)))
;|#



;;; curl B = 1/c dE/dt + 4pi I

(pec (((- (d ((SR-star 'c) ((Faraday 'c) E B)))
	  (* '4pi ((SR-star 'c) ((J 'c) rho I))))
       d/dt d/dy d/dz)
      an-event))
;#| Result:
;(+ (* 4pi (Ix (up t0 x0 y0 z0)))
;   (* -1 c (((partial 2) Bz) (up t0 x0 y0 z0)))
;   (* c (((partial 3) By) (up t0 x0 y0 z0)))
;   (((partial 0) Ex) (up t0 x0 y0 z0)))
;|#

(pec (((- (d ((SR-star 'c) ((Faraday 'c) E B)))
	  (* '4pi ((SR-star 'c) ((J 'c) rho I))))
       d/dt d/dz d/dx)
      an-event))
;#| Result:
;(+ (* 4pi (Iy (up t0 x0 y0 z0)))
;   (* -1 c (((partial 3) Bx) (up t0 x0 y0 z0)))
;   (* c (((partial 1) Bz) (up t0 x0 y0 z0)))
;   (((partial 0) Ey) (up t0 x0 y0 z0)))
;|#

(pec (((- (d ((SR-star 'c) ((Faraday 'c) E B)))
	  (* '4pi ((SR-star 'c) ((J 'c) rho I))))
       d/dt d/dx d/dy)
      an-event))
;#| Result:
;(+ (* 4pi (Iz (up t0 x0 y0 z0)))
;   (* -1 c (((partial 1) By) (up t0 x0 y0 z0)))
;   (* c (((partial 2) Bx) (up t0 x0 y0 z0)))
;   (((partial 0) Ez) (up t0 x0 y0 z0)))
;|#
