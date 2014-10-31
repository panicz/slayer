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

;;; Assume that we have 4-vectors with the time component in the 0 position

(define ((4-metric g00 g11 g22 g33 g01 g12 g23 g02 g13 g03) vf1 vf2)
  (+ (* g00 (square  ::c) (dt vf1) (dt vf2))
     (* g11 (dx vf1) (dx vf2))
     (* g22 (dy vf1) (dy vf2))
     (* g33 (dz vf1) (dz vf2))
     (* g01
	(+ (* (dt vf1) (dx vf2))
	   (* (dt vf2) (dx vf1))))
     (* g12
	(+ (* (dx vf1) (dy vf2))
	   (* (dx vf2) (dy vf1))))
     (* g23
	(+ (* (dy vf1) (dz vf2))
	   (* (dy vf2) (dz vf1))))
     (* g02
	(+ (* (dt vf1) (dy vf2))
	   (* (dt vf2) (dy vf1))))
     (* g13
	(+ (* (dx vf1) (dz vf2))
	   (* (dx vf2) (dz vf1))))
     (* g03
	(+ (* (dt vf1) (dz vf2))
	   (* (dt vf2) (dz vf1))))))

(define Lorentz-metric
  (4-metric -1 1 1 1 0 0 0 0 0 0))

(define eta Lorentz-metric)

;#|
;(define SR (rectangular 4))
;(instantiate-coordinates SR '(t x y z))
;(define ::c '::c)
;
;(define an-event
;  ((SR '->point) (up 't0 'x0 'y0 'z0)))
;
;(pec ((eta d/dt d/dt) an-event))
;#| Result:
;(* -1 (expt ::c 2))
;|#
;
;(define v1 (literal-vector-field 'v1 SR))
;
;(define v2 (literal-vector-field 'v2 SR))
;
;(pec ((eta v1 v2) an-event))
;#| Result:
;(+ (* -1 (expt ::c 2) (v1^0 (up t0 x0 y0 z0)) (v2^0 (up t0 x0 y0 z0)))
;   (* (v1^1 (up t0 x0 y0 z0)) (v2^1 (up t0 x0 y0 z0)))
;   (* (v1^2 (up t0 x0 y0 z0)) (v2^2 (up t0 x0 y0 z0)))
;   (* (v1^3 (up t0 x0 y0 z0)) (v2^3 (up t0 x0 y0 z0))))
;|#
;|#

;;; For a coordinate system there is an ordinary derivative operator
;;; associated with it.

(define (ordinary-derivative coordinate-system)
  (define (deriv function-of-events)
    (s:map/r (lambda (basis-vector-field)
	       (basis-vector-field function-of-events))
	     (coordinate-system->vector-basis coordinate-system)))
  (make-operator deriv 'ordinary-derivative))

;#|
;;;; 4.2.3
;;;; All coefficients are constant --> ordinary derivatives are zero...
;(pec (((ordinary-derivative SR)
;       (let ((g (4-metric 'g00 'g11 'g22 'g33 'g01 'g12 'g23 'g02 'g13 'g03))
;	     (es (coordinate-system->vector-basis SR)))
;	 ;;This extracts the metric coefficients.
;	 (s:map/r
;	  (lambda (v1)
;	    (s:map/r
;	     (lambda (v2) (g v1 v2))
;	     es))
;	  es)))
;      an-event))
;#| Result:
;(down (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
;      (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
;      (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
;      (down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0)))
;|#
;|#

;#|
;;;; If metric is flat then geodesics are lines.
;
;;;; Proper times
;(instantiate-coordinates the-real-line 'tau)
;
;(define SR-basis (coordinate-system->basis SR))
;
;;;; The Christoffel symbols are:
; 
;(define Lorentz-Gamma			;Christoffel symbols
;  (metric->Christoffel-2 Lorentz-metric
;			 (coordinate-system->basis SR)))
;
;(pec (let ((mu:N->M (compose (SR '->point)
;			     (up (literal-function 't)
;				 (literal-function 'x)
;				 (literal-function 'y)
;				 (literal-function 'z))
;			     (the-real-line '->coords))))
;       (let* ((basis-over-mu (basis->basis-over-map mu:N->M SR-basis))
;	      (1form-basis (basis->1form-basis basis-over-mu))
;	      (Cartan-over-mu
;	       (Christoffel->Cartan-over-map Lorentz-Gamma mu:N->M)))
;	 (s:map/r 
;	  (lambda (w)
;	    ((w
;	      (((covariant-derivative Cartan-over-mu) d/dtau)
;	       ((differential mu:N->M) d/dtau)))
;	     ((the-real-line '->point) 'tau)))
;	  1form-basis))))
;#| Result:
;(up (((expt D 2) t) tau)
;    (((expt D 2) x) tau)
;    (((expt D 2) y) tau)
;    (((expt D 2) z) tau))
;|#
;;;; Looks like a straight line in R^4.
;|#

;#|
;(pec ((Christoffel->symbols Lorentz-Gamma) an-event))
;#| Result:
;(down (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;      (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;      (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
;      (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0)))
;|#
;
;;;; Thus, make simplified version.
;(define Lorentz-Gamma
;  (make-Christoffel
;   (let ((O (lambda x 0)))
;     (down (down (up O O O O) (up O O O O) (up O O O O) (up O O O O))
;	   (down (up O O O O) (up O O O O) (up O O O O) (up O O O O))
;	   (down (up O O O O) (up O O O O) (up O O O O) (up O O O O))
;	   (down (up O O O O) (up O O O O) (up O O O O) (up O O O O))))
;   (coordinate-system->basis SR)))
;
;;;; Now look at curvature
;(for-each
; (lambda (alpha)
;   (for-each
;    (lambda (beta)
;      (for-each
;       (lambda (gamma)
;	 (for-each
;	  (lambda (delta)
;	    (newline)
;	    (pe `(,alpha ,beta ,gamma ,delta))
;	    (pe (((Riemann (Christoffel->Cartan Lorentz-Gamma))
;		  alpha beta gamma delta)
;		 an-event)))
;	  (list d/dt d/dx d/dy d/dz)))
;       (list d/dt d/dx d/dy d/dz)))
;    (list d/dt d/dx d/dy d/dz)))
; (list dt dx dy dz))
;;;; 256 zeros, so Lorentz space is flat.
;|#

;#|
;;;; MTW p205 spherical flat lorentz
;
;(define spherical-Lorentz (rectangular 4))
;(instantiate-coordinates spherical-Lorentz '(t r theta phi))
;
;(define spherical-Lorentz-point 
;  ((spherical-Lorentz '->point) (up 't 'r 'theta 'phi)))
;
;(define spherical-Lorentz-basis
;  (coordinate-system->basis spherical-Lorentz))
;
;(define (spherical-Lorentz-metric v1 v2)
;  (+ (* -1 (square c) (* (dt v1) (dt v2)))
;     (* (dr v1) (dr v2))
;     (* (square r)
;	(+ (* (dtheta v1) (dtheta v2))
;	   (* (square (sin theta))
;	      (* (dphi v1) (dphi v2)))))))
;
;(define foo
;  ((Christoffel->symbols
;    (metric->connection-2 spherical-Lorentz-metric
;			  spherical-Lorentz-basis))
;   spherical-Lorentz-point))
;
;(pe foo)
;#| Result:
;(down
; (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0) (up 0 0 0 0))
; (down (up 0 0 0 0) (up 0 0 0 0) (up 0 0 (/ 1 r) 0) (up 0 0 0 (/ 1 r)))
; (down (up 0 0 0 0) (up 0 0 (/ 1 r) 0) (up 0 (* -1 r) 0 0)
;       (up 0 0 0 (/ (cos theta) (sin theta))))
; (down (up 0 0 0 0)
;       (up 0 0 0 (/ 1 r))
;       (up 0 0 0 (/ (cos theta) (sin theta)))
;       (up 0
;	   (* -1 r (expt (sin theta) 2))
;	   (* -1 (sin theta) (cos theta))
;	   0)))
;|#
;
;;;; Thus, make simplified version.
;
;(define spherical-Lorentz-Gamma
;  (make-Christoffel
;   (let ((O (lambda x 0)))
;     (down
;      (down (up O O O O) (up O O O O) (up O O O O) (up O O O O))
;      (down (up O O O O) (up O O O O) (up O O (/ 1 r) O) (up O O O (/ 1 r)))
;      (down (up O O O O) (up O O (/ 1 r) O) (up O (* -1 r) O O)
;	    (up O O O (/ (cos theta) (sin theta))))
;      (down (up O O O O)
;	    (up O O O (/ 1 r))
;	    (up O O O (/ (cos theta) (sin theta)))
;	    (up O
;		(* -1 r (expt (sin theta) 2))
;		(* -1 (sin theta) (cos theta))
;		O))))
;   (coordinate-system->basis SR)))
;
;;;; Now look at curvature
;(for-each
; (lambda (alpha)
;   (for-each
;    (lambda (beta)
;      (for-each
;       (lambda (gamma)
;	 (for-each
;	  (lambda (delta)
;	    (newline)
;	    (pe `(,alpha ,beta ,gamma ,delta))
;	    (pe (((Riemann (Christoffel->Cartan spherical-Lorentz-Gamma))
;		  alpha beta gamma delta)
;		 spherical-Lorentz-point)))
;	  (list d/dt d/dr d/dtheta d/dphi)))
;       (list d/dt d/dr d/dtheta d/dphi)))
;    (list d/dt d/dr d/dtheta d/dphi)))
; (list dt dr dtheta dphi))
;;;; 256 zeros, so Lorentz space is flat, even in spherical coords.
;|#

(define ((metric-over-map mu:N->M) g-on-M)
  (lambda (v1 v2)
    (lambda (n)
      ((g-on-M
	(vector-field-over-map->vector-field v1 n)
	(vector-field-over-map->vector-field v2 n))
       (mu:N->M n)))))

;#|
;;;; Twin paradox: green Schutz p28
;
;;;; In Artemis coordinates, path of Diana has two segments.  vdiana=.96c
;
;(instantiate-coordinates the-real-line 't_artemis)
;
;(define seg1
;  (compose (SR '->point)
;	   (lambda (t) (up t (* .96 ::c t) 0 0))
;	   (the-real-line '->coords)))
;
;;;; After 25 years, Diana gets to the turning point.
;
;(define turning-point
;  (seg1 ((the-real-line '->point) 25)))
;
;(pec turning-point)
;#| Result:
;(up 25 (* 24. ::c) 0 0)
;|#
;
;;;; The tangent vector to seg1 (over the map seg1)
;
;(define T1
;  ((differential seg1) d/dt_artemis))
;
;
;;;; The turning point should be the integral of the tangent over 25
;;;; years.  Because seg1 is a straight line, this can be evaluated
;;;; anytime.
;
;(pec (((* T1 25) identity) ((the-real-line '->point) 1)))
;#| Result:
;(up 25 (* 24. ::c) 0 0)
;|#
;
;(define seg2
;  (compose (SR '->point)
;	   (lambda (t)
;	     (let ((t2 (- t 25))) 
;	       (+ (up t2 (* -.96 ::c t2) 0 0)
;		  ((SR '->coords) turning-point))))
;	   (the-real-line '->coords)))
;
;
;(define end-point
;  (seg2 ((the-real-line '->point) 50)))
;
;(pec end-point)
;#| Result:
;(up 50 0 0 0)
;|#
;
;(define T2
;  ((differential seg2) d/dt_artemis))
;
;(pec
; (* (sqrt
;     (- ((((metric-over-map seg1) Lorentz-metric) T1 T1)
;	 ((the-real-line '->point) 5))))
;    25))
;#| Result:
;(* 7.000000000000001 ::c)
;|#
;
;(pec
; (+ (* (sqrt
;	(- ((((metric-over-map seg1) Lorentz-metric) T1 T1)
;	    ((the-real-line '->point) 5))))
;       25)
;    (* (sqrt
;	(- ((((metric-over-map seg2) Lorentz-metric) T2 T2)
;	    ((the-real-line '->point) 5))))
;       25)))
;#| Result:
;(* 14.000000000000002 ::c)
;|#
;
;;;; So Diana has only aged 14 years while Artemis has aged 50 years.
;;;; This is 1/2 of the computation.  We must also compute Artemis's
;;;; proper aging from Diana's coordinates.
;|#

(define ((covariant-field-over-map mu:N->M) w-on-M)
  (lambda vectors-over-map
    ;;(assert (= (length vectors-over-map) (get-rank w-on-M)))
    (lambda (n)
      ((apply w-on-M
	      (map (lambda (V-over-mu)
		     (vector-field-over-map->vector-field V-over-mu n))
		   vectors-over-map))
       (mu:N->M n)))))

(define ((pullback-covariant mu:N->M) omega-on-M)
 (lambda vectors-on-N
   (apply ((covariant-field-over-map mu:N->M) omega-on-M)
	  (map (differential mu:N->M) vectors-on-N))))

;#|
;(define ((pullback-metric mu:N->M) g-on-M)
;  (lambda (v1 v2)
;    (((2-down-over-map mu:N->M) g-on-M)
;     ((differential mu:N->M) v1)
;     ((differential mu:N->M) v2))))
;|#

;#|
;;;; Alternative Twin paradox: green Schutz p28
;
;;;; In Artemis coordinates, path of Diana has two segments.  vdiana=.96c
;
;(instantiate-coordinates the-real-line 't_artemis)
;
;(define seg1
;  (compose (SR '->point)
;	   (lambda (t) (up t (* .96 ::c t) 0 0))
;	   (the-real-line '->coords)))
;
;;;; After 25 years, Diana gets to the turning point.
;
;(define turning-point
;  (seg1 ((the-real-line '->point) 25)))
;
;(pec turning-point)
;#| Result:
;(up 25 (* 24. ::c) 0 0)
;|#
;
;;;; The tangent vector to seg1 (over the map seg1)
;
;(define T1
;  ((differential seg1) d/dt_artemis))
;
;
;;;; The turning point should be the integral of the tangent over 25
;;;; years.  Because seg1 is a straight line, this can be evaluated
;;;; anytime.
;
;(pec (((* T1 25) identity) ((the-real-line '->point) 1)))
;#| Result:
;(up 25 (* 24. ::c) 0 0)
;|#
;
;(define seg2
;  (compose (SR '->point)
;	   (lambda (t)
;	     (let ((t2 (- t 25))) 
;	       (+ (up t2 (* -.96 ::c t2) 0 0)
;		  ((SR '->coords) turning-point))))
;	   (the-real-line '->coords)))
;
;
;(define end-point
;  (seg2 ((the-real-line '->point) 50)))
;
;(pec end-point)
;#| Result:
;(up 50 0 0 0)
;|#
;
;(pec
; (+ (* (sqrt
;	(- ((((pullback-covariant seg1) Lorentz-metric)
;	     d/dt_artemis d/dt_artemis)
;	    ((the-real-line '->point) 5))))
;       25)
;    (* (sqrt
;	(- ((((pullback-covariant seg2) Lorentz-metric)
;	     d/dt_artemis d/dt_artemis)
;	    ((the-real-line '->point) 5))))
;       25)))
;#| Result:
;(* 14.000000000000002 ::c)
;|#
;
;;;; So Diana has only aged 14 years while Artemis has aged 50 years.
;;;; This is 1/2 of the computation.  We must also compute Artemis's
;;;; proper aging from Diana's coordinates.
;|#

;;; Lorentz boosts

(define ((Boost-X velocity) vector-field)
  (let ((vt (dt vector-field))
	(vx (dx vector-field))
	(vy (dy vector-field))
	(vz (dz vector-field)))
    (coordinate-boost velocity vt vx
		      (lambda (vtp vxp)
			(+ (* vtp d/dt)
			   (* vxp d/dx)
			   (* vy d/dy)
			   (* vz d/dz))))))

(define (coordinate-boost velocity delta-t delta-x cont)
  ;; cont = (lambda (delta-t-prime delta-x-prime) ...)
  (let ((gamma (/ 1 (sqrt (- 1 (square (/ velocity ::c)))))))
    (let ((delta-t-prime
	   (* gamma (- delta-t (* (/ velocity (square ::c)) delta-x))))
	  (delta-x-prime 
	   (* gamma (- delta-x (* velocity delta-t)))))
      (cont delta-t-prime delta-x-prime))))

;#|
;(define an-event
;  ((SR '->point) (up 't0 'x0 'y0 'z0)))
;
;(define v1 (literal-vector-field 'v1 SR))
;
;(set! *divide-out-terms* #f)
;
;(pec ((dt ((Boost-X 'v) v1)) an-event))
;#| Result:
;(/ (+ (* (expt ::c 2) (v1^0 (up t0 x0 y0 z0)))
;      (* -1 v (v1^1 (up t0 x0 y0 z0))))
;   (* ::c (sqrt (+ (expt ::c 2) (* -1 (expt v 2))))))
;|#
;
;(pec ((dx ((Boost-X 'v) v1)) an-event))
;#| Result:
;(/ (+ (* -1 ::c v (v1^0 (up t0 x0 y0 z0)))
;      (* ::c (v1^1 (up t0 x0 y0 z0))))
;   (sqrt (+ (expt ::c 2) (* -1 (expt v 2)))))
;|#
;
;(pec ((dy ((Boost-X 'v) v1)) an-event))
;#| Result:
;(v1^2 (up t0 x0 y0 z0))
;|#
;
;(pec ((dz ((Boost-X 'v) v1)) an-event))
;#| Result:
;(v1^3 (up t0 x0 y0 z0))
;|#
;|#

(define ((v->v-over-map mu:N->M) v-on-M->v-on-M)
  (lambda (v)
    (procedure->vector-field
     (lambda (f)
       (lambda (n)
	 (((v-on-M->v-on-M (vector-field-over-map->vector-field v n)) f)
	  (mu:N->M n))))
     'foo))))

;#|
;;;; The velocity transformation law.
;
;(define SR (rectangular 4))
;(instantiate-coordinates SR '(t x y z))
;(define ::c '::c)
;
;(define an-event
;  ((SR '->point) (up 't0 'x0 'y0 'z0)))
;
;
;(instantiate-coordinates the-real-line 'tau)
;
;
;;;; Consider a segment of a uniform motion of velocity v in the +x
;;;; direction.
;
;(define (seg v)
;  (compose (SR '->point)
;	   (lambda (t) (up t (* v t) 0 0))
;	   (the-real-line '->coords)))
;
;
;;;; The tangent to the worldline is
;
;(define (T v)
;  ((differential (seg v)) d/dtau))
;
;(define SR-1form-basis-over-map
;  (basis->1form-basis
;   (basis->basis-over-map (seg 'v1) SR-basis)))
;
;(define boosted-tangent-to-map
;  (((v->v-over-map (seg 'v1)) (Boost-X (- 'v2)))
;   ((differential (seg 'v1)) d/dtau)))
;
;(pec ((/ (((component 1) SR-1form-basis-over-map)
;	  boosted-tangent-to-map)
;	 (((component 0) SR-1form-basis-over-map)
;	  boosted-tangent-to-map))
;      ((the-real-line '->point) 'tau)))
;#| Result:
;(/ (+ (* (expt ::c 2) v1) (* (expt ::c 2) v2))
;   (+ (expt ::c 2) (* v1 v2)))
;|#
;;;; This is the addition of velocities formula.  Ugh!
;
;|#