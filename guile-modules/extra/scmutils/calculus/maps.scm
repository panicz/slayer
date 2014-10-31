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

;;;; Maps between manifolds.

;;; If we have a function on a manifold M and a map from manifold N to
;;; manifold M we can define a function on N:

(define ((pullback-function mu:N->M) f-on-M)
  (compose f-on-M mu:N->M))


;;; If we have an inverse map mu^-1:M->N, we can push a function
;;; on N forward through the map:

(define ((pushforward-function mu^-1:M->N) f-on-N)
  (compose f-on-N mu^-1:M->N))



;;; The map between manifolds induces various ways to transport
;;; vectors from one manifold to another.  The simplest of these is
;;; the differential.

;;; The differential of a function mu:N->M from N to M takes a vector
;;; field on the source manifold N to a vector field-like operator on
;;; the target manifold M.  This results in a vector field over the
;;; map mu:N->M.  The result takes directional derivatives of
;;; functions defined on M, at points of M that are targets of points
;;; of N.

(define ((differential-of-map mu:N->M) v-on-N)
  (define (v-on-M g-on-M)
    (v-on-N (compose g-on-M mu:N->M)))
  (assert (vector-field? v-on-N))
  (procedure->vector-field v-on-M
			   `((d ,(diffop-name mu:N->M))
			     ,(diffop-name v-on-N))))


(define differential differential-of-map)

;;; For a long time we were confused between the concepts of
;;; differential and pushforward.  The resolution seems to be that the
;;; differential takes the manifold position in the source manifold
;;; and the pushforward takes the manifold position in the target
;;; manifold of the map.  So the pushforward needs an inverse map to
;;; define it and so the pushforward is not a very useful idea.

(define ((pushforward-vector mu:N->M mu^-1:M->N) v-on-N)
  ;; Assume (compose mu^-1:M->N mu:N->M) = identity
  (procedure->vector-field
   (lambda (f)
     (compose (((differential mu:N->M) v-on-N) f) mu^-1:M->N))
   `((pushforward ,(diffop-name mu:N->M))
     ,(diffop-name v-on-N))))

(define (literal-manifold-map name source target)
  (let ((n (source 'dimension))
	(m (target 'dimension)))
    (let ((sig (if (fix:= n 1) (-> Real Real) (-> (UP* Real n) Real))))
      (compose (target '->point)
	       (s:generate m 'up
			   (lambda (i)
			     (literal-function
			      (string->symbol
			       (string-append (symbol->string name)
					      (number->string i)))
			      sig)))
	       (source '->coords)))))


;#|
;;;; Explanation of the connection between the basis forms and the
;;;; differentials of coordinate functions.
;
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;
;(define cylindrical (polar/cylindrical 3))
;(instantiate-coordinates cylindrical '(r theta z))
;
;(define counter-clockwise (- (* x d/dy) (* y d/dx)))
;(define outward (+ (* x d/dx) (* y d/dy)))
;
;(define mr ((R3 '->point) (up 'x0 'y0 'z0)))
;(define mp ((cylindrical '->point) (up 'r0 'theta0 'z0)))
;
;(pec ((dx counter-clockwise) mr))
;#| Result:
;(* -1 y0)
;|#
;
;(pec ((((differential x) counter-clockwise) identity) mr))
;#| Result:
;(* -1 y0)
;|#
;
;(pec ((dx outward) mr))
;#| Result:
;x0
;|#
;
;(pec ((((differential x) outward) identity) mr))
;#| Result:
;x0
;|#
;
;(pec ((dy counter-clockwise) mr))
;#| Result:
;x0
;|#
;
;(pec ((((differential y) counter-clockwise) identity) mr))
;#| Result:
;x0
;|#
;
;(pec ((dy outward) mr))
;#| Result:
;y0
;|#
;
;(pec ((((differential y) outward) identity) mr))
;#| Result:
;y0
;|#
;
;(pec ((dr counter-clockwise) mp))
;#| Result:
;0
;|#
;
;(pec ((((differential r) counter-clockwise) identity) mp))
;#| Result:
;0
;|#
;
;(pec ((dr outward) mp))
;#| Result:
;r0
;|#
;
;(pec ((((differential r) outward) identity) mp))
;#| Result:
;r0
;|#
;
;(pec ((dtheta counter-clockwise) mp))
;#| Result:
;1
;|#
;
;(pec ((((differential theta) counter-clockwise) identity) mp))
;#| Result:
;1
;|#
;
;(pe ((dtheta outward) mp))
;#| Result:
;0
;|#
;
;(pec ((((differential theta) outward) identity) mp))
;#| Result:
;0
;|#
;|#

;#|
;(define R2 (rectangular 2))
;(instantiate-coordinates R2 '(x y))
;(define R2-point ((R2 '->point) (up 'x0 'y0)))
;
;(instantiate-coordinates the-real-line 't)
;(define rp ((the-real-line '->point) 'tau))
;
;(define mu (literal-manifold-map 'mu the-real-line R2))
;
;(define f (literal-scalar-field 'f R2))
;
;(pec ((((differential mu) d/dt) f) rp))
;#| Result:
;(+ (* (((partial 1) f) (up (mu0 tau) (mu1 tau))) ((D mu1) tau))
;   (* (((partial 0) f) (up (mu0 tau) (mu1 tau))) ((D mu0) tau)))
;|#
;
;(pec ((dx ((differential mu) d/dt)) rp))
;#| Result:
;((D mu0) tau)
;|#
;    
;(pec ((dy ((differential mu) d/dt)) rp))
;#| Result:
;((D mu1) tau)
;|#
;
;
;;;; but this is a fraud... Note that if we have a non-coordinate basis
;;;; the dual does not work on the transported vector.
;
;(define e0 (literal-vector-field 'e0 R2))
;
;(define e1 (literal-vector-field 'e1 R2))
;
;(define edual (vector-basis->dual (down e0 e1) R2))
;
;(pec (((ref edual 0) ((differential mu) d/dt)) rp))
;;Bad point: rectangular #(tau)
;
;;;; However, if we kludge the correct argument it gives the expected
;;;; answer.
;
;(pec (((ref edual 0)
;       (procedure->vector-field
;	(lambda (f)
;	  (lambda (m)
;	    ((((differential mu) d/dt) f)
;	     ((the-real-line '->point) 't))))
;	'foo))
;      R2-point))
;#| Result:
;(/ (+ (* -1 (e1^1 (up x0 y0)) ((D mu0) t))
;      (* (e1^0 (up x0 y0)) ((D mu1) t)))
;   (+ (* -1 (e0^0 (up x0 y0)) (e1^1 (up x0 y0)))
;      (* (e0^1 (up x0 y0)) (e1^0 (up x0 y0)))))
;|#
;
;(instantiate-coordinates the-real-line 't)
;
;(define unit-sphere (S2 1))
;
;;;; General path on the sphere
;
;(define mu
;  (compose (unit-sphere '->point)
;	   (up (literal-function 'theta)
;	       (literal-function 'phi))
;	   (the-real-line '->coords)))
;
;(define f
;  (compose (literal-function 'f (-> (UP Real Real) Real))
;	   (unit-sphere '->coords)))
;
;
;(pec ((((differential mu) d/dt) f)
;      ((the-real-line '->point) 't)))
;#| Result:
;(+ (* ((D theta) t) (((partial 0) f) (up (theta t) (phi t))))
;   (* (((partial 1) f) (up (theta t) (phi t))) ((D phi) t)))
;|#
;|#

;;; Another way to obtain a vector field over a map is to start with a
;;; vector field on the target manifold.  Given a vector field v-on-M
;;; and a map mu:N->M, we obtain a vector field over the map.  This is
;;; a thing like a vector field on M restricted to the targets of
;;; mu:N->M and evaluated on points of N.

(define ((vector-field-over-map mu:N->M) v-on-M)
  (procedure->vector-field
   (lambda (f-on-M)
     (compose (v-on-M f-on-M)
	      mu:N->M))
   `((vector-field-over-map ,(diffop-name mu:N->M))
     ,(diffop-name v-on-M))))

;;; A form field can also be transported across a map.  Given a form
;;; field on M and a map mu:N->M, we obtain a thing like a form field
;;; on M that measures vectors over the map mu:N->M and is evaluated
;;; on points of N.
;#|
;(define ((1form-field-over-map mu:N->M) w-on-M)
;  (procedure->1form-field
;   (lambda (V-over-mu)
;     (lambda (n)
;       ((w-on-M
;	 (vector-field-over-map->vector-field V-over-mu n))
;	(mu:N->M n))))
;   `((1form-field-over-map ,(diffop-name mu:N->M))
;     ,(diffop-name w-on-M))))
;|#

(define ((form-field-over-map mu:N->M) w-on-M)
  (procedure->nform-field
   (lambda vectors-over-map
     (assert (= (length vectors-over-map) (get-rank w-on-M)))
     (lambda (n)
       ((apply w-on-M
	       (map (lambda (V-over-mu)
		      (vector-field-over-map->vector-field V-over-mu n))
		    vectors-over-map))
	(mu:N->M n))))
   `((form-field-over-map ,(diffop-name mu:N->M))
     ,(diffop-name w-on-M))
   (get-rank w-on-M)))


;;; This helper has no clear meaning.

(define (vector-field-over-map->vector-field V-over-mu n) 
  (procedure->vector-field
   (lambda (f)
     (lambda (m)
       ;;(assert (= m (mu:N->M n)))
       ((V-over-mu f) n)))
   `(vector-field-over-map->vector-field
     ,(diffop-name V-over-mu))))

(define (basis->basis-over-map mu:N->M basis-on-M)
  (let ((vector-basis-on-M (basis->vector-basis basis-on-M))
	(dual-basis-on-M (basis->1form-basis basis-on-M)))
    (make-basis
     (s:map/r (vector-field-over-map mu:N->M)
	      vector-basis-on-M)
     (s:map/r (form-field-over-map mu:N->M)
	      dual-basis-on-M))))

;#|
;(instantiate-coordinates the-real-line 't)
;
;(define unit-sphere (S2 1))
;(instantiate-coordinates unit-sphere '(theta phi))
;(define f (literal-scalar-field 'f unit-sphere))
;
;;;; General path on the sphere
;(define mu
;  (compose (unit-sphere '->point)
;	   (up (literal-function 'theta)
;	       (literal-function 'phi))
;	   (the-real-line '->coords)))
;
;(pec ((((vector-field-over-map mu) d/dtheta) f)
;      ((the-real-line '->point) 't)))
;#| Result:
;(((partial 0) f) (up (theta t) (phi t)))
;|#
;
;(pec ((((form-field-over-map mu) dtheta)
;       ((differential mu) d/dt))
;      ((the-real-line '->point) 't)))
;#| Result:
;((D theta) t)
;|#
;
;(define foo
;  (basis->basis-over-map mu
;			 (coordinate-system->basis unit-sphere)))
;
;(pec
; (((basis->1form-basis foo)
;   (basis->vector-basis foo))
;  ((the-real-line '->point) 't)))
;#| Result:
;(up (down 1 0) (down 0 1))
;|#
;
;(pec
; (((basis->1form-basis foo)
;   ((differential mu) d/dt))
;  ((the-real-line '->point) 't)))
;#| Result:
;(up ((D theta) t) ((D phi) t))
;|#
;|#

;;; The following helper is used to define pullbacks of forms.
;#|
;(define ((effective-pushforward mu:N->M n) v-on-N)
;  (procedure->vector-field
;   (lambda (g-on-M)
;     (lambda (m)
;       ;;(assert (= m (mu:N->M n)))
;       ((((differential mu:N->M) v-on-N) 
;	 g-on-M) 
;	n)))
;   `((differential ,(diffop-name mu:N->M))
;     ,(diffop-name v-on-N))))
;
;;;; We extend the pullback to 1-forms:
;
;(define ((pullback-1form mu:N->M) omega-on-M)
;  (procedure->1form-field
;   (lambda (v-on-N)
;     (lambda (n)
;       ((omega-on-M
;	 ((effective-pushforward mu:N->M n) v-on-N))
;	(mu:N->M n))))
;   `((pullback ,(diffop-name mu:N->M))
;     ,(diffop-name omega-on-M))))
;
;(define ((pullback-1form mu:N->M) omega-on-M)
;  (procedure->1form-field
;   (lambda (X-on-N)
;     (((form-field-over-map mu:N->M) omega-on-M)
;      ((differential mu:N->M) X-on-N)))
;   `((pullback ,(diffop-name mu:N->M))
;     ,(diffop-name omega-on-M))))
;
;(define ((pullback mu:N->M) omega-on-M)
;  (let ((k (get-rank omega-on-M)))
;    (if (= k 0)
;	((pullback-function mu:N->M) omega-on-M)
;	(let ((the-pullback
;	       (lambda args
;		 (assert (fix:= (length args) k))
;		 (lambda (n)
;		   ((apply omega-on-M
;			   (map (effective-pushforward mu:N->M n)
;				args))
;		    (mu:N->M n))))))
;	  (procedure->nform-field the-pullback
;				  `((pullback ,(diffop-name mu:N->M))
;				    ,(diffop-name omega-on-M))
;				  k)))))
;|#
;;; The general case
;;; ((mu^* w) v) = w (mu_* v) = (w^mu ((d mu) v))

(define ((pullback-form mu:N->M) omega-on-M)
  (let ((k (get-rank omega-on-M)))
    (if (= k 0)
	((pullback-function mu:N->M) omega-on-M)
	(procedure->nform-field
	 (lambda vectors-on-N
	   (apply ((form-field-over-map mu:N->M) omega-on-M)
		  (map (differential mu:N->M)
		       vectors-on-N)))
	 `((pullback ,(diffop-name mu:N->M))
	   ,(diffop-name omega-on-M))
	 k))))

(define (pullback-vector-field mu:N->M mu^-1:M->N)
  (pushforward-vector mu^-1:M->N mu:N->M))

(define* ((pullback mu:N->M #:optional mu^-1:M->N) thing)
  (if (vector-field? thing)
      (if (default-object? mu^-1:M->N)
	  (error "Pullback vector needs inverse map")
	  ((pullback-vector-field mu:N->M mu^-1:M->N) thing))
      ((pullback-form mu:N->M) thing)))

;#|
;(pec (((pullback mu) f)
;      ((the-real-line '->point) 't)))
;#| Result:
;(f (up (theta t) (phi t)))
;|#
;
;(pec
;  ((((pullback mu) dtheta) d/dt)
;   ((the-real-line '->point) 't)))
;#| Result:
;((D theta) t)
;|#
;
;(pec
;  ((((pullback mu)
;     (wedge dtheta dphi))
;    d/dt d/dt)
;   ((the-real-line '->point) 't)))
;#| Result:
;0
;|#
;
;|#

;#|
;
;(define cylindrical (polar/cylindrical 3))
;(instantiate-coordinates cylindrical '(r theta zeta))
;
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;
;(define mu
;  (compose
;   (cylindrical '->point)
;   (up (literal-function 'mu^r
;			 (-> (UP Real Real Real) Real))
;       (literal-function 'mu^theta
;			 (-> (UP Real Real Real) Real))
;       (literal-function 'mu^zeta
;			 (-> (UP Real Real Real) Real)))
;   (R3 '->coords)))
;
;(pec
;  ((((pullback mu) dtheta) d/dx)
;   ((R3 '->point) (up 'x 'y 'z))))
;#| Result:
;(((partial 0) mu^theta) (up x y z))
;|#
;
;(pec
;  ((((pullback mu) dtheta) d/dy)
;   ((R3 '->point) (up 'x 'y 'z))))
;#| Result:
;(((partial 1) mu^theta) (up x y z))
;|#
;
;(pec
;  ((((pullback mu) dr) d/dx)
;   ((R3 '->point) (up 'x 'y 'z))))
;#| Result:
;(((partial 0) mu^r) (up x y z))
;|#
;
;(pec
;  ((((pullback mu) dr) d/dy)
;   ((R3 '->point) (up 'x 'y 'z))))
;#| Result:
;(((partial 1) mu^r) (up x y z))
;|#
;
;(pec
; ((((pullback mu)
;    (wedge dr dtheta))
;   d/dx d/dy)
;  ((R3 '->point)
;   (up 'x 'y 'z))))
;#| Result:
;(+ (* (((partial 1) mu^theta) (up x y z))
;      (((partial 0) mu^r) (up x y z)))
;   (* -1
;      (((partial 1) mu^r) (up x y z))
;      (((partial 0) mu^theta) (up x y z))))
;|#
;
;|#

;#|
;(instantiate-coordinates the-real-line 't)
;
;(define R2 (rectangular 2))
;(instantiate-coordinates R2 '(x y))
;
;(define phi
;  (compose (R2 '->point)
;	   (up square cube)
;	   (the-real-line '->coords)))
;
;(pec ((((pullback phi) (* x dy)) d/dt)
;      ((the-real-line '->point) 't0)))
;#| Result:
;(* 3 (expt t0 4))
;|#
;
;(define psi
;  (compose (the-real-line '->point)
;	   (lambda (v)
;	     (let ((x (ref v 0))
;		   (y (ref v 1)))
;	       (- x y)))
;	   (R2 '->coords)))
;
;(pec ((((pullback psi) dt)
;       (literal-vector-field 'u R2))
;      ((R2 '->point) (up 'x0 'y0))))
;#| Result:
;(+ (u^0 (up x0 y0)) (* -1 (u^1 (up x0 y0))))
;|#
;|#

;#|
;;;; pullback commutes with exterior derivative
;
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;(define R3-chi (R3 '->coords))
;(define R3-chi-inverse (R3 '->point))
;(define R3->R (-> (UP Real Real Real) Real))
;(define m3 ((R3 '->point) (up 'x0 'y0 'z0)))
;
;(define alpha (literal-function 'alpha R3->R))
;(define beta (literal-function 'beta R3->R))
;(define gamma (literal-function 'gamma R3->R))
;
;(define theta
;  (+ (* (compose alpha R3-chi) dx)
;     (* (compose beta R3-chi) dy)
;     (* (compose gamma R3-chi) dz)))
;
;(define R2 (rectangular 2))
;(instantiate-coordinates R2 '(u v))
;(define rectangular-chi (R2 '->coords))
;(define rectangular-chi-inverse (R2 '->point))
;(define R2->R (-> (UP Real Real) Real))
;(define m2 ((R2 '->point) (up 'u0 'v0)))
;(define X2 (literal-vector-field 'X R2))
;(define Y2 (literal-vector-field 'Y R2))
;
;(define mu
;  (compose R3-chi-inverse
;	   (up (literal-function 'mu^x R2->R)
;	       (literal-function 'mu^y R2->R)
;	       (literal-function 'mu^z R2->R))
;	   rectangular-chi))
;
;;;; first pullback a function
;
;(define f
;  (compose (literal-function 'f R3->R)
;	   R3-chi))
;
;(pec
; (((- ((pullback mu) (d f))
;      (d ((pullback mu) f)))
;   X2)
;  m2))
;#| Result:
;0
;|#
;
;;;; now pullback a form
;
;(pec (mu m2))
;#| Result:
;(up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0)))
;|#
;
;(pec ((((pullback mu) theta) X2) m2))
;#| Result:
;(+
; (* (((partial 0) mu^x) (up u0 v0))
;    (alpha (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
;    (X^0 (up u0 v0)))
; (* (((partial 1) mu^x) (up u0 v0))
;    (alpha (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
;    (X^1 (up u0 v0)))
; (* (((partial 0) mu^y) (up u0 v0))
;    (beta (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
;    (X^0 (up u0 v0)))
; (* (((partial 1) mu^y) (up u0 v0))
;    (beta (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
;    (X^1 (up u0 v0)))
; (* (((partial 0) mu^z) (up u0 v0))
;    (X^0 (up u0 v0))
;    (gamma (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0)))))
; (* (((partial 1) mu^z) (up u0 v0))
;    (gamma (up (mu^x (up u0 v0)) (mu^y (up u0 v0)) (mu^z (up u0 v0))))
;    (X^1 (up u0 v0))))
;|#
;
;(pec
; (((- ((pullback mu) (d theta))
;      (d ((pullback mu) theta)))
;   X2 Y2)
;  m2))
;#| Result:
;0
;|#
;;;; works.
;|#

;#|
;;;; Pullback commutes with wedge
;
;(pec
; (let ((theta (literal-1form-field 'theta R3))
;       (phi (literal-1form-field 'phi R3)))
;   (((- (wedge ((pullback mu) theta) ((pullback mu) phi))
;	((pullback mu) (wedge theta phi)))
;     X2
;     Y2)
;    m2)))
;#| Result:
;0
;|#
;
;(pec
; (let ((theta (literal-manifold-function 'f R3))
;       (phi (literal-1form-field 'phi R3)))
;   (((- (wedge ((pullback mu) theta) ((pullback mu) phi))
;	((pullback mu) (wedge theta phi)))
;     X2)
;    m2)))
;#| Result:
;0
;|#
;|#