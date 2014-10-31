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

;;;;                Tensor fields 

;;;  Goal is to write Einstein's Field Equations nicely, see end of file.

;;; Tensor fields are implemented as operators of type tensor.
;;; Tensor fields are defined only on manifolds.

;;; A tensor of type (n . m) takes n 1form fields and m vector-fields
;;; and produces a function on a point.  A tensor is applied to the n
;;; 1form-field arguments before the m vector-field arguments.

(define (tensor-field? t)
  (and (operator? t)
       (eq? (operator-subtype t) tensor-product)))

(define (tensor-type t)
  (cadr (assq 'tensor-type (operator-optionals t))))

(define (tensor-manifold t)
  (cadr (assq 'manifold (operator-optionals t))))

(define (has-tensor-manifold? t)
  (assq 'manifold (operator-optionals t)))

(define (tensor-type? t u d)
  (let ((tt (tensor-type t)))
    (and (= u (car tt)) (= d (cdr tt)))))

(define (make-tensor-field u d proc name manifold)
  (make-operator proc
		 (diffop-name name)
		 tensor-product
		 (rank->arity (fix:+ u d))
		 `(tensor-type ,(cons u d))
		 `(manifold ,manifold)))


(define (tf:zero . args) zero-manifold-function)

(define (tf:zero-like op)
  (assert (tensor-field? op) "tf:zero-like")
  (make-op tf:zero
	   'tf:zero
	   (operator-subtype op)
	   (operator-arity op)
	   (operator-optionals op)))

(assign-operation 'zero-like tf:zero-like tensor-field?)


(define (tf:zero? tf)
  (assert (tensor-field? tf) "tf:zero?")
  (eq? (operator-procedure tf) tf:zero))

(assign-operation 'zero? tf:zero? tensor-field?)

(define* (vector-field->tensor-field vf #:optional manifold)
  (assert (vector-field? vf) "Not a vector field.")
  (if (has-tensor-manifold? vf)
      (if (default-object? manifold)
	  (set! manifold (tensor-manifold vf))
	  (assert (eq? (tensor-manifold vf) manifold)
		  "Wrong manifold specified for vector field"))
      (if (default-object? manifold)
	  (error "Unknown manifold for vector field")))
  (vf->tf vf manifold))

(define (vf->tf vf manifold)
  (make-tensor-field 1 0
		     (lambda (ff) (ff vf))
		     `(->t ,(diffop-name vf))
		     manifold))

(define* (1form-field->tensor-field ff #:optional manifold)
  (assert (and (form-field? ff) (fix:= (get-rank ff) 1))
	  "Not a 1form field.")
  (if (has-tensor-manifold? ff)
      (if (default-object? manifold)
	  (set! manifold (tensor-manifold ff))
	  (assert (eq? (tensor-manifold ff) manifold)
		  "Wrong manifold for 1form field"))
      (if (default-object? manifold)
	  (error "Unknown manifold for 1form field")))
  (ff->tf ff manifold))

(define (ff->tf ff manifold)
  (make-tensor-field 0 1 ff
		     `(->t ,(diffop-name ff))
		     manifold))


;;; Manifold must be given here, since a function has no 
;;; structure giving the manifold.

(define (manifold-function->tensor-field f manifold)
  (make-tensor-field 0 0 (lambda () f)
		     `(->t ,(diffop-name f))
		     manifold))


(define (01tensor-field->1form-field t)
  (assert (and (tensor-field? t) (tensor-type? t 0 1))
	  "Not a (0 1) tensor.")
  (operator-procedure t))

(define (10tensor-field->vector-field t coordinate-system)
  (assert (and (tensor-field? t) (tensor-type? t 1 0))
	  "Not a (1 0) tensor.")
  (let ((basis (coordinate-system 'coordinate-basis)))
    (s:sigma/r (lambda (e f) (* (t f) e))
	       (basis->vector-basis basis)
	       (basis->1form-basis basis))))

;;; The product of T1 of type (n1 . d1) and T2 of type (n2 . d2) is a new
;;; tensor of type (n1+n2 . d1+d2).  

(define (tensor-product t1 t2)
  (assert (and (tensor-field? t1) (tensor-field? t2))
	  "Not tensor fields.")
  (let ((type1 (tensor-type t1)) (type2 (tensor-type t2))
	(man (tensor-manifold t1)))
    (assert (eq? man (tensor-manifold t2)) "Tensors not from same manifold")
    (let ((u1 (car type1)) (d1 (cdr type1))
	  (u2 (car type2)) (d2 (cdr type2)))
      (let ((u (fix:+ u1 u2)) (d (fix:+ d1 d2)))
	(let ((n (fix:+ u d)))
	  (define (the-product . args)
	    (assert (fix:= (length args) n)
		    "Wrong number of arguments to tensor product.")
	    (* (apply t1
		      (append (sublist args 0 u1)
			      (sublist args u (fix:+ u d1))))
	       (apply t2
		      (append (sublist args u1 u)
			      (sublist args (fix:+ u d1) n)))))
	  (make-tensor-field u d
			     the-product
			     `(t:* ,(diffop-name t1)
				   ,(diffop-name t2))
			     man))))))


;;; Contraction requires an appropriate coordinate system, but the
;;; result is independent of the coordinate system.

(define (contract t up-index down-index coordinate-system)
  (assert (tensor-field? t) "Not a tensor field.")
  (let ((type (tensor-type t))
	(basis (coordinate-system 'coordinate-basis)))
    (let ((u (car type)) (d (cdr type)))
      (assert (and (fix:<= 0 up-index) (fix:< up-index u))
	      "Up index not in range")
      (assert (and (fix:<= 0 down-index) (fix:< down-index d))
	      "Down index not in range")
      (let ((nu (fix:- u 1)) (nd (fix:- d 1)) (on (fix:+ u d)))
	(let ((n (fix:+ nu nd)) (ud (fix:+ u down-index)))
	  (define (the-contraction . args)
	    (assert (fix:= (length args) n)
		    "Wrong number of arguments to tensor contraction.")
	    (s:sigma/r (lambda (f e)
			 (apply t
				(generate-list on
				  (lambda (i)
				    (cond ((fix:< i up-index)
					   (list-ref args i)) 
					  ((fix:= i up-index) f)
					  ((and (fix:> i up-index)
						(fix:< i ud))
					   (list-ref args (fix:- i 1)))
					  ((fix:= i ud) e)
					  (else
					   (list-ref args
						     (fix:- i 2))))))))
		       (basis->1form-basis basis)
		       (basis->vector-basis basis)))
	  (make-tensor-field nu nd
			     the-contraction
			     `(contract ,(diffop-name t)
					,up-index ,down-index)
			     (tensor-manifold t)))))))

(define (tensor-field->coefficient-structure t coordinate-system)
  (assert (tensor-field? t) "Not a tensor field.")
  (let ((type (tensor-type t))
	(basis (coordinate-system 'coordinate-basis))
	(n (coordinate-system 'dimension)))
    (let ((u (car type)) (1form-basis (basis->1form-basis basis))
	  (d (cdr type)) (vector-basis (basis->vector-basis basis)))
      (define (iterate-up count argl)
	(if (fix:= count 0)
	    (apply t argl)
	    (s:generate n 'up
			(lambda (i)
			  (iterate-up (fix:- count 1)
				      (cons (ref 1form-basis i) argl))))))
      (define (iterate-down count argl)
	(if (fix:= count 0)
	    (iterate-up u argl)
	    (s:generate n 'down
			(lambda (i)
			  (iterate-down (fix:- count 1)
				      (cons (ref vector-basis i) argl))))))
      (iterate-down d '()))))


(define (coefficient-structure->tensor-field coeffs coordinate-system)
  (let ((basis (coordinate-system 'coordinate-basis))
	(manifold (coordinate-system 'manifold)))
    (let ((vector-basis
	   (s:map/r (lambda (e)
		      (vf->tf e manifold))
		    (basis->vector-basis basis)))
	  (1form-basis
	   (s:map/r (lambda (f)
		      (ff->tf f manifold))
		    (basis->1form-basis basis))))
      (define (coeff-walk coeffs)
	(if (structure? coeffs)
	    (let ((coeffs (s:map coeff-walk coeffs)))
	      (if (up? coeffs)
		  (s:sigma/r (lambda (coeff e_i)
			       (* coeff e_i))
			     coeffs
			     vector-basis)
		  (s:sigma/r (lambda (coeff w^i)
			       (* coeff w^i))
			     coeffs
			     1form-basis)))
	    coeffs))
      (coeff-walk coeffs))))

;;; I have to be able to invert the metric.

(define (invert-metric g coordinate-system)
  (assert (tensor-field? g)
	  "The metric tensor must be a tensor field.")
  (assert (eq? (tensor-manifold g) (coordinate-system 'manifold))
	  "Metric tensor and the coordinate system must have the same manifold.")
  (let ((type (tensor-type g)))
    (let ((u (car type)) (d (cdr type)))
      (define ((the-inverted-metric w1 w2) m)
	(let ((coeffs
	       ((tensor-field->coefficient-structure g coordinate-system) m)))
	  (let ((icoeffs (s:invert coeffs)))
	    (((coefficient-structure->tensor-field icoeffs coordinate-system)
	      w1 w2)
	     m))))
      (assert (and (fix:= u 0) (fix:= d 2)) "Not a 2-down metric")
      (make-tensor-field d u
			 the-inverted-metric
			 `(invert ,(diffop-name g))
			 (tensor-manifold g)))))

;;; Tensor derivative... The comma derivative does not produce an
;;; invariant tensor field, but rather a structure of coefficient
;;; functions that are the derivatives of the components of the tensor
;;; with respect to a given coordinate system.  These are packaged
;;; into a tensor-shaped structure to make uniform arithmetic.

(define (t:comma t coordinate-system)
  (assert (tensor-field? t) "Not a tensor field.")
  (assert (eq? (tensor-manifold t) (coordinate-system 'manifold))
	  "The tensor and the coordinate system must have the same manifold.")
  (let ((dc (coordinate-system 'dual-chains))
	(coeffs (tensor-field->coefficient-structure t coordinate-system)))
    (coefficient-structure->tensor-field
     (s:map (lambda (chain)
	      (s:map/r (lambda (coeff-fun)
			 (compose
			  ((apply partial chain)
			   (compose coeff-fun (coordinate-system '->point)))
			  (coordinate-system '->coords)))
		       coeffs))
	    dc)
     coordinate-system)))


(define (Christoffel g coordinate-system)
  (assert (tensor-field? g)
	  "The metric-tensor must be a tensor field.")
  (assert (eq? (tensor-manifold g) (coordinate-system 'manifold))
	  "Metric tensor and the coordinate system must have the same manifold.")
  (let ((gi (* 1/2 (invert-metric g coordinate-system)))
	(Dg (t:comma g coordinate-system))
	(basis (coordinate-system 'coordinate-basis)))
    (define (the-Christoffel wi ek el)
      (s:sigma/r
       (lambda (wm em)
	 (* (gi wi wm)
	    (- (+ (Dg em ek el) (Dg em el ek))
	       (Dg ek el em))))
       (basis->1form-basis basis)
       (basis->vector-basis basis)))
    (make-tensor-field 1 2 the-Christoffel
		       `(Christoffel ,(diffop-name g))
		       (tensor-manifold g))))


;;; Covariant derivatives.

(define (t:covariant-derivative metric-tensor coordinate-system)
  (assert (tensor-field? metric-tensor)
	  "The metric-tensor must be a tensor field.")
  (assert (eq? (tensor-manifold metric-tensor) (coordinate-system 'manifold))
	  "Metric tensor and the coordinate system must have the same manifold.")
  (let ((Gamma (Christoffel metric-tensor coordinate-system)))
    (define (nabla v)
      (assert (vector-field? v)
	      "Not a vector field: t:covariant-derivative")
      (let ((V (vf->tf v (tensor-manifold metric-tensor))))
	(define (nabla_v T)
	  (assert (tensor-field? T)
		  "Not a tensor field: t:covariant-derivative")
	  (let ((type (tensor-type T)))
	    (let ((u (car type)) (d (cdr type))
		  (TG (tensor-product T Gamma)))
	      (contract
	       (tensor-product V
			       (+ (t:comma T coordinate-system)
				  (- (sigma (lambda (i)
					      (contract TG i (fix:+ d 1)
							coordinate-system))
					    0 (fix:- u 1))
				     (sigma (lambda (i)
					      (contract TG
							u (fix:+ i (fix:- d 1))
							coordinate-system))
					    0 (fix:- d 1)))))
	       0 0
	       coordinate-system))))
	(make-operator nabla_v
		       `(nabla ,(diffop-name v)))))
    nabla))

;#|
;(define (Riemann-tensor metric-tensor coordinate-system)
;  (assert (tensor-field? metric-tensor)
;	  "The metric-tensor must be a tensor field.")
;  (assert (eq? (tensor-manifold metric-tensor)
;	       (coordinate-system 'manifold))
;	  "Metric and coordinate system must have the same manifold.")
;  (let ((manifold (tensor-manifold metric-tensor))
;	(nabla (t:covariant-derivative metric-tensor coordinate-system)))
;    (define (Riemann w x u v)
;      (((- (commutator (nabla u) (nabla v)) (nabla (commutator u v)))
;	(vf->tf x manifold))
;       w))
;    (make-tensor-field 1 3
;		       Riemann
;		       `(Riemann ,(diffop-name metric-tensor))
;		       manifold)))
;|#

;;; This alternate (Box 14.2 p.340 MTW) is vastly more efficient to compute.

(define (Riemann-tensor metric-tensor coordinate-system)
  (assert (tensor-field? metric-tensor)
	  "The metric-tensor must be a tensor field.")
  (assert (eq? (tensor-manifold metric-tensor)
	       (coordinate-system 'manifold))
	  "Metric and coordinate system must have the same manifold.")
  (let ((Gamma (Christoffel metric-tensor coordinate-system)))
    (let ((dGamma (t:comma Gamma coordinate-system))
	  (GG (contract (tensor-product Gamma Gamma) 0 3 coordinate-system)))
      (define (Riemann w x u v)
	(+ (- (dGamma w x v u) (dGamma w x u v))
	   (- (GG w x v u) (GG w x u v))))
      (make-tensor-field 1 3
		       Riemann
		       `(Riemann ,(diffop-name metric-tensor))
		       (tensor-manifold metric-tensor)))))

(define (Ricci-tensor metric-tensor coordinate-system)
  ;; u, v vector fields, m is a point
  (let ((R^i_jkl (Riemann-tensor metric-tensor coordinate-system)))
    (let ((R_jl (contract R^i_jkl 0 1 coordinate-system)))
      R_jl)))

(define (Ricci-scalar metric-tensor coordinate-system)
  (let ((R_ij (Ricci-tensor metric-tensor coordinate-system))
	(g^ki (invert-metric metric-tensor coordinate-system)))
    (let ((R^k_j
	   (contract (* g^ki R_ij) 1 0 coordinate-system)))
      (let ((R (contract R^k_j 0 0 coordinate-system)))
	R))))

(define (Einstein-tensor metric-tensor coordinate-system)
  (- (Ricci-tensor metric-tensor coordinate-system)
     (* 1/2
	(Ricci-scalar metric-tensor coordinate-system)
	metric-tensor)))

(define ((Einstein-field-equation k Lambda coordinate-system)
	 metric-tensor stress-energy-tensor)
  (- (+ (Einstein-tensor metric-tensor coordinate-system)
	(* Lambda metric-tensor))
     (* k stress-energy-tensor)))

;;; (pe (/ (* 8 :pi :G) (expt :c 4)))
;;; (& 2.076115391974129e-43 (/ (expt second 2) (* kilogram meter)))

;;; Perhaps suppress coordinate system?

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
;;; now...
;;; process time: 1960 (1860 RUN + 100 GC); real time: 1989
;|#
;
;(show-time
; (lambda ()
;   (pec ((tensor-field->coefficient-structure
;	  (Ricci-tensor S2M-metric spherical-coordinates)
;	  spherical-coordinates)
;	 ms))))
;#| Result:
;(down (down 1 0) (down 0 (expt (sin theta0) 2)))
;;; process time: 1220 (1170 RUN + 50 GC); real time: 1231
;|#
;
;(show-time
; (lambda ()
;   (pec (((Ricci-scalar S2M-metric spherical-coordinates))
;	 ms))))
;#| Result:
;2
;;; process time: 1220 (1170 RUN + 50 GC); real time: 1216
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
;|#

;#|
;(define (Minkowski-metric c)
;  (+ (* -1 (square c) (square dT)) (square dX) (square dY) (square dZ)))
;
;(define M-metric (Minkowski-metric 'c))
;
;(define V (literal-vector-field 'V spacetime-rectangular))
;
;(pec ((M-metric V V) r-event))
;#| Result:
;(+ (* -1 (expt (V^0 (up t0 x0 y0 z0)) 2))
;   (expt (V^1 (up t0 x0 y0 z0)) 2)
;   (expt (V^2 (up t0 x0 y0 z0)) 2)
;   (expt (V^3 (up t0 x0 y0 z0)) 2))
;|#
;
;(show-time
; (lambda ()
;   (pec ((tensor-field->coefficient-structure
;	  (Ricci-tensor M-metric spacetime-rectangular)
;	  spacetime-rectangular)
;	 r-event))))
;#| Result:
;(down (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0) (down 0 0 0 0))
;;; derivative hack 
;;; process time: 7216060 (6861050 RUN + 355010 GC); real time: 7272926
;;; Alternate definition of Riemann-tensor
;;; process time: 432410 (395800 RUN + 36610 GC); real time: 487212
;;; process time: 134980 (128750 RUN + 6230 GC); real time: 228098
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
;;; process time: 126780 (120940 RUN + 5840 GC); real time: 127096
;|#
;|#

;#|
;(define (Schwarzschild-metric M G c)
;  (let ((a (- 1 (/ (* 2 G M) (* (square c) r)))))
;    (+ (*  -1 (square c) a (square dT))
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