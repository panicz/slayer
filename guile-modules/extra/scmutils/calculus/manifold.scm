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

;;;; Manifolds are declared

(define (define-manifold manifold-name
	  manifold-type manifold-dimension
	  point-type)
  (let ((point-prototype
	 (structure->prototype manifold-name point-type))
	(point-chains
	 (structure->access-chains point-type))
	(coordinate-systems '()))
    (define (the-manifold m)
      (case m
	((name manifold-name) manifold-name)

	((type manifold-type) manifold-type)
	((dimension manifold-dimension) manifold-dimension)

	((point-type) point-type)
	((point-dimension) s:dimension)

	((typical-point) (typical-object point-prototype))
	((point-prototype) point-prototype)
	((point-chains) point-chains)

	((new-coordinates)
	 (lambda (new)
	   (set! coordinate-systems
		 (cons new coordinate-systems))))

	(else
	 (let ((cs
		(find-matching-item coordinate-systems
		  (lambda (coordinate-system)
		    (eq? m (coordinate-system 'coordinate-system-name))))))
	   (if cs
	       cs
	       (error "Unknown message: manifold" manifold-name m))))))
    (if (environment-bound? generic-environment manifold-name)
	(write-line `(clobbering ,manifold-name)))
    (environment-define generic-environment manifold-name the-manifold)
    manifold-name))

(define (define-coordinate-system manifold coordinate-system-name
	  coordinate-prototype
	  coordinates->point point->coordinates)
  (let* ((access-chains
	  (s:map-chain (lambda (element chain) chain)
		       coordinate-prototype))
	 (dual-chains (flip-indices access-chains)))
    (let ((n (s:dimension coordinate-prototype))
	  (coordinate-functions #f)
	  (coordinate-basis-vector-fields #f)
	  (coordinate-basis-1form-fields #f)
	  (coordinate-basis #f)
	  (coordinates->point
	   (compose (linear-memoize-1arg coordinates->point)
		    (lambda (coords)
		      (s:map/r simplify-numerical-expression
			       coords))))
	  (point->coordinates
	   (compose (linear-memoize-1arg point->coordinates)
		    (lambda (point)
		      (s:map/r simplify-numerical-expression
			       point)))))
      (define (the-coordinate-system m)
	(case m
	  ((manifold) manifold)
	  ((name coordinate-system-name) coordinate-system-name)
	  ((dimension coordinate-system-dimension)  n)
	  ((->point) coordinates->point)
	  ((->coords) point->coordinates)
	  ((typical-coords) (typical-object coordinate-prototype))
	  ((coordinate-prototype) coordinate-prototype)
	  ((access-chains) access-chains)
	  ((dual-chains) dual-chains)
	  ((coordinate-functions)
	   (if (not coordinate-functions)
	       (set! coordinate-functions
		     (s:map/r (lambda (coordinate-name access-chain)
				(list coordinate-name
				      (compose (apply component access-chain)
					       point->coordinates)))
			      coordinate-prototype
			      access-chains)))
	   coordinate-functions)
	  ((coordinate-basis-vector-fields)
	   (if (not coordinate-basis-vector-fields)
	       (set! coordinate-basis-vector-fields
		     (s:map/r (lambda (coordinate-name access-chain)
				(let* ((oname (symbol 'd/d coordinate-name))
				       (vf
					(apply coordinate-basis-vector-field
					       the-coordinate-system
					       oname
					       access-chain)))
				  (set-operator-optionals! vf
				    (cons `(manifold ,manifold)
					  (operator-optionals vf)))
				  (list oname vf)))
			      coordinate-prototype
			      access-chains)))
	   coordinate-basis-vector-fields)

	  ((coordinate-basis-1form-fields)
	   (if (not coordinate-basis-1form-fields)
	       (set! coordinate-basis-1form-fields
		     (s:map/r (lambda (coordinate-name access-chain)
				(let* ((oname (symbol 'd coordinate-name))
				       (ff
					(apply coordinate-basis-1form-field
					       the-coordinate-system
					       oname
					       access-chain)))
				  (set-operator-optionals! ff
				    (cons `(manifold ,manifold)
					  (operator-optionals ff)))
				  (list oname ff)))
			      coordinate-prototype
			      access-chains)))
	   coordinate-basis-1form-fields)
	  ((coordinate-basis)
	   (if (not coordinate-basis)
	       (set! coordinate-basis
		     (make-basis
		      (s:map/r cadr
			       (flip-indices
				(the-coordinate-system
				 'coordinate-basis-vector-fields)))
		      (s:map/r cadr
			       (the-coordinate-system
				'coordinate-basis-1form-fields)))))
	   coordinate-basis)
	  (else
	   (cond ((assq m
			(vector->list
			 (the-coordinate-system
			  'coordinate-functions)))
		  => cadr)
		 ((assq m
			(vector->list
			 (the-coordinate-system
			  'coordinate-basis-vector-fields)))
		  => cadr)
		 ((assq m
			(vector->list
			 (the-coordinate-system
			  'coordinate-basis-1form-fields)))
		  => cadr)
		 (else
		  (error "Unknown message: coordinate-system"
			 coordinate-system-name (manifold 'manifold-name)))))))
      (if (and (not (fix:= n (manifold 'dimension)))
	       (not (eq? coordinate-system-name 'embedding)))
	  (error "Coordinate system does not have dimension of manifold"
		 coordinate-system-name (manifold 'manifold-name)))

      ((manifold 'new-coordinates) the-coordinate-system)))
  (list coordinate-system-name (manifold 'name)))


(define (install-coordinates coordinate-system)
  (define (install-symbol name value)
    (if (environment-bound? generic-environment name)
	(write-line `(clobbering ,name)))
    (environment-define generic-environment name value))
  (define (install-symbols s)
    (s:foreach (lambda (symval)
		 (install-symbol (car symval) (cadr symval)))
	       s))
  (install-symbols (coordinate-system 'coordinate-functions))
  (install-symbols (coordinate-system 'coordinate-basis-vector-fields))
  (install-symbols (coordinate-system 'coordinate-basis-1form-fields))
  (list (coordinate-system 'name) ((coordinate-system 'manifold) 'name)))

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
;	(error "Bad point: real-tuple" point)))
;  )
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
;			      ((= i 1) (atan y x))
;			      (else (ref point i))))))
;	(error "Bad point: polar" point)))
;  )
;
;(install-coordinates (Euclidean-plane 'rectangular))
;
;(install-coordinates (Euclidean-plane 'polar))
;
;(define mr (((Euclidean-plane 'rectangular) '->point) (up 'x0 'y0)))
;
;(define mp (((Euclidean-plane 'polar) '->point) (up 'r0 'theta0)))
;
;(define circular (- (* x d/dy) (* y d/dx)))
;
;(pec ((circular (+ (* 2 x) (* 3 y))) mr))
;#| Result:
;(+ (* 3 x0) (* -2 y0))
;|#
;
;(pec ((circular theta) mr))
;#| Result:
;1
;|#
;
;(pec ((dr circular) mr))
;#| Result:
;0
;|#
;
;
;(pec (((d r) d/dr) mr))
;#| Result:
;1
;|#
;
;(pec ((dr d/dr) mr))
;#| Result:
;1
;|#
;
;
;(pec ((dr (literal-vector-field 'v (Euclidean-plane 'polar))) mr))
;#| Result:
;(v^0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
;|#
;
;(pec (((d r) (literal-vector-field 'v (Euclidean-plane 'polar))) mr))
;#| Result:
;(v^0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
;|#
;
;(pec ((dr (literal-vector-field 'v (Euclidean-plane 'rectangular))) mr))
;#| Result:
;(+ (/ (* x0 (v^0 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2))))
;   (/ (* y0 (v^1 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2)))))
;|#
;
;(pec (((d r) (literal-vector-field 'v (Euclidean-plane 'rectangular))) mr))
;#| Result:
;(+ (/ (* x0 (v^0 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2))))
;   (/ (* y0 (v^1 (up x0 y0))) (sqrt (+ (expt x0 2) (expt y0 2)))))
;|#
;
;(define (g-polar u v)
;  (+ (* (dr u) (dr v))
;     (* (* r (dtheta u)) (* r (dtheta v)))))
;
;(define (g-rect u v)
;  (+ (* (dx u) (dx v))
;     (* (dy u) (dy v))))
;
;(pec (((- g-polar g-rect)
;      (literal-vector-field 'v (Euclidean-plane 'rectangular))
;      (literal-vector-field 'v (Euclidean-plane 'rectangular)))
;     mr))
;#| Result:
;0
;|#
;
;(pec (((- g-polar g-rect)
;      (literal-vector-field 'v (Euclidean-plane 'polar))
;      (literal-vector-field 'v (Euclidean-plane 'polar)))
;     mr))
;#| Result:
;0
;|#
;
;(pec (((- g-polar g-rect)
;      (literal-vector-field 'v (Euclidean-plane 'polar))
;      (literal-vector-field 'v (Euclidean-plane 'polar)))
;     mp))
;#| Result:
;0
;|#
;
;(pec (((- g-polar g-rect)
;      (literal-vector-field 'v (Euclidean-plane 'rectangular))
;      (literal-vector-field 'v (Euclidean-plane 'rectangular)))
;     mp))
;#| Result:
;0
;|#
;
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
;	      (* (cos theta))))		;z
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
;	(error "Bad point: S2M" point)))
;  )
;|#


