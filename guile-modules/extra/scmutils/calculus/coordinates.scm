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

;;;; A coordinate system is an invertible map from the space to R^n.

;;; The following are some useful embeddings of common manifold
;;; coordinate systems in R^n.

;;; We can define the ordinary n-dimensional rectangular coordinates
;;; on R^n as a message-acceptor as follows:

(define (real-tuple n)
  (define (coordinates->point coords)
    (if (and (up? coords) (fix:= (s:dimension coords) n))
	coords
	(error "Bad coordinates: real-tuple" coords)))
  (define (point->coordinates point)
    (if (and (up? point) (fix:= (s:dimension point) n))
	point
	(error "Bad point: real-tuple" point)))
  (assert (exact-integer? n) "Bad dimension: real-tuple")
  (let* ((coord-prototype
	  (c:generate n 'up (lambda (i) (symbol 'x i))))
	 ;; Should be (make-access-chains coord-prototype)
	 (access-chains (c:generate n 'up (lambda (i) (list i))))
	 (dual-chains (flip-indices access-chains))
	 (point-prototype (c:generate n 'up (lambda (i) (symbol 'xi i))))
	 (point-chains (c:generate n 'up (lambda (i) (list i)))))
    (lambda (m)
      (case m
	((dimension) n)
	((->point) coordinates->point)
	((->coords) point->coordinates)
	((type) real-tuple)
	((typical-coords) (typical-object coord-prototype))
	((coordinate-prototype) coord-prototype)
	((access-chains) access-chains)
	((dual-chains) dual-chains)
	((typical-point) (typical-object point-prototype))
	((point-chains) point-chains)
	((manifold) #f)
	(else (error "Unknown message: real-tuple" m))))))

(define rectangular real-tuple)

(define the-real-line
  (let ()
    (define (coordinates->point coord)
      (assert (numerical-quantity? coord) "Bad coordinate: the-real-line")
      (up coord))
    (define (point->coordinates point)
      (assert (and (up? point) (fix:= (s:dimension point) 1))
	      "Bad point: the-real-line")
      (ref point 0))
    (let* ((coord-prototype 't)
	   ;; Should be (make-access-chains coord-prototype)
	   (access-chains (up (list)))
	   (dual-chains (down (list)))
	   (point-prototype (up 't))
	   (point-chains (up 0)))
      (lambda (m)
	(case m
	  ((dimension) 1)
	  ((->point) coordinates->point)
	  ((->coords) point->coordinates)
	  ((type) the-real-line)
	  ((typical-coords) (typical-object coord-prototype))
	  ((coordinate-prototype) coord-prototype)
	  ((access-chains) access-chains)
	  ((dual-chains) dual-chains)
	  ((typical-point) (typical-object point-prototype))
	  ((point-chains) point-chains)
	  ((manifold) #f)
	  (else (error "Unknown message: the-real-line" m)))))))

;;; We can also have coordinate systems that cover only part of the
;;; space.  The polar/cylindrical coordinate system is singular at the
;;; origin:

(define (polar/cylindrical n)
  (define (coordinates->point coords)
    (if (and (up? coords) (fix:= (s:dimension coords) n))
	(let ((r (ref coords 0))
	      (theta (ref coords 1)))
	  (s:generate (s:dimension coords) 'up
		      (lambda (i)
			(cond ((= i 0) (* r (cos theta)))
			      ((= i 1) (* r (sin theta)))
			      (else (ref coords i))))))
	(error "Bad coordinates: polar/cylindrical" coords)))
  (define (point->coordinates point)
    (if (and (up? point) (fix:= (s:dimension point) n))
	(let ((x (ref point 0))
	      (y (ref point 1)))
	  (s:generate (s:dimension point) 'up
		      (lambda (i)
			(cond ((= i 0) (sqrt (+ (square x) (square y))))
			      ((= i 1) (atan y x))
			      (else (ref point i))))))
	(error "Bad point: polar/cylindrical" point)))
  (assert (and (exact-integer? n) (> n 1)) "Bad dimension: polar/cylindrical")
  (let* ((coord-prototype (s:generate n 'up (lambda (i) (symbol 'x i))))
	 ;; Should be (make-access-chains coord-prototype)
	 (access-chains (s:generate n 'up (lambda (i) (list i))))
	 (dual-chains (flip-indices access-chains))
	 (point-prototype (s:generate n 'up (lambda (i) (symbol 'xi i))))
	 (point-chains (s:generate n 'up (lambda (i) (list i)))))
    (lambda (m)
      (case m
	((dimension) n)
	((->point) coordinates->point)
	((->coords) point->coordinates)
	((type) polar/cylindrical)
	((typical-coords) (typical-object coord-prototype))
	((coordinate-prototype) coord-prototype)
	((access-chains) access-chains)
	((dual-chains) dual-chains)
	((typical-point) (typical-object point-prototype))
	((point-chains) point-chains)
	((manifold) #f)
	(else (error "Unknown message: polar/cylindrical" m))))))

;;; Consider a two sphere, in spherical coordinates
;;;  (theta, phi) = (colatitude, longitude).

(define (S2 r)
  (define (coordinates->point coords)
    (if (and (up? coords) (fix:= (s:dimension coords) 2))
	(let ((theta (ref coords 0))
	      (phi   (ref coords 1)))
	  (up (* r (sin theta) (cos phi)) ;x
	      (* r (sin theta) (sin phi)) ;y
	      (* r (cos theta))))	  ;z
	(error "Bad coordinates: S2" coords)))
  (define (point->coordinates point)
    (if (and (up? point) (fix:= (s:dimension point) 3))
	(let ((x (ref point 0))
	      (y (ref point 1))
	      (z (ref point 2)))
	  ;;(assert (= (square r)
	  ;;           (+ (square x) (square y) (square z))))
	  (up (acos (/ z r))		;theta
	      (atan y x)))		;phi
	(error "Bad point: S2" point)))
  ;(assert (and (real? r) (> r 0)))
  (let* ((coord-prototype (up 'theta 'phi))
	 ;; Should be (make-access-chains coord-prototype)
	 (access-chains (up (list 0) (list 1)))
	 (dual-chains   (down (list 0) (list 1)))
	 (point-prototype (up 'x 'y 'z))
	 (point-chains (up (list 0) (list 1) (list 2))))
    (lambda (m)
      (case m
	((dimension) 2)
	((->point) coordinates->point)
	((->coords) point->coordinates)
	((type) spherical)
	((typical-coords) (typical-object coord-prototype))
	((coordinate-prototype) coord-prototype)
	((access-chains) access-chains)
	((dual-chains) dual-chains)
	((typical-point) (typical-object point-prototype))
	((point-chains) point-chains)
	((manifold) #f)
	(else
	 (error "Unknown message: spherical" m))))))

;;; There is a kludge in this system... A single coordinate is just
;;; the number:

(define (c:generate n type proc)
  (if (fix:= n 1)
      (proc 0)
      (s:generate n type proc)))


;#|
;(define rectangular-plane (rectangular 2))
;(define rectangular-3space (rectangular 3))
;
;(define polar (polar/cylindrical 2))
;(define cylindrical (polar/cylindrical 3))
;
;;;; These coordinate systems are quite nice.  The points are really
;;;; the intermediates needed to give the "square root" of a coordinate
;;;; transformation.
;
;(pe
; ((compose (polar '->coords) (rectangular-plane '->point))
;  (up 'x 'y)))
;(up (sqrt (+ (expt x 2) (expt y 2))) (atan y x))
;   
;(pe
; ((compose (rectangular-plane '->coords) (polar '->point))
;  (up 'r 'theta)))
;(up (* r (cos theta)) (* r (sin theta)))
;|#

(define (instantiate-coordinates coordinate-system coordinate-names)
  (assert (or (and (eq? coordinate-system the-real-line)
		   (symbol? coordinate-names))
	      (and (list? coordinate-names)
		   (= (coordinate-system 'dimension)
		      (length coordinate-names)))
	      (and (structure? coordinate-names)
		   (= (coordinate-system 'dimension)
		      (s:dimension coordinate-names))))
	  "Wrong number of coordinate names -- INSTANTIATE-COORDINATES")
  (for-each
   (lambda (name access-chain dual-chain)
     ;; Make the coordinate function
     (if (environment-bound? generic-environment name)
	 (write-line `(clobbering ,name)))
     (environment-define generic-environment name
			 (compose (apply component access-chain)
				  (coordinate-system '->coords)))
     
     ;; make the coordinate basis vectors
     (let ((oname
	    (string->symbol (string-append "d/d" (symbol->string name)))))
       (if (environment-bound? generic-environment oname)
	   (write-line `(clobbering ,oname)))
       (environment-define generic-environment oname
			   (apply coordinate-basis-vector-field
				  coordinate-system
				  oname
				  access-chain)))
     ;; make the coordinate basis forms
     (let ((oname
	    (string->symbol (string-append "d" (symbol->string name)))))
       (if (environment-bound? generic-environment oname)
	   (write-line `(clobbering ,oname)))
       (environment-define generic-environment oname
			   (apply coordinate-basis-1form-field
				  coordinate-system
				  oname
				  dual-chain))))
   (cond ((symbol? coordinate-names)
	  (list coordinate-names))
	 ((list? coordinate-names)
	  (reverse coordinate-names))
	 ((structure? coordinate-names)
	  (s:fringe coordinate-names))
	 (else (error "Bad coordinate-name structure")))
   (s:fringe (coordinate-system 'access-chains))
   (s:fringe (coordinate-system 'dual-chains)))
  'done)

;;; Stubs, to be defined in later code

(define (coordinate-basis-vector-field . x) 'for-later)

(define (coordinate-basis-1form-field . x) 'for-later)


;#|
;;;; A scalar field can be defined by combining coordinate functions:
;
;;;; First we instantiate the coordinate functions:
;
;(instantiate-coordinates rectangular-3space '(x y z))
;
;(define h (+ 5 (square x) (* -1 x (cube y)) (/ 1 y)))
;
;;;; The field, however defined, can be seen as independent of
;;;; coordinate system:
;
;(h ((rectangular-3space '->point) (up 3. 4. 'z)))
;;Value: -177.75
;
;(h ((cylindrical '->point) (up 5. (atan 4 3) 'z)))
;;Value: -177.74999999999997
;
;;;; However this may be too clever, producing a traditional notation
;;;; that is hard to understand deeply.  Perhaps it is better to be
;;;; explicit about what is coordinate-system independent.  For
;;;; example, we can define a coordinate-free function h by composing a
;;;; definition in terms of coordinates with a coordinate function.
;
;(define (h-concrete xy)
;  (let ((x (ref xy 0))
;	(y (ref xy 1)))
;    (+ 5
;       (square x)
;       (* -1 x (cube y))
;       (/ 1 y))))
;
;(define h
;  (compose h-concrete (rectangular-3space '->coords)))
;
;(h ((rectangular-3space '->point) (up 3. 4 5)))
;;Value: -177.75
;|#

(define (literal-scalar-field name coordinate-system)
  (let ((n (coordinate-system 'dimension)))
    (let ((function-signature
	   (if (fix:= n 1) (-> Real Real) (-> (UP* Real n) Real))))
      (compose (literal-function name function-signature)
	       (coordinate-system '->coords)))))

(define (zero-coordinate-function c) 0)

;;; An alias.

(define literal-manifold-function literal-scalar-field)

(define (zero-manifold-function m) 0)

(define (one-manifold-function m) 1)
