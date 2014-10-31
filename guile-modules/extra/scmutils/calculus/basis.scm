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

;;; The following give us dual bases 
;;; Basis objects have a dimension, a basis, a dual basis.


;;; coordinate bases

(define (coordinate-basis? x)
  (and (pair? x) (eq? (car x) '*coordinate-basis*)))

(define (coordinate-system->basis coordinate-system)
  (list '*coordinate-basis*
	(coordinate-system->vector-basis coordinate-system)
	(coordinate-system->1form-basis coordinate-system)
	(coordinate-system 'dimension)
	coordinate-system))


;;; general bases

(define (basis? x)
  (or (coordinate-basis? x)
      (and (pair? x)
	   (eq? (car x) '*basis*))))

(define (make-basis vector-basis 1form-basis)
  (let ((n (length (s:fringe vector-basis))))
    (assert (fix:= n (length (s:fringe 1form-basis))))
    (list '*basis* vector-basis 1form-basis n)))

(define (basis->vector-basis x)
  (assert (basis? x))
  (cadr x))

(define (basis->1form-basis x)
  (assert (basis? x))
  (caddr x))

(define (basis->dimension x)
  (assert (basis? x))
  (cadddr x))

;;; sigma (proc e_i w^i)
(define (contract-2 proc basis)
  (let ((vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis)))
    (s:sigma/r proc
	       vector-basis
	       1form-basis)))    

;;; Has a dependence on flat basis sets.  Experimental stuff kills system!

(define (vector-basis->dual vector-basis coordinate-system)
  (let* ((typical-coords (coordinate-system 'typical-coords))
	 (vector-basis-coefficient-functions
;	  #|
;	  (compose (vector-basis (coordinate-system '->coords))
;		   (coordinate-system '->point))
;	  |#
	  (s:map/r (lambda (basis-vector)
		     (vector-field->components basis-vector coordinate-system))
		   vector-basis)
	  )
	 (guts
	  (lambda (coords)
	    (s:flip (compatible-shape typical-coords)
		    (s:inverse
		     (compatible-shape typical-coords)
		     (s:map (lambda (fn) (fn coords))
			    vector-basis-coefficient-functions)
		     typical-coords)
		    typical-coords)))
	 (1form-basis-coefficient-functions 
	  (c:generate (coordinate-system 'dimension)
		      'up
		      (lambda (i)
			(compose (component i) guts))))
	 (1form-basis
	  (s:map/r (lambda (1form-basis-coefficient-function)
		     (components->1form-field 1form-basis-coefficient-function
					      coordinate-system))
		   1form-basis-coefficient-functions)))
    1form-basis))

;#|
;(instantiate-coordinates (S2 'R) '(theta phi))
;
;(define e0
;  (components->vector-field
;   (up (literal-function 'e0t (-> (UP* Real) Real))
;       (literal-function 'e0p (-> (UP* Real) Real)))
;   (S2 'R)))
;
;(define e1
;  (components->vector-field
;   (up (literal-function 'e1t (-> (UP* Real) Real))
;       (literal-function 'e1p (-> (UP* Real) Real)))
;   (S2 'R)))
;
;(define edual
;  (vector-basis->dual 
;   (down e0 e1)
;   (S2 'R)))
;
;(pec ((edual (down e0 e1))
;      (((S2 'R) '->point)
;       (up 'theta 'phi))))
;#| Result:
;(up (down 1 0) (down 0 1))
;|#
;|#

(define (((make-constant-vector-field basis m0) v) f)
  (let ((vector-basis (basis->vector-basis basis))
	(1form-basis (basis->1form-basis basis)))
    (* (vector-basis f)
       (s:map/r (lambda (1fb) (lambda (m) ((1fb v) m0)))
		1form-basis))))
