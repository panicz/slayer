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

;;; Given a list of vectors, produce an orthogonal list that spans the same space.

(define* (gram-schmidt vects #:optional metric)
  (if (default-object? metric)
      (set! metric euclidean-metric))
  (let lp ((to-go (cdr vects)) (done (list (car vects))))
    (if (null? to-go)
	done
	(let ((a (car to-go)))
	  (let ((b (- a
		      (apply +
			     (map (lambda (b)
				    (* (/ (metric a b) (metric b b)) b))
				  done)))))
	    (if (zero? b)
		'dependent
		(lp (cdr to-go)
		    (cons b
			  done))))))))

(define (euclidean-metric v1 v2)
  (s:dot-product v1 v2))

(define ((lorentz-metric c) v1 v2)
  (assert (and (structure? v1)
	       (structure? v2)
	       (eq? (s:type v1) (s:type v2))
	       (= (s:dimension v1) 4)
	       (= (s:dimension v2) 4)))
  (- (+ (* (ref v1 1) (ref v2 1))
	(* (ref v1 2) (ref v2 2))
	(* (ref v1 3) (ref v2 3)))
     (* c c (ref v1 0) (ref v2 0))))

(define ((structure->metric m) v1 v2)
  (* v1 (* m v2)))

(define ((matrix->metric m) v1 v2)
  (assert (vector? v1) (vector v2))
  (matrix-ref (* (vector->row-matrix v1)
		 m
		 (vector->column-matrix v2))
	      0 0))




