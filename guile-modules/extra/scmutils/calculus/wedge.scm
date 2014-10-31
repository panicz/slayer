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

;;; Higher rank forms can be constructed from 1forms by wedging them
;;; together.  This antisymmetric tensor product is computed as a
;;; determinant.  The purpose of this is to allow us to use the
;;; construction dx^dy to compute the area described by the vectors
;;; that are given to it.

(define (wedge2 form1 form2)
  (let ((n1 (get-rank form1)) (n2 (get-rank form2)))
    (if (or (zero? n1) (zero? n2))
	(* form1 form2)
	(let ((n (fix:+ n1 n2)))
	  (define (the-wedge . args)
	    (assert (fix:= (length args) n)
		    "Wrong number of args to wedge product")
	    (let ((perms (permutations (iota n))))
	      (g:* (/ 1 (* (factorial n1) (factorial n2))) ; Error in Singer.
		   (apply g:+
			  (map (lambda (p)
				 (let ((pargs (permute p args)))
				   (let ((order (permutation-interchanges p))
					 (a1 (list-head pargs n1))
					 (a2 (list-tail pargs n1)))
				     (g:* (if (even? order) 1 -1)
					  (apply form1 a1)
					  (apply form2 a2)))))
			       perms)))))
	  (procedure->nform-field the-wedge
				  `(wedge ,(diffop-name form1)
					  ,(diffop-name form2))
				  n)))))

(define (wedge . args)
  (reduce wedge2 (constant 1) args))

;;; See Spivak p275 v1 "Differential Geometry" to see the correct
;;; definition.  The key is that the wedge of the coordinate basis
;;; forms had better be the volume element.


(define (get-rank op)
  (cond ((operator? op)
	 (let ((a (arity op)))
	   (if (not (and (pair? a)
			 (exact-integer? (car a))
			 (exact-integer? (cdr a))
			 (int:= (car a) (cdr a))))
	       (error "Unknown rank operator " op))
	   (car a)))
	((function? op) 0)
	(else (error "Bad rank " op))))

(define (rank->arity n)
  (cons n n))

(define (procedure->nform-field proc name n)
  (if (= n 0)
      (proc)
      (make-operator proc name wedge (rank->arity n))))

;#|
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;(define R3-point ((R3 '->point) (up 'x0 'y0 'z0)))
;
;(define w (literal-1form-field 'w R3))
;(define u (literal-1form-field 'u R3))
;(define v (literal-1form-field 'v R3))
;
;(define X (literal-vector-field 'X R3))
;(define Y (literal-vector-field 'Y R3))
;(define Z (literal-vector-field 'Z R3))
;(define W (literal-vector-field 'W R3))
;
;;;; Just checking that everything is working...
;
;(pec ((w X) R3-point))
;#| Result:
;(+ (* (X^0 (up x0 y0 z0)) (w_0 (up x0 y0 z0)))
;   (* (X^1 (up x0 y0 z0)) (w_1 (up x0 y0 z0)))
;   (* (X^2 (up x0 y0 z0)) (w_2 (up x0 y0 z0))))
;|#
;
;;;; A few theorems
;
;(pec (((- (wedge (wedge w u) v) (wedge w (wedge u v))) X Y Z)
;      R3-point))
;#| Result:
;0
;|#
;
;(pec (((- (wedge (+ w u) v) (+ (wedge w v) (wedge u v))) X Y)
;      R3-point))
;#| Result:
;0
;|#
;
;;;; Note, a product of forms is their wedge!
;
;(pec (((- (wedge u v) (* u v)) X Y)
;      R3-point))
;#| Result:
;0
;|#
;|#

;#|
;(define R3 (rectangular 3))
;(instantiate-coordinates R3 '(x y z))
;(define R3-point ((R3 '->point) (up 'x0 'y0 'z0)))
;
;(define dx^dy (wedge dx dy))
;
;((dx^dy d/dx d/dy) R3-point)
;;Value 1
;
;((dx^dy d/dx d/dx) R3-point)
;;Value: 0
;
;((dx^dy d/dy d/dx) R3-point)
;;Value: -1
;|#

;;; Alternative definition in terms of alternation.

(define (Alt form)
  (let ((n (get-rank form)))
    (if (zero? n)
	form
	(let ()
	  (define (the-alternation . args)
	    (assert (fix:= (length args) n)
		    "Wrong number of args to alternation")
	    (let ((perms (permutations (iota n))))
	      (g:* (/ 1 (factorial n))
		   (apply g:+
			  (map (lambda (p)
				 (let ((pargs (permute p args)))
				   (let ((order (permutation-interchanges p)))
				     (g:* (if (even? order) 1 -1)
					  (apply form pargs)))))
			       perms)))))
	  (procedure->nform-field the-alternation
				  `(Alt ,(diffop-name form))
				  n)))))

(define (tensor-product2 t1 t2)
  (let ((n1 (get-rank t1)) (n2 (get-rank t2)))
    (if (or (zero? n1) (zero? n2))
	(* form1 form2)
	(let ((n (fix:+ n1 n2)))
	  (define (the-product . args)
	    (assert (fix:= (length args) n)
		    "Wrong number of args to tensor product")
	    (* (apply t1 (list-head args n1))
	       (apply t2 (list-tail args n1))))
	  (procedure->nform-field the-product
				  `(tensor-product ,(diffop-name t1)
						   ,(diffop-name t2))
				  n)))))

(define (w2 form1 form2)
  (let ((n1 (get-rank form1)) (n2 (get-rank form2)))
    (* (/ (factorial (+ n1 n2))
	  (* (factorial n1) (factorial n2)))
       (Alt (tensor-product2 form1 form2)))))

;;;(define (wedge . args)
;;;  (reduce w2 (constant 1) args))
