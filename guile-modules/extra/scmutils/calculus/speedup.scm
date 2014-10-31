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

;;;;                  Calculus Speedups
;;; These overlay the procedures in other files, improving their
;;; performance, and making some previously infeasible computations
;;; feasible.

(define *optimizing-functions* #t)
;;;(define *optimizing-functions* #f)

(define *debugging-simplify-function* #f)

;;; Functions of one structured argument.  We must supply an argument
;;; prototype.

(define memoized-simplify
  (hash-memoize-1arg 
   (lambda (expr)
     (default-simplify expr))))


(define (simplify-structure-function f argument-prototype)  
  (if *optimizing-functions*
      (let* ((value
	      (with-literal-reconstruction-enabled
		  (lambda ()
		    (with-self-evaluating-unbound-variables
			(lambda () (f argument-prototype))))))
	     (value-expression (memoized-simplify value)))
	((memoized-abstract-structure-function argument-prototype)
	 value-expression))
      f))

(define memoized-abstract-structure-function
  (linear-memoize-1arg
   (lambda (argument-prototype)
     (hash-memoize-1arg
      (lambda (value-expression)
	(abstract-structure-function argument-prototype value-expression))))))


(define excluded-symbols
  (append '(Real X -> UP DOWN UP* DOWN* literal-function quote up down partial)
	  symbolic-operators))

(define (abstract-structure-function argument-prototype value-expression)
  (cond ((number? value-expression)
	 (cond ((zero? value-expression) zero-manifold-function)
	       ((one? value-expression) one-manifold-function)
	       (else (constant value-expression))))
	(else
	 (let* ((parameter-chains
		 (reverse! (s:fringe (s:map-chain cons argument-prototype))))
		(parameters (map car parameter-chains))
		(access-chains (map cdr parameter-chains))
		(free-vars
		 (list-difference (list-difference
				   (expression/free-variables
				    (text->expression value-expression))
				   parameters)
				  excluded-symbols))
		(function-expression
		 (lambdafy 1
			   (lambda (svar-name)
			     `(let (,@(map (lambda (var) `(,var ',var))
					   free-vars)
				    ,@(map (lambda (chain)
					     `(,(car chain)
					       (g:ref ,svar-name ,@(cdr chain))))
					   parameter-chains) )
				(with-literal-apply-enabled
				 (lambda () ,value-expression))))))
		(cselimed-expr (gjs/cselim function-expression))
		(function
		 (lambda->interpreted-generic-procedure cselimed-expr)))
	   (if *debugging-simplify-function* (pp cselimed-expr))
	   function))))

;;; Sometimes we need to simplify an internal result.  

(define (simplify-numerical-expression expr)
  (cond ((and (pair? expr) (eq? (car expr) '*number*))
	 (let ((result
		(make-numerical-literal
		 (memoized-simplify expr))))
	   ;; copy extra properties, if any
	   (set-cdr! (cdr result) (cddr expr))
	   result))
	(else expr)))

;#|;(pp (simplify-numerical-expression
;     (/ 1 (+ (/ 1 'r1) (/ 1 'r2)))))
;(*number* (expression (/ (* r1 r2) (+ r1 r2))))
;|#

;;; Procedures that benefit from such a speedup...

;;; from tensor.scm 

(define (t:comma t coordinate-system)
  (assert (tensor-field? t) "Not a tensor field.")
  (let ((dc (coordinate-system 'dual-chains))
	(coeffs (tensor-field->coefficient-structure t coordinate-system)))
    (coefficient-structure->tensor-field
     (s:map (lambda (chain)
	      (s:map/r (lambda (coeff-fun)
			 (let* ((f
				 ((apply partial chain)
				  (compose coeff-fun (coordinate-system '->point))))
;				#|(sf 
;				 (simplify-structure-function f
;				      (coordinate-system 'coordinate-prototype)))
				)
			   (simplify-structure-function
			    (compose f (coordinate-system '->coords))
			    ((tensor-manifold t) 'point-prototype))))
		       coeffs))
	    dc)
     coordinate-system)))

;;; From vector-fields.scm

(define ((vector-field-procedure components coordinate-system) f)
  (let ((vf (* (D (compose f (coordinate-system '->point))) components)))
    (if (coordinate-system 'manifold)
	(simplify-structure-function
	 (compose vf (coordinate-system '->coords))
	 ((coordinate-system 'manifold) 'point-prototype))
	(compose (simplify-structure-function vf
		       (coordinate-system 'coordinate-prototype))
		 (coordinate-system '->coords)))))

(define ((coordinate-basis-vector-field-procedure coordinate-system . i) f)
  (if (coordinate-system 'manifold)
      (simplify-structure-function
       (compose ((apply partial i) (compose f (coordinate-system '->point)))
		(coordinate-system '->coords))
       ((coordinate-system 'manifold) 'point-prototype))
      (compose (simplify-structure-function
		((apply partial i) (compose f (coordinate-system '->point)))
		(coordinate-system 'coordinate-prototype))
	       (coordinate-system '->coords))))


;;; From form-fields.scm

(define ((1form-field-procedure components coordinate-system) vf)
  (define (internal vf)
    (assert (vector-field? vf))
    (if (coordinate-system 'manifold)
	(simplify-structure-function
	 (compose (* components
		     (vector-field->components vf coordinate-system))
		  (coordinate-system '->coords))
	 ((coordinate-system 'manifold) 'point-prototype))
	(compose (simplify-structure-function
		  (* components
		     (vector-field->components vf coordinate-system))
		  (coordinate-system 'coordinate-prototype))
		 (coordinate-system '->coords))))
  (s:map/r internal vf))

(define (tensor-field->coefficient-structure t coordinate-system)
  (assert (tensor-field? t) "Not a tensor field.")
  (let ((type (tensor-type t))
	(basis (coordinate-system 'coordinate-basis))
	(n (coordinate-system 'dimension)))
    (let ((u (car type)) (1form-basis (basis->1form-basis basis))
			 (d (cdr type)) (vector-basis (basis->vector-basis basis)))
      (define (iterate-up count argl)
	(if (fix:= count 0)
	    (simplify-structure-function (apply t argl)
					 ((tensor-manifold t) 'point-prototype))
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
	    (simplify-structure-function
	     (* (apply t1
		       (append (sublist args 0 u1)
			       (sublist args u (fix:+ u d1))))
		(apply t2
		       (append (sublist args u1 u)
			       (sublist args (fix:+ u d1) n))))
	     (man 'point-prototype)))
	  (make-tensor-field u d
			     the-product
			     `(t:* ,(diffop-name t1)
				   ,(diffop-name t2))
			     man))))))

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
	    (simplify-structure-function
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
			(basis->vector-basis basis))
	     ((tensor-manifold t) 'point-prototype)))
	  (make-tensor-field nu nd
			     the-contraction
			     `(contract ,(diffop-name t)
					,up-index ,down-index)
			     (tensor-manifold t)))))))

(define (invert-metric g coordinate-system)
  (assert (tensor-field? g) "Not a tensor field.")
  (let ((type (tensor-type g)))
    (let ((u (car type)) (d (cdr type)))
      (define ((the-inverted-metric w1 w2) m)
	(let* ((coeffs
		((tensor-field->coefficient-structure g coordinate-system) m))
	       (icoeffs
;		#|(s:map/r simplify-numerical-expression (s:invert coeffs))
;		|#
		(s:invert coeffs))
	       (gi
		((coefficient-structure->tensor-field icoeffs
						      coordinate-system)
		 w1 w2)))
	  ((simplify-structure-function gi
					((tensor-manifold g)
					 'point-prototype))
	   m)))
      (assert (and (fix:= u 0) (fix:= d 2)) "Not a 2-down metric")
      (make-tensor-field d u
			 the-inverted-metric
			 `(invert ,(diffop-name g))
			 (tensor-manifold g)))))

(define (Christoffel g coordinate-system)
  (let ((gi (* 1/2 (invert-metric g coordinate-system)))
	(Dg (t:comma g coordinate-system))
	(basis (coordinate-system 'coordinate-basis)))
    (define (the-Christoffel wi ek el)
      (simplify-structure-function
       (s:sigma/r
	(lambda (wm em)
	  (* (gi wi wm)
	     (- (+ (Dg em ek el) (Dg em el ek))
		(Dg ek el em))))
	(basis->1form-basis basis)
	(basis->vector-basis basis))
       ((tensor-manifold g) 'point-prototype)))
    (make-tensor-field 1 2 the-Christoffel
		       `(Christoffel ,(diffop-name g))
		       (tensor-manifold g))))

;;; add memoization on internal functions when memoizer is fixed to gc
;;; useless memoizers.

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
	(make-operator (linear-memoize-1arg nabla_v)
		       `(nabla ,(diffop-name v)))))
    (linear-memoize-1arg nabla)))


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
	(simplify-structure-function
	 (+ (- (dGamma w x v u) (dGamma w x u v))
	    (- (GG w x v u) (GG w x u v)))
	 ((tensor-manifold metric-tensor) 'point-prototype)))
      (make-tensor-field 1 3
			 Riemann
			 `(Riemann ,(diffop-name metric-tensor))
			 (tensor-manifold metric-tensor)))))

;;; procedures to have results memoized

(memoize-procedure! 'vf->tf 'linear generic-environment)

(memoize-procedure! 'ff->tf 'linear generic-environment)

(memoize-procedure! 't:comma 'linear generic-environment)

(memoize-procedure! 'tensor-field->coefficient-structure
		    'linear generic-environment)

(memoize-procedure! 'coefficient-structure->tensor-field
		    'linear generic-environment)

(memoize-procedure! 'tensor-product 'linear generic-environment)

(memoize-procedure! 'contract 'linear generic-environment)

(memoize-procedure! 'invert-metric 'linear generic-environment)

(memoize-procedure! 'Christoffel 'linear generic-environment)

(memoize-procedure! 't:covariant-derivative 'linear generic-environment)

(memoize-procedure! 'Riemann-tensor 'linear generic-environment)

