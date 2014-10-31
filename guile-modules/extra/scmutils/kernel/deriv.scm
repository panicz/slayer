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

;;;;                 General Derivative Procedures

;(declare (usual-integrations))

;;;     In DIFF.SCM we define the primitive mechanism extending the
;;; generic operators for differential quantities.  We also defined
;;; DIFF:DERIVATIVE, the procedure that produces the derivative
;;; function of a real-valued function of a real argument.  Here we
;;; use this mechanism to build derivatives of systems with structured
;;; arguments and structured values.  The basic rule is that a
;;; derivative function produces a value which may be multiplied by an
;;; increment of the argument to get a linear approximation to the
;;; increment in the function.

;;; Let's start with functions on Euclidean space:
;;;
;;;           f
;;;     R^n -----> R^m
;;;
;;; The derivative Df of this function is a function defined on R^n.
;;; It maps incremental vectors in R^n to incremental vectors in R^m.
;;;
;;;           Df          df
;;;     R^n -----> (R^n -----> R^m)
;;;
;;; It is only for these Euclidean spaces that we can identify the
;;; manifold with its tangent space at each point.  This will be a
;;; problem we will get back to later.  Note that df is a linear
;;; function, so it can be represented by an mXn matrix. (That is, 
;;; one with m rows and n columns.)

;;; One first try is to reduce the problem to simple derivatives.
;#|
;(define (deriv:euclidean f)
;  (define (a-euclidean-derivative v)
;    (cond ((vector? v)
;	   (let ((a
;		  (v:generate (vector-length v)
;		    (lambda (i)
;		      (simple-derivative-internal
;		       (lambda (xi)
;			 (f (vector-with-substituted-coord v i xi)))
;		       (vector-ref v i))))))
;	     (if (vector? (vector-ref a 0))
;		 (array->matrix (transpose-array a))
;		 a)))
;	  ((abstract-vector? v)
;	   ???)
;	  ((numerical-quantity? v)
;	   (simple-derivative-internal f v))
;	  (else
;	   (error "Bad argument to derivative" f v))))
;  a-euclidean-derivative)
;|#
;;; In order to implement derivatives with respect to abstract
;;; quantities we need to create more types -- differential vector,
;;; differential matrix, etc?  Let's attack that later.

;;; One thing that we notice is funny about this procedure is the
;;; strange conversion of the vector of vectors into a transposed
;;; matrix.  This is because we are not keeping the up/down parity of
;;; indices here.  We could do better if we realized that a row vector
;;; of column vectors (which we are naturally creating) is the same as
;;; the column of rows that is the canonical form for a matrix.

;;; We generalize this Euclidean-space derivative so that we may pass
;;; in an arbitrary structure of nested vectors.

(define (deriv:euclidean-structure f)
    (define (sd g v)
      (cond ((structure? v)
	     (s:generate (s:length v) (s:opposite v)
			 (lambda (i)
			   (sd (lambda (xi)
				 (g (s:with-substituted-coord v i xi)))
			       (s:ref v i))))) 
	    ((or (numerical-quantity? v)
		 (abstract-quantity? v))
	     (simple-derivative-internal g v))
	    (else
	     (error "Bad structure -- DERIV:EUCLIDEAN-STRUCTURE"
		    g v))))
    (define (a-euclidean-derivative v)
      (sd f v))
    a-euclidean-derivative)

;#|
;(define (deriv:euclidean-structure f)
;  (define (sd g v)
;    (cond ((structure? v)
;	   (s:generate (s:length v) (s:opposite v)
;		       (lambda (i)
;			 (sd (lambda (xi)
;			       (g (s:with-substituted-coord v i xi)))
;			     (s:ref v i))))) 
;	  ((or (numerical-quantity? v)
;	       (abstract-quantity? v))
;	   (simple-derivative-internal g v))
;	  (else
;	   (error "Bad structure -- DERIV:EUCLIDEAN-STRUCTURE"
;		  g v))))
;  (define (a-euclidean-derivative v)
;    (fluid-let ((differential-tag-count differential-tag-count))
;      (sd f v)))
;  a-euclidean-derivative)
;
;;;; The fluid let greatly improves the efficiency of the system by
;;;; reducing more intermediate expressions to a canonical form, but it
;;;; causes the following bug:
;
;(pe ((simple-derivative-internal
;      (lambda (eps)
;	 (lambda (t)
;	   ((D (* cos eps)) t)))
;      'e)
;     't))
;(* -1 (sin t)) ;; correct
;
;
;(pe (((D
;       (lambda (eps)
;	 (lambda (t)
;	   ((D (* cos eps)) t))))
;      'e)
;     't))
;0	      ;; wrong!
;
;;;; To recover this idea see custom-repl.scm
;|#

;;; We ignore abstract structures, for the nonce.
;;; There are structures.  A structure is like a vector, but it is
;;; either UP or DOWN.  S:GENERATE takes an argument that gives the
;;; UP/DOWN type.  S:OPPOSITE computes the toggle of that bit.

;#|;;; Harmonic oscillator example
;
;(pp
; (expression
;  (let ((k (literal-number 'k)) (m (literal-number 'm)))
;    ((deriv:euclidean-structure
;      (lambda (v)
;	(let ((t (s:ref v 0))
;	      (q (s:ref v 1))
;	      (p (s:ref v 2)))
;	  (+ (/ (* p (m:transpose p))
;		(* 2 m))
;	     (* 1/2 k (* (m:transpose q) q))
;	     (sin t)))))
;     (up (literal-number 't)
;	 (up (literal-number 'x)
;	     (literal-number 'y))
;	 (down (literal-number 'px)
;	       (literal-number 'py)))))))
;(pp
; (expression
;  (let ((k (literal-number 'k)) (m (literal-number 'm)))
;    ((deriv:euclidean-structure
;      (lambda (v)
;	(let ((t (s:ref v 0))
;	      (q (s:ref v 1))
;	      (p (s:ref v 2)))
;	  (g:+ (g:/ (g:square p)
;		(g:* 2 m))
;	     (g:* 1/2 k (g:square q))
;	     (g:sin t)))))
;     (up (literal-number 't)
;	 (up (literal-number 'x)
;	     (literal-number 'y))
;	 (down (literal-number 'px)
;	       (literal-number 'py)))))))
;(down (cos t)
;      (down (* (* 1/2 k) (+ x x))
;	    (* (* 1/2 k) (+ y y)))
;      (up (* (/ 1 (* 2 m)) (+ px px))
;	  (* (/ 1 (* 2 m)) (+ py py))))
;|#

;;; Once we have this, we can implement derivatives of multivariate
;;; functions by wrapping their arguments into an UP-STRUCTURE for
;;; differentiation by DERIV:EUCLIDEAN-STRUCTURE.

(define (deriv:multivariate-derivative f)
  (let ((a (g:arity f)))
    (cond ((equal? a *at-least-zero*)
	   (lambda args
	     (let ((ans
		    ((deriv:euclidean-structure
		      (lambda (s) (g:apply f (up-structure->list s))))
		     (list->up-structure args))))
	       (if (and (null? (cdr args)) ;one structure argument
			(structure? (car args)))
		   (g:ref ans 0)	   ;flush a level of structure
		   ans))))
	  ((equal? a *exactly-zero*)
	   (lambda () :zero))
	  ((equal? a *at-least-one*)
	   (lambda (x . y)
	     ((deriv:euclidean-structure
	       (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (cons x y)))))
	  ((equal? a *exactly-one*)
	   (deriv:euclidean-structure f))
	  ((equal? a *at-least-two*)
	   (lambda (x y . z)
	     ((deriv:euclidean-structure
	       (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (cons* x y z)))))
	  ((equal? a *exactly-two*)
	   (lambda (x y)
	     ((deriv:euclidean-structure
	       (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (list x y)))))
	  ((equal? a *at-least-three*)
	   (lambda (u x y . z)
	     ((deriv:euclidean-structure
	       (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (cons* u x y z)))))
	  ((equal? a *exactly-three*)
	   (lambda (x y z)
	     ((deriv:euclidean-structure
	       (lambda (s) (g:apply f (up-structure->list s))))
	      (list->up-structure (list x y z)))))
	  ((equal? a *one-or-two*)
	   (lambda* (x #:optional y)
	     (if (default-object? y)
		 ((deriv:euclidean-structure f) x)
		 ((deriv:euclidean-structure
		   (lambda (s) (g:apply f (up-structure->list s))))
		  (list->up-structure (list x y))))))
	  (else
	   (lambda args
	     (let ((ans
		    ((deriv:euclidean-structure
		      (lambda (s) (g:apply f (up-structure->list s))))
		     (list->up-structure args))))
	       (if (and (null? (cdr args)) ;one structure argument
			(structure? (car args)))
		   (g:ref ans 0)	   ;flush a level of structure
		   ans)))))))

;#|
;(define (deriv:derivative f)
;  (compose s:canonicalize (deriv:multivariate-derivative f)))
;
;(let ((k (literal-number 'k)) (m (literal-number 'm)))
;  (pp
;   (expression
;    ((deriv:derivative
;      (lambda (t q p)
;	(+ (/ (* p (m:transpose p))
;	      (* 2 m))
;	   (* 1/2 k (* (m:transpose q) q))
;	   (sin t))))
;     (literal-number 't)
;     (up (literal-number 'x)
;	 (literal-number 'y))
;     (down (literal-number 'px)
;	   (literal-number 'py))))))
;(down (cos t)
;      (down (* (* 1/2 k) (+ x x))
;	    (* (* 1/2 k) (+ y y)))
;      (up (* (/ 1 (* 2 m)) (+ px px))
;	  (* (/ 1 (* 2 m)) (+ py py))))
;|#


;;; Also, there is no problem implementing a general kind of partial
;;; derivative once we have this, because the partial derivative
;;; operation is nothing more than a selector operation composed with
;;; the result of the DERIV:MULTIVARIATE-DERIVATIVE.  Of course, there
;;; may be some special cases where these are computed without working
;;; out the whole shebang, but that is relatively easy.

;;; The selectors are numerical indices into the argument structure.
;#|
;(define (deriv:pd f selectors)
;  (let lp ((selectors selectors)
;	   (ans (deriv:multivariate-derivative f))) 
;    (if (null? selectors)
;	(compose s:canonicalize ans)
;	(lp (cdr selectors)
;	    (compose (lambda (s)
;		       (s:ref s (car selectors)))
;		     ans)))))
;|#

(define (deriv:pd f selectors)
  (let lp ((selectors selectors)
	   (ans (deriv:multivariate-derivative f))) 
    (if (null? selectors)
	ans
	(lp (cdr selectors)
	    (compose (lambda (s)
		       (s:ref s (car selectors)))
		     ans)))))

;;; (assign-operation 'partial-derivative  deriv:pd   function? any?)

(assign-operation 'partial-derivative
		  deriv:pd
		  (disjunction function? structure?)
		  any?)
