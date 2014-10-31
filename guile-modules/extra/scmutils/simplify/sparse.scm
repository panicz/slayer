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

;;;; Sparse flat form support for interpolation and gcd.

;(declare (usual-integrations))


;;; These algorithms work on the termlists of the flat multivariate
;;; polynomial form that is defined in the file fpf.  A flat sparse
;;; polynomial is a list of terms, each of which is the cons of an
;;; exponent list and a coefficient.  This package uses this format,
;;; but attempts to stand on its own: it does not use the fpf:xxx
;;; definitions.  This produces redundant code, but it is easier to
;;; work with the independent mechanism this way.


(define (sparse-exponents term) (car term))
(define (sparse-coefficient term) (cdr term))
(define (sparse-term exponents coefficient)
  (cons exponents coefficient))

(define (sparse-constant-term? term)
  (every zero? (sparse-exponents term)))

(define (sparse-constant? p)
  (and (fix:= (length p) 1)
       (sparse-constant-term? (car p))))

(define (sparse-one-term? t)
  (and (sparse-constant-term? t)
       (= (sparse-coefficient t) 1)))

(define (sparse-one? p)
  (and (sparse-constant? p)
       (= (sparse-coefficient (car p)) 1)))

(define (sparse-zero? p)
  (null? p))

(define (sparse-zero-term? t)
  (and (sparse-constant-term? t)
       (= (sparse-coefficient t) 0)))


(define (sparse-constant-term arity-n constant)
  (sparse-term (make-list arity-n 0) constant))

(define (sparse-one arity-n)
  (list (sparse-constant-term arity-n 1)))


(define (sparse-identity-term arity-n varnum)
  (sparse-term (generate-list arity-n
			      (lambda (i)
				(if (fix:= i varnum) 1 0)))
	       1))

(define (sparse-linear arity-n varnum root)
  (if (zero? root)
      (list (sparse-identity-term arity-n varnum))
      (list (sparse-identity-term arity-n varnum)
	    (sparse-constant-term arity-n (- root)))))


(define (sparse-term-> t1 t2)
  (sparse:>exponents? (sparse-exponents t1)
		      (sparse-exponents t2)))


;;; Graded Lexicographical Order

(define (sparse:>exponents? fs1 fs2)
  (let ((o1 (reduce fix:+ 0 fs1))
	(o2 (reduce fix:+ 0 fs2)))
    (cond ((fix:> o1 o2) #t)
	  ((fix:< o1 o2) #f)
	  (else
	   (let lp ((l1 fs1) (l2 fs2))
	     (cond ((null? l1) #f)
		   ((null? l2) #t)
		   ((fix:> (car l1) (car l2)) #t)
		   ((fix:< (car l1) (car l2)) #f)
		   (else (lp (cdr l1) (cdr l2)))))))))
;#|
;;;; Lexicographical Order
;
;(define (sparse:>exponents? fs1 fs2)
;  (let lp ((l1 fs1) (l2 fs2))
;    (cond ((null? l1) #f)
;	  ((null? l2) #t)
;	  ((fix:> (car l1) (car l2)) #t)
;	  ((fix:< (car l1) (car l2)) #f)
;	  (else (lp (cdr l1) (cdr l2))))))
;|#

(define (sparse-normalize poly term)
  (map (lambda (pterm)
	 (sparse-term
	  (map - (sparse-exponents pterm) (sparse-exponents term))
	  (/ (sparse-coefficient pterm) (sparse-coefficient term))))
       poly))

(define (sparse-scale poly term)
  (map (lambda (pterm)
	 (sparse-term
	  (map + (sparse-exponents pterm) (sparse-exponents term))
	  (* (sparse-coefficient pterm) (sparse-coefficient term))))
       poly))

(define (sparse-add xlist ylist)
  (let tloop ((xlist xlist) (ylist ylist))
    (cond ((null? xlist) ylist)
	  ((null? ylist) xlist)
	  (else
	   (let ((e1 (sparse-exponents (car xlist)))
		 (e2 (sparse-exponents (car ylist))))
	     (cond ((equal? e1 e2)
		    (let ((ncoeff (+ (sparse-coefficient (car xlist))
				     (sparse-coefficient (car ylist)))))
		      (if (= ncoeff 0)
			  (tloop (cdr xlist) (cdr ylist))
			  (cons (sparse-term e1 ncoeff)
				(tloop (cdr xlist) (cdr ylist))))))
		   ((sparse:>exponents? e1 e2)
		    (cons (car xlist) (tloop (cdr xlist) ylist)))
		   (else
		    (cons (car ylist) (tloop xlist (cdr ylist))))))))))

(define (negate-term t)
  (sparse-term (sparse-exponents t) (- (sparse-coefficient t))))

(define (sparse-multiply xlist ylist)
  (let lp ((xlist xlist))
    (if (null? xlist)
	'()
	(sparse-add (sparse-multiply-term (car xlist) ylist)
		    (lp (cdr xlist))))))

(define (sparse-multiply-term t x)
  (let ((exponents (sparse-exponents t))
	(coeff (sparse-coefficient t)))
    (map (lambda (term)
	   (sparse-term (map + exponents (sparse-exponents term))
			(* coeff (sparse-coefficient term))))
	 x)))

(define (sparse-abs p)
  (if (null? p)
      '()
      (if (< (sparse-coefficient (car p)) 0)
	  (map (lambda (term)
		 (sparse-term (sparse-exponents term)
			      (- (sparse-coefficient term))))
	       p)
	  p)))

(define (sparse-divide numerator-terms denominator-terms cont)
  (let ((dexps (sparse-exponents (car denominator-terms)))
	(dcoef (sparse-coefficient (car denominator-terms))))
    (define (dloop nterms cont)
      (if (null? nterms)
	  (cont '() '())
	  (let ((nexps (sparse-exponents (car nterms)))
		(ncoef (sparse-coefficient (car nterms))))
	    (cond ((&and (map >= nexps dexps)) ;monomial-divisible?
		   (let ((qt (sparse-term (map - nexps dexps)
					  (/ ncoef dcoef))))
		     (dloop
		      (sparse-add (cdr nterms)
		        (sparse-multiply-term (negate-term qt)
			  (cdr denominator-terms)))
		      (lambda (q r)
			(cont (sparse-add (list qt) q) r)))))
		  (else
		   (dloop (cdr nterms)
			  (lambda (q r)
			    (cont q
				  (sparse-add (list (car nterms))
						    r)))))))))
    (dloop numerator-terms cont)))

(define (sparse-divisible? n d)
  (null? (sparse-divide n d (lambda (q r) r))))

(define (fpf:->sparse p)
  (fpf:terms p))

;#|
;(define (divide-test q d r)
;  (let ((pq (fpf:expression-> q (lambda (p v) p)))
;	(pd (fpf:expression-> d (lambda (p v) p)))
;	(pr (fpf:expression-> r (lambda (p v) p))))
;    (let ((pn (fpf:+ (fpf:* pq pd) pr))
;	  (sq (fpf:->sparse pq))
;	  (sr (fpf:->sparse pr)))
;      (let ((sn (fpf:->sparse pn))
;	    (sd (fpf:->sparse pd)))
;	;; n = q*d + r
;	(sparse-divide sn sd
;		       (lambda (q r)
;			 (pp `((sn ,sn) = (q ,q) * (d ,sd) + (r ,r)))
;			 (if (and (equal? q sq) (equal? r sr))
;			     (pp #t)
;			     (pp `((sq ,sq) (sr ,sr))))))))))
;|#

;;; Evaluation of polynomials at argument vectors.

(define (sparse-evaluate p x)
  (apply +
	 (map (lambda (term)
		(* (sparse-coefficient term)
		   (apply *
			  (map expt
			       x
			       (sparse-exponents term)))))
	      p)))


;;; If x is smaller than the arity of p then the last vars are filled
;;; in by components of x, making a polynomial of lesser arity.

(define (sparse-evaluate> p x)
  (let* ((n (length x))
	 (arity (length (sparse-exponents (car p))))
	 (narity (- arity n)))
    (combine-like-terms
     (map (lambda (term)
	    (sparse-term (list-head (sparse-exponents term) narity)
			 (* (sparse-coefficient term)
			    (apply *
				   (map expt
					x
					(list-tail (sparse-exponents term)
						   narity))))))
	  p))))

;#|
;(print-expression
; (sparse-evaluate>
;  '(((2 3 0) . 3) ((1 1 1) . 1) ((0 0 1) . 4) ((0 0 0) . 1))
;  '(y z)))
;(((2) . (* 3 (expt y 3))) ((1) . (* y z)) ((0) . (* 4 z)) ((0) . 1))
;|#

(define (sparse-evaluate< p x)
  (let ((n (length x)))
    (combine-like-terms
     (map (lambda (term)
	    (sparse-term (list-tail (sparse-exponents term) n)
			 (* (sparse-coefficient term)
			    (apply *
				   (map expt
					x
					(list-head (sparse-exponents term)
						   n))))))
	  p))))

;#|
;(print-expression
; (sparse-evaluate<
;  '(((2 3 0) . 3) ((1 1 1) . 1) ((0 0 1) . 4) ((0 0 0) . 1))
;  '(x y)))
;(((1) . (+ 4 (* x y))) ((0) . (+ 1 (* 3 (expt x 2) (expt y 3)))))
;|#

(define (combine-like-terms terms)
  (merge-adjacent-terms (sort terms sparse-term->)))

(define (merge-adjacent-terms terms)
  (cond ((null? terms)
	 '())
	((null? (cdr terms))
	 (if (= (sparse-coefficient (car terms)) 0)
	     '()
	     terms))
	((equal? (sparse-exponents (car terms))
		 (sparse-exponents (cadr terms)))
	 (let ((coeff (+ (sparse-coefficient (car terms))
			 (sparse-coefficient (cadr terms)))))
	   (if (= coeff 0)
	       (merge-adjacent-terms (cddr terms))
	       (merge-adjacent-terms
		(cons (sparse-term (sparse-exponents (car terms)) coeff)
		      (cddr terms))))))
	(else
	 (cons (car terms)
	       (merge-adjacent-terms (cdr terms))))))
