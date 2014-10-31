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

;;; Hamiltonians look better if we divide them out.

(define *divide-out-terms* #t)

(define *fully-divide-out-terms* #t)

(define *heuristic-numbers* #f)

(define (canonicalize-numbers expr)
  (cond ((with-units? expr)
	 (with-si-units->expression expr))
	((pair? expr)
	 (cons (canonicalize-numbers (operator expr))
	       (map canonicalize-numbers (operands expr))))
	((and (number? expr) *heuristic-numbers*)
	 (heuristic-canonicalize-complex expr))
	(else
	 expr)))

(define (ham:simplify hexp)
  (cond ((and (quotient? hexp) *divide-out-terms*)
	 (cond ((sum? (symb:numerator hexp))
		(let ((d (symb:denominator hexp)))
		  (a-reduce symb:+
			    (map (lambda (n)
				   (default-simplify (symb:/ n d)))
				 (operands (symb:numerator hexp))))))
	       (else hexp)))
	((and (compound-data-constructor? hexp) *fully-divide-out-terms*)
	 (cons (operator hexp) (map ham:simplify (operands hexp))))
	(else hexp)))


;;; Equations are often prettier if we get rid of the denominators,
;;; but watch out for singularities.

(define (eqn:simplify hexp)
  (cond ((quotient? hexp)
	 (symb:numerator hexp))
	((matrix? hexp)
	 ((m:elementwise eqn:simplify) hexp))
	((vector? hexp)
	 ((v:elementwise eqn:simplify) hexp))
	(else hexp)))

(define (flush-derivative expr)
  (substitute derivative-symbol
	      'derivative
	      expr))

(define (flush-literal-function-constructors expr)
  (if (pair? expr)
      (if (eq? (car expr) 'literal-function)
	  (if (and (pair? (cadr expr)) (eq? (caadr expr) 'quote))
	      (flush-literal-function-constructors (cadadr expr))
	      (cadr expr))
	  (cons (flush-literal-function-constructors (car expr))
		(flush-literal-function-constructors (cdr expr))))
      expr))


(define *factoring* #f)

(define (simplify exp)
  (flush-derivative
   (flush-literal-function-constructors
    (ham:simplify
     ((if *factoring* poly:factor (lambda (expr) expr))
      (default-simplify exp))))))

(define *only-printing* #f)
(define *last-expression-printed* #f)


(define (prepare-for-printing expr simplifier)
  (set! *last-expression-printed* 
	(if (memq expr '(#t #f))
	    expr
	    (let ((nexpr
		   (canonicalize-numbers (expression expr))))
	      (simplifier nexpr))))
  *last-expression-printed*)



(define* (show-expression expr #:optional simplifier)
  (if (default-object? simplifier) (set! simplifier simplify))
  (prepare-for-printing expr simplifier)
  (cond (*only-printing*
	 (pp *last-expression-printed*))
	(else
	 (internal-show-expression *last-expression-printed*))))

(define* (print-expression expr #:optional simplifier)
  (if (default-object? simplifier)
      (set! simplifier simplify))
  (prepare-for-printing expr simplifier)
  (pp *last-expression-printed*))

(define pe print-expression)
(define se show-expression)


(define* (print-expression-prefix expr #:optional simplifier)
  (if (default-object? simplifier)
      (set! simplifier simplify))
  (prepare-for-printing expr simplifier)
  ((pp-line-prefix "; ") *last-expression-printed*))

(define pep print-expression-prefix)

(define* (print-expression-comment expr #:optional simplifier)
  (if (default-object? simplifier)
      (set! simplifier simplify))
  (prepare-for-printing expr simplifier)
  (newline)
  (display "#| Result:")
  (newline)
  (pp *last-expression-printed*)
  (display "|#"))

(define pec print-expression-comment)