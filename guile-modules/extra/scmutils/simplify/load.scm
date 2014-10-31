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

;;;; Simplifier loader

;;; Canonical simplifiers are based on Rational Canonical Form,
;;;  which is, in turn, based on Polynomial Canonical Form.

;;;The following two files must be loaded in the given order.
(load "pcf.scm")
(load "rcf.scm")


;;;; We need flattened polynomials to support rule-based simplifiers.

(load "fpf.scm" )


;;;; Canonical simplifiers are glued together with SIMPLIFY.

(load "simplify.scm" )

(load "split-poly.scm" )

;;;; Rule-based simplifiers

(load "symbenv.scm" )

(define rule-environment symbolic-environment)

;;;; (define (rule-memoize f) f)
(define (rule-memoize f) (linear-memoize-1arg f))
;;;; (define (rule-memoize f) (linear-memoize f))
;;;; (define (rule-memoize f) (hash-memoize f))
;;;; (define (rule-memoize f) (hash-memoize-1arg f))

(load "syntax.scm" )
(load "rule-syntax.scm" )
(load "matcher.scm" )
(load "rule-simplifier.scm" )
(load "rules.scm" )

;(for-each (lambda (name)
;	    #|
;	    ;; This code exports by copying the binding:
;	    (local-assignment 
;			      name
;			      (environment-lookup rule-environment
;						  name))
;	    |#
;	    ;; This code shares the binding:
;	    (environment-link-name 
;				   rule-environment
;				   name))
;	  '(;; Useful simplifiers
;	    ->poisson-form
;	    new-simplify
;	    easy-simplify
;	    full-simplify
;	    trigexpand
;	    trigcontract
;
;	    ;; Boolean simplifier controls.
;	    log-exp-simplify
;	    sqrt-expt-simplify
;	    inverse-simplify
;	    ignore-zero-simplify
;	    commute-partials-simplify
;
;	    log-exp-simplify?
;	    sqrt-expt-simplify?
;	    inverse-simplify?
;	    commute-partials?
;	    ))

(define (default-simplify exp)
  (new-simplify (expression exp)))

(load "sparse-load.scm" )
