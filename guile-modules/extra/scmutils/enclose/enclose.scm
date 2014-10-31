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

(declare (usual-integrations))

(define (lambda->numerical-procedure lexp)
  (compile-and-run-numerical lexp numerical-environment))

(define (generic-procedure->numerical-procedure proc #!optional arities)
  (if (default-object? arities) (set! arities 1))
  (compile-and-run-numerical (lambdafy arities proc) numerical-environment))


;;; Used by find-path.

(define (lambda->interpreted-generic-procedure lexp)
  (eval lexp generic-environment))

(define (abstract-to-function vars expression)
  (lambda->interpreted-generic-procedure
   `(lambda ,vars ,expression)))

;#|
;(define (lambda->user-procedure lexp)
;  (compile-and-run-sexp lexp user-initial-environment))
;
;
;(define (lambda->generic-procedure lexp)
;  (compile-and-run-sexp lexp generic-environment '()))
;
;(define (lambda->symbolic-procedure lexp)
;  (compile-and-run-sexp lexp generic-environment '()))
;
;
;(define (lambda->interpreted-user-procedure lexp)
;  (eval lexp user-initial-environment))
;
;(define (lambda->interpreted-numerical-procedure lexp)
;  (eval lexp numerical-environment))
;
;
;(define (lambda->interpreted-symbolic-procedure lexp)
;  (eval lexp generic-environment))
;
;
;
;(define (symbolic-procedure->lambda proc)
;  (let ((a (procedure-arity proc)))
;    (if (equal? (car a) (cdr a))
;	(s-p->l-a a proc)
;	(error "Unknown arity -- symbolic-procedure->lambda" proc))))
;
;(define (s-p->l-a arity proc)
;  (let ((gens (generate-list arity
;			     (lambda (i)
;			       (generate-uninterned-symbol 'x)))))
;    `(lambda ,gens
;       ,(apply proc gens))))
;|#