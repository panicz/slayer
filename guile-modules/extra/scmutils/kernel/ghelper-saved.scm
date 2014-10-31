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

;;;;         Generic Operator Dispatch

(declare (usual-integrations))

;;; Generic operator dispatch is implemented here by a two-level
;;;  table.  The operators are represented by atomic symbols, so 
;;;  an ASSQ alist structure is appropriate.  This may be optimized by
;;;  a hash table if necessary.  The next level is implemented by a
;;;  discrimination list, where the arguments passed to the operator
;;;  are examined by predicates that are supplied at the point of
;;;  attachment of a handler (by ASSIGN-OPERATION).

(define (generic-apply-1 operator x)
  (let ((args (list x)))
    (lookup-operation operator args (apply-to args) no-way-known)))

(define (generic-predicate-1 pred x)
  (let ((args (list x)))
    (lookup-operation pred args (apply-to args) (lambda (pred args) #f))))

(define (generic-apply-2 operator x y)
  (let ((args (list x y)))
    (lookup-operation operator args (apply-to args) no-way-known)))

(define (generic-predicate-2 pred x y)
  (let ((args (list x y)))
    (lookup-operation pred args (apply-to args) (lambda (pred args) #f))))

(define (generic-apply operator .  arguments)
  (lookup-operation operator arguments (apply-to arguments) no-way-known))

(define (generic-apply-default-operation 
	 default-operation operator .  arguments)
  (lookup-operation operator arguments (apply-to arguments)
		    (lambda (operator args)
		      (default-operation args))))

(define (generic-apply-default default-value operator .  arguments)
  (lookup-operation operator
		    arguments
		    (apply-to arguments)
		    (lambda (operator args)
		      default-value)))

(define (generic-predicate pred . arguments)
  (lookup-operation pred
		    arguments
		    (apply-to arguments)
		    (lambda (pred args) #f)))


(define ((apply-to args) proc)
  (apply proc args))

(define *the-operator-table* (make-eq-hash-table 50))

(define (lookup-operation operator arguments do-it no-way-known)
  (let ((v (hash-table/get *the-operator-table* operator #f)))
    ;; v is either #f or a discrimination tree.
    (if v
	(discriminate-keys (cdr v)
			   arguments
			   (lambda ()
			     (no-way-known operator arguments))
			   (lambda (tree fail)
			     (if (procedure? tree)
				 (do-it tree)
				 (fail))))
        (error "Unknown generic operator" operator))))

(define (discriminate-keys tree keys fail succeed)
  (if (null? keys)
      (succeed tree fail)
      (let predlp ((dlist tree))
	(cond ((null? dlist) (fail))
	      (((caar dlist) (car keys))
	       (discriminate-keys (cdar dlist) (cdr keys)
				  (lambda () (predlp (cdr dlist)))
				  succeed))
	      (else
	       (predlp (cdr dlist)))))))


;;; To make an entry in the table we must extend the table in two
;;;  ways: We need to add the operator, if necessary, and we need to
;;;  add the argument-predicates.  A predicate is supplied for each
;;;  argument.  They must be tensor-conjoined.

(define (assign-operation operator handler . argument-predicates)
  ;;(assert (fix:= (procedure-arity handler) (length argument-predicates)))
  (let ((tree
	 (or (hash-table/get *the-operator-table* operator #f)
	     (list '*tree*))))
    (discrimination-tree/put!
		    tree
		    argument-predicates
		    handler)
    (hash-table/put! *the-operator-table* operator tree))
  'done)

(define (discrimination-tree/put! tree preds value)
  (cond ((null? preds)
	 (set-cdr! tree value))
	(else
	 (let ((v (assq (car preds) (cdr tree))))
	   (if v
	       (discrimination-tree/put! v (cdr preds) value)
	       (let ((v (cons (car preds) '())))
		 (set-cdr! tree (cons v (cdr tree)))
		 (discrimination-tree/put! v (cdr preds) value)))))))

;;; Failures make it to here.  Time to DWIM, with apologies to Warren Teitelman.
;;;  Can we look at some argument as a default numerical expression?
				    
(define (no-way-known operator arguments)
  (let ((new-arguments (map dwim arguments)))
    (if (equal? arguments new-arguments)
	(error "Generic operator inapplicable" operator arguments)
	(apply generic-apply operator new-arguments))))

(define (dwim argument)
  (if (pair? argument)
      (cond ((memq (car argument) type-tags)
	     argument)
	    ((memq (car argument) generic-numerical-operators)
	     (apply (eval (car argument) generic-environment)
		    (cdr argument)))
	    (else
	     argument))
      argument))
