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

;;;; Scmutils top-level loader

;(ge user-initial-environment)

;(add-subsystem-identification! "\n  ScmUtils" '("Mechanics " " Fall 2006"))

(define scmutils-base-environment
  (if (defined? 'scmutils-base-environment)
      (begin
	(set-current-module scmutils-base-environment)
	scmutils-base-environment)
      (current-module)))

;(load "general/comutils.scm"); scmutils-base-environment)

;(start-canonicalizing-symbols!)

;;; LOCAL-ASSIGNMENT should eventually be replaced with
;;; ENVIRONMENT-DEFINE when that has stabilized.

;(local-assignment scmutils-base-environment
;		  '*environment*
;		  'scmutils-base-environment)

;(local-assignment scmutils-base-environment
(define 
		  derivative-symbol
		  (string->symbol "D"))

(define (with-working-directory-pathname path thunk)
  (let ((old (getcwd)))
    (chdir path)
    (let ((result (thunk)))
      (chdir old)
      result)))


(define (in-scmutils-directory relative-path thunk)
  (with-working-directory-pathname
   relative-path
    thunk))

;(load-option 'hash-table)
;(load-option 'synchronous-subprocess)

;; This doesn't work because load must also get dll's into the microcode.
;;(load-option 'swat)

(load "compat.scm")

(load "general/load.scm")
(load "kernel/load.scm")

;;(environment-define system-global-environment
;;		    'generic-environment
;;		    (access generic-environment scmutils-base-environment))

(load "simplify/load.scm" )

;(define symbolic-environment
;  (access symbolic-environment scmutils-base-environment))
;(define rule-environment
;  (access rule-environment scmutils-base-environment))

(define symbolic-operators
  (hash-fold (lambda (key value prior) (cons key prior)) '() symbolic-operator-table))

(load "display/load.scm" )

;(load "enclose/load.scm" )
(define (lambda->numerical-procedure lexp)
  (primitive-eval lexp))

(load "numerics/load.scm")

(load "poly/load.scm" )


;(start-preserving-case!)

(load "kernel/litfun.scm" )

(load "units/load.scm" )      

(load "mechanics/load.scm")

(load "calculus/load.scm")





