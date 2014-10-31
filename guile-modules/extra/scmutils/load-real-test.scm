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

(ge user-initial-environment)

(add-identification! "ScmUtils" "Mechanics " " Summer 2004")

(define scmutils-base-environment
  user-initial-environment)

(load "general/comutils" scmutils-base-environment)

(start-canonicalizing-symbols!)

;;; LOCAL-ASSIGNMENT should eventually be replaced with
;;; ENVIRONMENT-DEFINE when that has stabilized.

(local-assignment scmutils-base-environment
		  '*environment*
		  'scmutils-base-environment)

(local-assignment scmutils-base-environment
		  'derivative-symbol
		  (string->symbol "D"))

(define (in-scmutils-directory relative-path thunk)
  (with-working-directory-pathname
      (merge-pathnames (pathname-as-directory relative-path)
		       (directory-pathname (current-load-pathname)))
    thunk))

(load-option 'hash-table)
(load-option 'synchronous-subprocess)

;;; This doesn't work because load must also get dll's into the microcode.
;;;(load-option 'swat)

(in-scmutils-directory "./general"
		       (lambda ()
			 (load "load" scmutils-base-environment)))
(in-scmutils-directory "./kernel"
		       (lambda ()
			 (load "load" scmutils-base-environment)))

(environment-define system-global-environment
		    'generic-environment
		    (access generic-environment scmutils-base-environment))

(in-scmutils-directory "./simplify"
		       (lambda ()
			 (load "load" scmutils-base-environment)))

(define symbolic-environment
  (access symbolic-environment scmutils-base-environment))
(define rule-environment
  (access rule-environment scmutils-base-environment))

(in-scmutils-directory "./display"
		       (lambda ()
			 (load "load" scmutils-base-environment)))

(in-scmutils-directory "./enclose"
		       (lambda ()
			 (load "load" scmutils-base-environment)))
(in-scmutils-directory "./numerics"
		       (lambda ()
			 (load "load" scmutils-base-environment)))
(in-scmutils-directory "./poly"
		       (lambda ()
			 (load "load" scmutils-base-environment)))


(start-preserving-case!)

(in-scmutils-directory "./kernel"
		       (lambda ()
			 (load "litfun" scmutils-base-environment)))

(in-scmutils-directory "./units"
		       (lambda ()
			 (load "load" scmutils-base-environment)))      

(in-scmutils-directory "./mech"
		       (lambda ()
			 (load "load" scmutils-base-environment)))

(ge generic-environment)


