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

;;;; Scmutils kernel loader

;;; Useful universal utilities

(load "numeric.scm" )
(load "utils.scm"   )
(load "iterat.scm"  )
(load "express.scm" )

;;; The following define the generic operators

(load "ghelper.scm" )
(load "generic.scm" )
(load "mathutil.scm")
(load "strutl.scm"  )

;;; Magic apply extension to allow application
;;;  of things, such as vectors and numbers, that
;;;  are not legal Scheme procedures.

;(load "extapply.scm")
;;; Disable for system debugging.
;;; (set! *enable-generic-apply* #f)
;;; Enable for mechanics.
;(set! *enable-generic-apply* #t)
(define (with-literal-apply-enabled thunk) 
  (thunk))

;;; GHELPER is needed to load specific types
;;;  Lookup is in reverse order, so put numbers last

;;; Support for loading types.
(load "types.scm"   )
;;(load "/usr/local/scmutils/src/kernel/types.scm.~21~.scm"   )

;;;(define (diff-memoize-1arg f) f)
;;;(define (diff-memoize-2arg f) f)
;;;(define (diff-memoize f) f)
(define (diff-memoize-1arg f) (linear-memoize-1arg f))
(define (diff-memoize-2arg f) (linear-memoize f))
(define (diff-memoize f) (linear-memoize f))
;;;(define (diff-memoize f) (hash-memoize f))

(load "matrices.scm")
(load "quaternion.scm")
(load "pseries.scm" )	; requires streams
(load "modarith.scm")

(load "diff.scm"    )
(load "deriv.scm"   )
(load "operator.scm")
(load "function.scm")

(load "numbers.scm" )
;;; The following two need to be loaded after NUMBERS, 
;;;  because they use MAKE-NUMERICAL-COMBINATION.
(load "vectors.scm" )
(load "structs.scm" )

;;; Literal-construction.
(load "numsymb.scm" )

;;; must come after numsymb
;;; Heuristic rounding 
;(load "heuristic.scm")		; uses scheme rational type

;;; Sets up generic environment
(load "genenv.scm"  )		
; not currently replacing + with g:+ etc
; but here are some symbols not used in standard scheme:
(define D g:derivative)
(define partial g:partial)
(define partial-derivative g:partial-derivative)
(define ref g:ref)

;(load "custom-repl.scm")	; uses environments
