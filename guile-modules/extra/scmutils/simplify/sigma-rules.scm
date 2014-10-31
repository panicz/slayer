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

;;; (load "/usr/local/scmutils/src/simplify/sigma-rules" rule-environment)


(define simple-base-case?
  (disjunction exact-zero? exact-one?))

(define sigma-induction
  (rule-system

   ( (sigma (? f) (? lb simple-base-case?) (+ 1 (? n)))
     none
     (+ (sigma (: f) (: lb) (: n)) ((: f) (: (+ n 1)))) )

   ( (sigma (? f) (? lb) (? lb))
     none
     ((: f) (: lb)) )

   ))

;;; ((access sigma-induction rule-environment) '(sigma cube 1 (+ 1 n)))
;;; ;Value: (+ (sigma cube 1 n) (cube (+ 1 n)))

;;; ((access sigma-induction rule-environment) '(sigma cube 3 3))
;;; ;Value: (cube 3)

(define (sigma-simplify exp)
  ((simplify-until-stable sigma-induction simplify-and-flatten) exp))

;;; (sigma-simplify '(sigma cube 1 (+ 1 n)))
;;; ;Value: (+ 1 (expt n 3) (* 3 (expt n 2)) (* 3 n) (sigma cube 1 n))
