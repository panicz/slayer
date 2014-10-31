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


;#|
;;;; Metric can be obtained for an embedded manifold.  For example:
;
;(define (((S2-metric R) coords) v1 v2)
;  (let ((df ((D ((S2 R) '->point)) coords)))
;    (dot-product (* df v1) (* df v2))))
;
;(pe (((S2-metric 'R) (up 'theta 'phi))
;     (up 'xi_1 'eta_1)
;     (up 'xi_2 'eta_2)))
;(+ (* (expt R 2) eta_1 eta_2 (expt (sin theta) 2))
;   (* (expt R 2) xi_1 xi_2))
;
;
;;;; More generally, for an embedded manifold and geometric vector
;;;; fields.
;
;(define ((metric v1 v2) pos)
;  (dot-product 
;   ((v1 identity) pos)
;   ((v2 identity) pos)))
;
;(define v1 (+ (* 'xi_1 d/dtheta) (* 'eta_1 d/dphi)))
;
;(define v2 (+ (* 'xi_2 d/dtheta) (* 'eta_2 d/dphi)))
;
;(pe ((met2 v1 v2)
;     (((S2 'R) '->point) (up 'theta 'phi))))
;(+ (* (expt R 2) eta_1 eta_2 (expt (sin theta) 2))
;   (* (expt R 2) xi_1 xi_2))
;|#
