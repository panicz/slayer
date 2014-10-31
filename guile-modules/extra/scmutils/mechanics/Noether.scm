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

;;; Noether Theorem Support

;;; F-tilde is a parametric coordinate transformation that given
;;; parameters takes a state and returns transformed coordinates.   
;;; F-tilde may take an arbitrary number of real-valued parameters.
;;; F-tilde applied to zeros is the coordinate selector:  It takes a
;;; state and returns the coordinates.  The hypothesis of Noether's
;;; theorem is that the Lagrangian is invariant under the
;;; transformation for all values of the parameters.

;;; (D (lambda parms (compose L (F->C (apply F-tilde parms))))) = 0

(define (Noether-integral L F-tilde)
  (let ((zero-parameters (make-list (car (arity F-tilde)) 0)))
    (* ((partial 2) L) (apply (D F-tilde) zero-parameters))))

;#|
;(define ((L-central-rectangular m V) local)
;  (let ((q (coordinate local))
;        (v (velocity local)))
;    (- (* 1/2 m (square v))
;       (V (sqrt (square q))))))
;
;(define (F-tilde theta phi psi)
;  (compose (Rx theta)
;	   (Ry phi)
;	   (Rz psi)
;	   coordinate))
;
;(pe ((Noether-integral
;      (L-central-rectangular 'm (literal-function 'Vr))
;      F-tilde)
;     (up 't
;	 (up 'x 'y 'z)
;	 (up 'vx 'vy 'vz))))
;(down (+ (* -1 m vy z) (* m vz y))
;      (+ (* m vx z) (* -1 m vz x))
;      (+ (* -1 m vx y) (* m vy x)))
;|#

