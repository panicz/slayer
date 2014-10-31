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

;;;;                   Abstract Calculus

;;; The purpose of this program is to give an interpretation to the
;;; common notation of differential calculus, where we see things
;;; notated by expressions such as (x y dx + y^2 dy) interpreted as
;;; differential forms and things notated by such expressions such as
;;; (x y d/dx + y^2 d/dy) interpreted as vector-field operators.

;;; We have x, y ... as coordinate functions.  There are vector fields
;;; that take manifold points and produce functions of functions on
;;; the manifold.  We define d/dx, d/dy ... as basis vector field
;;; operators (that take functions and produce functions on the
;;; manifold that give the directional derivative of the given
;;; function at the manifold point).  And we have dx, dy ...  as basis
;;; one forms (that take points and produce functions of vector
;;; fields).

;;; This convention enables the notation without the intervention of
;;; more complex generic arithmetic, since the first argument
;;; (Curryed) of any object is the manifold point.  However, it may be
;;; better to implement this so that vector fields take the functions
;;; first and the manifold point second and so that the forms take the
;;; vector field first and the manifold point second.  This kills an
;;; easy interpretation of the notation, requiring extension to
;;; generic arithmetic, but it may be more consistent with Spivak.

;;; This program is an implementation of calculus on Euclidean space.
;;; Definitions are independent of the coordinate systems, but we do
;;; not have more than one map to cover a space, so we don't quite
;;; have manifolds.
