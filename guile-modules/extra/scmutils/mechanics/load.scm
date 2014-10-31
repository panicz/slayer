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

;;;(load "lag" generic-environment)

(set-current-module generic-environment)

(load "../units/constants.scm" )


;;; Chapter 1

(load "universal.scm" )

(load "Lagrangian.scm" )

(load "action.scm" )

(load "Lagrangian-evolution.scm" )

(load "gamma-bar.scm" )

(load "Lagrangian-transformations.scm" )

(load "Noether.scm" )


;;; Chapter 2

(load "rigid.scm" )


;;; Chapter 3

(load "Hamiltonian.scm" )

(load "Routhian.scm" )

(load "Hamiltonian-evolution.scm" )

(set-current-module  scmutils-base-environment)
(load "sections.scm" )
(set-current-module generic-environment)


;;; Chapter 4

(load "qualitative.scm" )


;;; Chapter 5

(load "point-transformation.scm" )

(load "canonical.scm" )

(load "symplectic.scm" )

(load "dual.scm" )

(load "time-varying.scm" )

(load "generating-functions.scm" )

(load "time-evolution.scm" )

(load "Lie-transform.scm" )


;;; Chapter 6

(load "pendulum.scm" )

(set-current-module scmutils-base-environment)
