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
;;;; Experiments to determine the story
;
;;;; V-on-M is a vector field on M  template  V(f)(m)
;;;; V-over-mu is a vector field over map mu 
;;;;   mu: N->M, f:M->R  template V(f)(n)
;;;; X is a vector field on N  template X(g)(n) where g:N->R
;;;; when mu = identity this reduces to the previous covariant-derivative
;
;(define ((vector-over-map->coef mu V-over-mu basis-on-M) n)
;  (let ((dual-basis (basis->1form-basis basis-on-M))
;	(m (mu n)))
;    (s:map/r (lambda (edual)
;	       ((edual 
;		 (procedure->vector-field
;		  (lambda (f) (lambda (m) ((V-over-mu f) n)))
;		  'foo))
;		m))
;	   dual-basis)))
;
;(define ((((covariant-derivative basis-on-M Christoffel-symbols mu:N->M) 
;	   X-on-N) 
;	  V-over-mu) 
;	 f-on-M)
;  (let ((vector-basis-on-M (basis->vector-basis basis-on-M)))
;    (let ((v-components (vector-over-map->coef mu:N->M V-over-mu basis-on-M))
;	  (x-components 
;	   (vector-over-map->coef mu:N->M
;				  ((differential mu:N->M) X-on-N)
;				  basis-on-M)))
;      (* (compose (vector-basis-on-M f-on-M) mu:N->M)
;	 (+ (X-on-N v-components)
;	    (* (* (compose Christoffel-symbols mu:N->M)
;		  x-components)
;	       v-components))))))
;#|
;(pe (let ((U (components->vector-field (lambda (x) 1) the-real-line))
;	  (mu:N->M (compose (S '->point)
;			    (up (literal-function 'theta)
;				(literal-function 'phi)))))
;      ((vector-over-map->coef 
;	mu:N->M
;	(((covariant-derivative 2-sphere-basis G-S2-1 mu:N->M)
;	  U)
;	 ((differential mu:N->M) U))				  
;	2-sphere-basis)
;       'tau)))
;(up (+ (* -1
;	  (cos (theta tau))
;	  (expt ((D phi) tau) 2)
;	  (sin (theta tau)))
;       (((expt D 2) theta) tau))
;    (+ (/ (* 2
;	     ((D theta) tau)
;	     (cos (theta tau))
;	     ((D phi) tau))
;	  (sin (theta tau)))
;       (((expt D 2) phi) tau)))
;|# |#
