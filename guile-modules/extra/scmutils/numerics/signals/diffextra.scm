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



(define diff:make-rectangular
  (diff:binary-op g:make-rectangular
		  (lambda (x y) 1)
		  (lambda (x y) 1)))

(define diff:real-part
  (diff:unary-op g:real-part g:real-part))

(define diff:imag-part
  (diff:unary-op g:imag-part g:imag-part))


(assign-operation 'make-rectangular
		  diff:make-rectangular
		  differential? differential?)

(assign-operation 'real-part diff:real-part differential?)

(assign-operation 'imag-part diff:imag-part differential?)


;#|
;(define (all-pass-pair omega0 zeta)
;  (define (ap omega)
;    (let* ((fratio (/ omega omega0))
;	   (rp (- 1 (square fratio)))
;	   (ip (* 2 +i zeta fratio)))
;      (/ (- rp ip) (+ rp ip))))
;  ap)
;
;(define H
;  (* (all-pass-pair 315 0.066)
;     (all-pass-pair 943 0.033)
;     (all-pass-pair 1888 0.058)))
;
;(plot-trace 4
;	    (angle
;	     (sigfun:make (compose H f->omega)
;			  (sigfun:make-span -400 +400)))
;	    #t)
;
;(define ((group-delay H) omega)
;  (let* ((z (H omega))
;	 (x (real-part z))
;	 (y (imag-part z))
;	 (r (+ (square x) (square y)))
;	 (Hp (derivative H))
;	 (dz (Hp omega))
;	 (dx (real-part dz))
;	 (dy (imag-part dz)))
;    (/ (- (* y dx) (* x dy)) r)))
;
;(plot-trace 4
;	    (sigfun:make (compose (group-delay H) f->omega)
;			 (sigfun:make-span -400 +400))
;	    #t)
;
;|#

