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

(define fconstant
  (sigfun:make (constant 1)
	       (sigfun:make-span -25.6 25.6)))

(plot-trace 1 (magnitude fconstant))

(define tdelta
  (signal->time-function (frequency-function->signal fconstant)))

(plot-trace 2 (magnitude tdelta))

(define (train f n)
  (if (= n 0)
      f
      (let* ((span (sigfun:span f))
	     (period (- (sigfun:max span) (sigfun:min span)))
	     (shift (/ period (expt 2 (+ n 1)))))
	(sigfun:make (+ (sigfun:shift (train f (- n 1)) (- shift))
			(sigfun:shift (train f (- n 1)) (+ shift)))
		     span))))

(plot-trace 3 (train tdelta 3))