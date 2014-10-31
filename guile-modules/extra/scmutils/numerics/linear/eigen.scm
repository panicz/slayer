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

;;;; Simple code to find eigenvalues and eigenvectors of small systems.


(define* (matrix->eigenvalues matrix #:optional (expand-multiplicities? #t))
;  (if (default-object? expand-multiplicities?)
;      (set! expand-multiplicities? #t))
  (let ((x (generate-uninterned-symbol "x")))
    (let ((poly
	   (m:determinant
	    (g:- matrix (g:* x (m:make-identity (m:dimension matrix)))))))
      (poly:expression-> (expression poly)
			 (lambda (pcf syms)
			   (poly->roots pcf expand-multiplicities?))))))


(define* (real-matrix->eigenvalues-eigenvectors matrix #:optional cutoff)
  (if (default-object? cutoff)
      (set! cutoff (* 1000 *machine-epsilon*)))
  (let ((eigenvalues (matrix->eigenvalues matrix #f)))
    (map (lambda (root)
	   (let ((m (car root))		; multiplicity
		 (x (cdr root)))
	     (svd (g:- matrix (g:* x (m:make-identity (m:dimension matrix))))
		  (lambda (U SIGMA V W)
		    (let ((n (vector-length W))
			  (minw
			   (* cutoff (apply max (map abs (vector->list W))))))
		      (cons x
			    (let lp ((i 0))
			      (cond ((fix:= i n) '())
				    ((< (abs (vector-ref W i)) cutoff)
				     (cons (m:nth-col V i)
					   (lp (fix:+ i 1))))
				    (else
				     (lp (fix:+ i 1)))))))))))
	 eigenvalues)))

(define (matrix->eigenvalues-eigenvectors matrix)
  (let ((eigenvalues (matrix->eigenvalues matrix #f)))
    (map (lambda (root)
	   (let ((m (car root))		; multiplicity
		 (x (cdr root)))
	     (cons x
		   (lu-null-space
		    (g:- matrix
			 (g:* x (m:make-identity (m:dimension matrix))))))))
	 eigenvalues)))

;#|
;;;; For example, this system has 3 distinct eigenvalues and
;;;; corresponding eigenvectors:
;
;(pp (matrix->eigenvalues-eigenvectors
;     (matrix-by-rows '(2 -1 0)
;		     '(-1 2 -1)
;		     '(0 -1 2))))
;((.585786437626913 #(.5000000000000014 .7071067811865455 .5000000000000014))
; (2. #(-.7071067811865475 0. .7071067811865475))
; (3.414213562373087 #(.5000000000000014 -.7071067811865455 .5000000000000014)))
;
;
;;;; For systems with multiplicity the multiple eigenvectors appear
;;;; with the multiple root:
;
;(pp (matrix->eigenvalues-eigenvectors
;     (matrix-by-rows '(0 0 0)
;		     '(0 1 0)
;		     '(0 0 1))))
;((0. #(1 0 0)) (1. #(0 0 1) #(0 1 0)))
;
;;;; A real example: the standard map at a hyperbolic point.
;
;(pp (matrix->eigenvalues-eigenvectors
;     (a^m_n->mmn
;      (let ((K 1))
;	((D (lambda (v)
;	      (let ((x (ref v 0))
;		    (y (ref v 1)))
;		(let ((yp  (+ y (* K (sin x)))))
;		  (up (+ x yp) yp)))))
;	 (up 0 0))))))
;((.38196601125010315 #(-.525731112119133 .8506508083520402))
; (2.6180339887498967 #(.8506508083520401 .5257311121191331)))
;
;
;;;; Pavel's test
;
;(pp (matrix->eigenvalues-eigenvectors
;     (matrix-by-rows '(1 1)
;		     '(0 1))))
;((1. #(1 0)))
;
;(pp (matrix->eigenvalues-eigenvectors
;     (matrix-by-rows '(13 -4 2)
;		     '(-4 13 -2)
;		     '(2 -2 10))))
;((9. #(-.4472135954999579 0. .8944271909999159)
;     #(.7071067811865475 .7071067811865475 0.))
; (17.999999999999954
;  #(.6666666666666701 -.6666666666666624 .33333333333333504)))
;
;(pp (matrix->eigenvalues-eigenvectors
;     (matrix-by-rows '(1 2 3)
;		     '(4 5 6)
;		     '(7 8 9))))
;
;((0.
;  #(.4082482904638631 -.8164965809277261 .4082482904638631))
; (-1.1168439698070243
;  #(-.7858302387420639 -.08675133925663416 .6123275602288135))
; (16.116843969807025
;  #(.2319706872462861 .5253220933012344 .8186734993561811)))
;
;
;(pp (matrix->eigenvalues-eigenvectors
;     (matrix-by-rows '(2 0 0 0)
;		     '(1 2 0 0)
;		     '(0 0 2 0)
;		     '(0 0 0 2))))
;((2. #(0. 0. 0. 1.) #(0. 0. 1. 0.) #(0. 1. 0. 0.)))
;
;
;(pp (matrix->eigenvalues-eigenvectors
;     (matrix-by-rows '(2 0 0 0)
;		     '(1 2 0 0)
;		     '(0 0 2 0)
;		     '(0 0 1 2))))
;((2. #(0. 0. 0. 1.) #(0. 1. 0. 0.)))
;
;|#
