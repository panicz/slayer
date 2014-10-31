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

;;;; Moments of a distribution

;;; First some specific moments of a vector

(define (v:mean v)
  (let ((n (vector-length v)))
    (let lp ((i 0) (sum 0))
      (if (fix:= i n)
	  (/ sum n)
	  (lp (fix:+ i 1)
	      (+ sum (vector-ref v i)))))))

(define (v:variance v)
  (define (square x) (* x x))
  (let ((n (vector-length v))
	(mean (v:mean v)))
    (let lp ((i 0) (sumsq 0) (sum 0))
      (if (fix:= i n)
	  (/ (- sumsq
		(/ (square sum)
		   n))
	     (fix:- n 1))
	  (let ((y (- (vector-ref v i) mean)))
	    (lp (fix:+ i 1)
		(+ sumsq (square y))
		(+ sum y)))))))
    
(define (v:standard-deviation v)
  (sqrt (v:variance v)))

(define (v:average-deviation v)
  (let ((n (vector-length v))
	(mean (v:mean v)))
    (let lp ((i 0) (sum 0))
      (if (fix:= i n)
	  (/ sum n)
	  (let ((y (abs (- (vector-ref v i) mean))))
	    (lp (fix:+ i 1)
		(+ sum y)))))))

;;; We can calculate them all at once

(define (v:moments v cont)
  ;; cont =  (lambda (mean var std skew kurt adev) ...)
  (define (square x) (* x x))
  (let ((n (vector-length v))
	(mean (v:mean v)))
    (let lp ((i 0) (sum 0) (sumsq 0) (sumcb 0) (sumqu 0) (asum 0))
      (if (fix:= i n)
	  (let* ((var (/ (- sumsq
			    (/ (square sum)
			       n))
			 (fix:- n 1)))
		 (std (sqrt var))
		 (skew (/ sumcb (* n var std)))
		 (kurt (- (/ sumqu (* n (square var))) 3.0))
		 (adev (/ asum n)))
	    (cont mean var std skew kurt adev))
	  (let* ((y (- (vector-ref v i) mean))
		 (yy (* y y))
		 (yyy (* y yy))
		 (yyyy (* y yyy)))
	    (lp (fix:+ i 1)
		(+ sum y)
		(+ sumsq yy)
		(+ sumcb yyy)
		(+ sumqu yyyy)
		(+ asum (abs y))))))))


;;; For lists

(define (mean l)
  (v:moments (list->vector l)
	     (lambda (mean var std skew kurt adev)
	       mean)))

(define (variance l)
  (v:moments (list->vector l)
	     (lambda (mean var std skew kurt adev)
	       var)))

(define (standard-deviation l)
  (v:moments (list->vector l)
	     (lambda (mean var std skew kurt adev)
	       std)))

(define (skewness l)
  (v:moments (list->vector l)
	     (lambda (mean var std skew kurt adev)
	       skew)))

(define (kurtosis l)
  (v:moments (list->vector l)
	     (lambda (mean var std skew kurt adev)
	       kurt)))

(define (average-deviation l)
  (v:moments (list->vector l)
	     (lambda (mean var std skew kurt adev)
	       adev)))

;;; Streams of data have running moments

(define (running-mean decay stream)
  (let loop ((sum (head stream))
	     (count 1)
	     (stream (tail stream)))
    (cons-stream (/ sum count)
		 (loop (+ (head stream) (* decay sum))
		       (+ 1 (* decay count))
		       (tail stream)))))

