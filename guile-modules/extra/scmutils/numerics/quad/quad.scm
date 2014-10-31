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



;;;     Fundamental quad manipulation 
;;;        given double arguments.

;;; Knuth's theorem B provides a method of adding 2 floats
;;;  to produce a sum and an error estimate.
;;;   It takes 6 floating operations and no comparisions.

(define (+B u v k)			;k=(lambda (sum error) ...)
  (let ((s (flo:+ u v)))
    (let ((up (flo:- s v)))
      (let ((vpp (flo:- s up)))
	(k s (flo:+ (flo:- u up) (flo:- v vpp)))))))


(define (-B u v k)			;k=(lambda (sum error) ...)
  (let ((s (flo:- u v)))
    (let ((up (flo:+ s v)))
      (let ((vpp (flo:- s up)))
	(k s
	   (flo:- (flo:- u up)
		  (flo:+ v vpp)))))))


;;; Multiplication of two floating operations, transcribed from 
;;;  Quinn & Tremaine 1990.

(define IEEE-p 53)			;see standard.
(define tremaine-j
  (ceiling (/ IEEE-p 2)))
(define tremaine-c
  (exact->inexact (int:+ (expt 2 tremaine-j) 1)))

(define (*T u v k)			;k=(lambda (prod error) ...)
  (let* ((up (flo:* u tremaine-c))
	 (u1 (flo:- u up))
	 (u1 (flo:+ u1 up))
	 (u2 (flo:- u u1))
	 (vp (flo:* v tremaine-c))
	 (v1 (flo:- v vp))
	 (v1 (flo:+ v1 vp))
	 (v2 (flo:- v v1))
	 (prod (flo:* u v))
	 (e1 (flo:* u1 v1))
	 (e2 (flo:* u2 v2))
	 (e12 (flo:* u1 v2))
	 (e21 (flo:* u2 v1))
	 (err (flo:- e1 prod))
	 (err (flo:+ err e12))
	 (err (flo:+ err e21))
	 (perr (flo:+ err e2)))
    (k prod perr)))


(define (/T u v k)			;k=(lambda (prod error) ...)
  (let ((quot (flo:/ u v)))
    (*T quot v
	(lambda (rat err)
	  (let* ((qerr (flo:- u rat))
		 (qerr (flo:/ (flo:- qerr err) v)))
	    (k quot qerr))))))

;;;  Quad arithmetic -- a quad is a cons of the high and low part.

(define q:high car)
(define q:low cdr)

(define (q:+ q1 q2)
  (let ((q1h (car q1))
	(q1l (cdr q1))
	(q2h (car q2))
	(q2l (cdr q2)))
    (+B q1h q2h
	(lambda (s e)
	  (+B s (flo:+ (flo:+ q1l q2l) e)
	      cons)))))

(define (q:- q1 q2)
  (let ((q1h (car q1))
	(q1l (cdr q1))
	(q2h (car q2))
	(q2l (cdr q2)))
    (-B q1h q2h
	(lambda (s e)
	  (+B s (flo:+ (flo:- q1l q2l) e)
	      cons)))))

(define (q:* q1 q2)
  (let ((q1h (car q1))
	(q1l (cdr q1))
	(q2h (car q2))
	(q2l (cdr q2)))
    (*T q1h q2h
	(lambda (p e)
	  (+B p
	      (flo:+ (flo:+ (flo:* q1l q2h) (flo:* q1h q2l)) e)
	      cons)))))

(define (q:/ q1 q2)
  (let ((q1h (car q1))
	(q1l (cdr q1))
	(q2h (car q2))
	(q2l (cdr q2)))
    (/T q1h q2h
	(lambda (q e)
	  (+B q
	      (flo:+ (flo:+ (flo:/ q1l q2h)
			    (flo:* -1.0 (flo:* q (flo:/ q2l q2h))))
		     e)
	      cons)))))

(define (->q r)
  (cons (exact->inexact r) 0.0))

(define (rat->q rat)
  (q:/ (->q (numerator rat))
       (->q (denominator rat))))


(define q:0 (->q 0.0))
(define q:1 (->q 1.0))
(define q:2 (->q 2.0))
(define q:3 (->q 3.0))
(define q:4 (->q 4.0))

(define *quad-epsilon*
  (flo:* *machine-epsilon* *machine-epsilon*))


(define (q:sqrt x)
  (let ((xh (car x)))
    (if (zero? xh)
	q:0
	(let lp ((guess (->q (sqrt xh))))
	  (let ((new (q:/ (q:+ guess (q:/ x guess)) q:2)))
	    (if (flo:= (car guess) (car new))
		new
		(lp new)))))))


(define q:sin-bugger-factor
  (expt 2. (* -1 IEEE-p)))

(define (q:sin x)
  (let ((xh (q:high x)))
    (if (flo:< (flo:abs xh) q:sin-bugger-factor)
	x
	(let ((y (q:sin (q:/ x q:3))))
	  (q:- (q:* q:3 y)
	       (q:* q:4
		    (q:* y
			 (q:* y y))))))))


(define (q:exp x)
  (let lp ((n 1) (xp x) (f q:1) (sum q:1))
    (let ((term (q:/ xp f)))
      (if (< (abs (q:high term)) *quad-epsilon*)
	  sum
	    (lp (fix:+ n 1)
		(q:* x xp)
		(q:* f (->q (fix:+ n 1)))
		(q:+ sum term))))))

(define (alternate-sine x)
  (let ((xsq (q:* x x)))
    (let lp ((n 1) (xp x) (f q:1) (sum q:0))
      (let ((term (q:/ xp f)))
	(if (< (abs (q:high term)) *quad-epsilon*)
	    sum
	    (lp (fix:+ n 2)
		(q:* (->q -1.0) (q:* xsq xp))
		(q:* f (->q (fix:* (fix:+ n 1) (fix:+ n 2))))
		(q:+ sum term)))))))


;#| probably wrong
;
;(define rat:pi 314159265358979323846264338327950288419716939937510/100000000000000000000000000000000000000000000000000)
;
;(define (q:floor x)
;  (floor (q:high x)))
;
;(define (q:truncate x)
;  (->q (truncate (q:high x))))  ;;; what if the error has an integer part?
;
;(define continued-wallp true)
;
;(define (best-fraction-from-quad x maxint)
;  (let* ((a0 (inexact->exact (floor (q:high x))))
;	 (r0 (q:- x (q:truncate x))))
;    (if (<= (q:high r0) (* (abs (q:high x)) *quad-epsilon*))
;	a0 
;	(let* ((d (q:/ q:1 r0))
;	       (a1 (inexact->exact (truncate (q:high d)))))
;	  (let lp ((s d) (last-p a0) (last-q 1) (p (+ (* a0 a1) 1)) (q a1))
;	    (if continued-wallp
;		(write-line (list s last-p last-q p q)))
;	    (if (> q maxint)
;		(/ last-p last-q)
;		(let ((r (q:- s (q:truncate s))))
;		  (if (<= (q:high r) (* (q:high s) *quad-epsilon*))
;		      (/ p q)
;		      (let* ((new-d (q:/ q:1 r))
;			     (a (inexact->exact (truncate (q:high new-d)))))
;			(lp new-d
;			    p
;			    q
;			    (+ last-p (* a p))
;			    (+ last-q (* a q))))))))))))
;
;|#

(define (cq:+ z1 z2)
  (map q:+ z1 z2))

(define (cq:- z1 z2)
  (map q:- z1 z2))

(define (cq:* z1 z2)
  (list (q:- (q:* (car z1) (car z2))
	     (q:* (cadr z1) (cadr z2)))
	(q:+ (q:* (car z1) (cadr z2))
	     (q:* (cadr z1) (car z2)))))

(define (cq:/ z1 z2)
  (let ((m (q:+ (q:* (car z2) (car z2))
		(q:* (cadr z2) (cadr z2)))))
    (list (q:/ (q:+ (q:* (car z1) (car z2))
		    (q:* (cadr z1) (cadr z2)))
	       m)
	  (q:/ (q:- (q:* (cadr z1) (car z2))
		    (q:* (car z1) (cadr z2)))
	       m))))

(define (q->cq q)
  (list q q:0))

(define (c->cq c)
  (list (->q (real-part c))
	(->q (imag-part c))))

(define cq:0 (q->cq q:0))
(define cq:1 (q->cq q:1))
(define cq:i (list q:0 q:1))

(define (q:magnitude z)
  (q:sqrt (q:+ (q:* (car z) (car z)) (q:* (cadr z) (cadr z)))))

;;; some other trig functions

(define (compute-q:pi)
  (let lp ((guess (->q pi)))
    (write-line guess)
    (let ((new-guess (q:+ guess (q:sin guess))))
      (if (< (abs (q:high (q:- new-guess guess))) *quad-epsilon*)
	  new-guess
	  (lp new-guess)))))
			    
(define q:pi '(3.141592653589793 . 1.2246467991473535e-16))
(define q:pi/2 '(1.5707963267948966 . 6.123233995736767e-17))

(define (q:cos x)
  (q:sin (q:+ x q:pi/2)))

(define (q:tan x) (q:/ (q:sin x) (q:cos x)))

(define (q:atan x)
  (let ((fp (q:+ q:1 (q:* x x)))
	(guess (->q (atan (q:high x)))))
    (let lp ((guess guess))
      (let ((new-guess (q:- guess (q:/ (q:- (q:tan guess) x) fp))))
	(if (< (abs (q:high (q:- new-guess guess))) *quad-epsilon*)
	    new-guess
	    (lp new-guess))))))

