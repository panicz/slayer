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

;;;;        Quaternions
;;; 13 August 1998 -- rfrankel, gjs

;(declare (usual-integrations))


(define (q:type v) quaternion-type-tag)

(define (q:type-predicate v) quaternion-quantity?)


(define (make-quaternion v)
  (list quaternion-type-tag v))

(define (quaternion r i j k)
  (make-quaternion (vector r i j k)))

(define (quaternion->vector q)
  (cadr q))

(define (quaternion-ref q i)
  (vector-ref (quaternion->vector q) i))



(define (quaternion->3vector q)
  (vector-tail (quaternion->vector q) 1))

(define (quaternion->real-part q)
  (quaternion-ref q 0))

(define (quaternion+quaternion q1 q2)
  (make-quaternion
   (make-initialized-vector 4
     (lambda (i)
       (g:+ (quaternion-ref q1 i)
	    (quaternion-ref q2 i))))))

(define (quaternion-quaternion q1 q2)
  (make-quaternion
   (make-initialized-vector 4
     (lambda (i)
       (g:- (quaternion-ref q1 i)
	    (quaternion-ref q2 i))))))

(define (quaternion*quaternion q1 q2)
  (let ((r1 (quaternion-ref q1 0))
	(i1 (quaternion-ref q1 1))
	(j1 (quaternion-ref q1 2))
	(k1 (quaternion-ref q1 3))
	(r2 (quaternion-ref q2 0))
	(i2 (quaternion-ref q2 1))
	(j2 (quaternion-ref q2 2))
	(k2 (quaternion-ref q2 3)))
    (make-quaternion
     (vector (g:- (g:* r1 r2) (g:+ (g:* i1 i2) (g:* j1 j2) (g:* k1 k2)))
	     (g:+ (g:* r1 i2) (g:* i1 r2) (g:* j1 k2) (g:* -1 k1 j2))
	     (g:+ (g:* r1 j2) (g:* -1 i1 k2) (g:* j1 r2) (g:* k1 i2))
	     (g:+ (g:* r1 k2) (g:* i1 j2) (g:* -1 j1 i2) (g:* k1 r2))))))


(define (q:conjugate q)
  (make-quaternion
   (make-initialized-vector 4
     (lambda (i)
       (if (fix:= i 0)
	   (quaternion-ref q i)
	   (g:- (quaternion-ref q i)))))))

(define (q:negate q)
  (make-quaternion
   (make-initialized-vector 4
     (lambda (i)
       (g:- (quaternion-ref q i))))))

(define (q:zero-like q)
  (make-quaternion
   (make-vector 4 0)))

(define (q:zero? q)
  (and (g:zero? (quaternion-ref q 0))
       (g:zero? (quaternion-ref q 1))
       (g:zero? (quaternion-ref q 2))
       (g:zero? (quaternion-ref q 3))))

(define (q:magnitude q)
  (g:sqrt (g:+ (g:square (quaternion-ref q 0))
	       (g:square (quaternion-ref q 1))
	       (g:square (quaternion-ref q 2))
	       (g:square (quaternion-ref q 3)))))

(define (q:= q1 q2)
  (and (g:= (quaternion-ref q1 0) (quaternion-ref q2 0))
       (g:= (quaternion-ref q1 1) (quaternion-ref q2 1))
       (g:= (quaternion-ref q1 2) (quaternion-ref q2 2))
       (g:= (quaternion-ref q1 3) (quaternion-ref q2 3))))

(define (scalar*quaternion s q)
  (make-quaternion
   (make-initialized-vector 4
    (lambda (i)
      (g:* s (quaternion-ref q i))))))

(define (quaternion*scalar q s)
  (make-quaternion
   (make-initialized-vector 4
    (lambda (i)
      (g:* (quaternion-ref q i) s)))))

(define (quaternion/scalar q s)
  (make-quaternion
   (make-initialized-vector 4
    (lambda (i)
      (g:/ (quaternion-ref q i) s)))))

(define (q:inexact? q)
  (or (g:inexact? (quaternion-ref q 0))
      (g:inexact? (quaternion-ref q 1))
      (g:inexact? (quaternion-ref q 2))
      (g:inexact? (quaternion-ref q 3))))



(define (q:apply q args)
  (let ((vec (quaternion->vector q)))
    (make-quaternion
     (v:generate (vector-length vec)
		 (lambda (i)
		   (g:apply (vector-ref vec i) args))))))

(define (q:arity q)
  (let ((v (quaternion->vector q)))
    (let ((n 4))
      (let lp ((i 1) (a (g:arity (vector-ref v 0))))
	(if (fix:= i n)
	    a
	    (let ((b (joint-arity a (g:arity (vector-ref v i)))))
	      (if b
		  (lp (+ i 1) b)
		  #f)))))))

(define (q:partial-derivative q varspecs)
  (let ((v (quaternion->vector q)))
    (make-quaternion
     ((v:elementwise
       (lambda (f)
	 (apply g:partial-derivative f varspecs)))
      v))))



(assign-operation 'type             q:type            quaternion?)
(assign-operation 'type-predicate   q:type-predicate  quaternion?)

(assign-operation 'arity            q:arity           quaternion?)

(assign-operation 'inexact?         q:inexact?        quaternion?)

(assign-operation 'zero-like        q:zero-like       quaternion?)

(assign-operation 'zero?            q:zero?           quaternion?)

(assign-operation 'negate           q:negate          quaternion?)

(assign-operation 'magnitude        q:magnitude       quaternion?)


(assign-operation 'conjugate        q:conjugate       quaternion?)


(assign-operation '=     q:=                       quaternion? quaternion?)

(assign-operation '+     quaternion+quaternion     quaternion? quaternion?)
(assign-operation '-     quaternion-quaternion     quaternion? quaternion?)

(assign-operation '*     quaternion*quaternion     quaternion? quaternion?)

(assign-operation '*     scalar*quaternion         scalar?     quaternion?)
(assign-operation '*     quaternion*scalar         quaternion? scalar?)

(assign-operation '/     quaternion/scalar         quaternion? scalar?)

(assign-operation 'apply            q:apply        quaternion? any?)

(assign-operation 'partial-derivative q:partial-derivative quaternion? any?)

