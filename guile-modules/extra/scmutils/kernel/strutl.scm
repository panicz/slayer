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

;;;; STRUTL.SCM -- Stream utilities

;(declare (usual-integrations))

(define stream:for-each
 (let ()
   (define (loop p s n)
     (cond ((empty-stream? s)
	    'done)
	   ((int:= n 1)
	    (p (head s))
	    '...)
	   (else
	    (p (head s))
	    (loop p (tail s) (int:- n 1)))))
   (lambda (p s . optionals)
     (loop p s (if optionals (car optionals) -1)))))


(define print-stream
  (lambda (s . optionals)
     (apply stream:for-each write-line s optionals)))

(define (combiner-padded-streams f pad)
  (define (lp ss)
    (stream-cons (apply f
			(map (lambda (s)
			       (if (null? s)
				   pad
				   (head s)))
			     ss))
		 (lp (map (lambda (s)
			    (if (null? s)
				s
				(tail s)))
			  ss))))
  (lambda args (lp args)))


(define (stream-of-iterates next value)
  (stream-cons value
	       (stream-of-iterates next (next value))))


(define (infinite-stream-of x)
  (stream-cons x
	       (infinite-stream-of x)))


(define (stream-evaluate s x)
  (map-stream (lambda (f) (f x)) s))

(define (stream-apply s x)
  (map-stream (lambda (f) (apply f x)) s))


(define (map-stream f s)
  (if (empty-stream? s)
      (stream)	; the-empty-stream
      (stream-cons (f (head s))
		   (map-stream f (tail s)))))

(define map-streams stream-map)


(define (merge-streams s1 s2)
  (stream-cons (stream-car s1)
	       (stream-cons (stream-car s2)
			    (merge-streams (stream-cdr s1)
					   (stream-cdr s2)))))

(define (shorten-stream n s)
  (if (or (fix:= n 0) (empty-stream? s))
      (stream)	; the-empty-stream
      (stream-cons (head s)
		   (shorten-stream (fix:- n 1)
				   (tail s)))))

(define (stream:+ s1 s2)
  (map-streams g:+ s1 s2))

(define (stream:- s1 s2)
  (map-streams g:- s1 s2))

(define (stream:* s1 s2)
  (map-streams g:* s1 s2))

(define (stream:/ s1 s2)
  (map-streams g:/ s1 s2))



(define zero-stream 
  (stream-cons :zero zero-stream))

(define one-stream
  (stream-cons :one one-stream))

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (int:+ n 1))))


(define natural-number-stream
  (stream-cons :one
	       (stream:+ one-stream
			 natural-number-stream)))

(define factorial-stream
  (let ()
    (define (fact-helper n! n+1)
      (stream-cons n!
		   (fact-helper (g:* n+1 n!) (g:+ n+1 1))))
    (fact-helper 1 1)))

(define (stream-of-powers x unity)
  (stream-of-iterates (lambda (y) (g:* x y))
		      unity))


;(define stream-for-each
;  (access stream-for-each (->environment '(runtime stream))))
;
;(define stream-append
;  (access stream-append (->environment '(runtime stream))))
;
;(define stream-filter
;  (access stream-filter (->environment '(runtime stream))))
;
;(define stream-accumulate
;  (access stream-accumulate (->environment '(runtime stream))))

;;; MIT Scheme system provides 
;;;  PRIME-NUMBERS-STREAM

; for other scheme implementations:
(define prime-numbers-stream (stream-filter prime? natural-number-stream))
; warning: slow!


;;; expands a stream with zeros interpolated between given values.

(define (stream:inflate stream n-interpolated-zeros)
  (stream-cons (head stream)
	       (stream:list-append (make-list n-interpolated-zeros
					      (g:zero-like (head stream)))
				   (stream:inflate (tail stream)
						   n-interpolated-zeros))))

(define (stream:list-append list stream)
  (if (null? list)
      stream
      (stream-cons (car list)
		   (stream:list-append (cdr list)
				       stream))))