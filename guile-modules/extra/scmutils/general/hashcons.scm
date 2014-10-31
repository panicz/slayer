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

;;;; cons-unique (aka hashcons)
;;;  Apparently invented by Ershov (CACM 1, 8, August 1958, pp. 3--6)
;;;  Re-introduced by E.Goto in 1974.

(declare (usual-integrations))

;;; Given two arguments cons-unique returns a pair.  If exactly the
;;; same two arguments were previously combined with cons-unique it
;;; returns the same pair it returned the first time.

(define cons-unique
  ;; I don't want to cons if unnecessary.
  (let ((the-pair (cons #f #f)))  
    (define (hashcons x y)
      (set-car! the-pair x)
      (set-cdr! the-pair y)
      (let ((canonical-pair
	     (hash-table/get the-cons-table the-pair #f)))
	(if canonical-pair
	    (begin
	      (set-car! the-pair #f)
	      (set-cdr! the-pair #f)
	      canonical-pair)
	    (let ((new the-pair))
	      (hash-table/put! the-cons-table new new)
	      (set! the-pair (cons #f #f))
	      new))))
    hashcons))


;;; Given a list structure, to get a canonical copy equal to the given
;;; list structure.  Must canonicalize and share all substructure.
	    
(define (canonical-copy x)
  (if (pair? x)
      (let ((canonical-pair		; already canonical?
	     (hash-table/get the-cons-table x #f)))
	(or canonical-pair
	    (cons-unique (canonical-copy (car x))
			 (canonical-copy (cdr x)))))
      x))


(define (map-unique p lst)
  (if (pair? lst)
      (cons-unique (p (car lst))
		   (map-unique p (cdr lst)))
      lst))

;;; Support for the hashcons system.

(define (pair-eq? u v)
  (and (eq? (car u) (car v))
       (eq? (cdr u) (cdr v))))

(define the-cons-table
  ((weak-hash-table/constructor equal-hash-mod
				pair-eq?
				#t)))

;#|
;;;; For example...
;
;(define foo
;  '(define (canonical-copy x)
;     (if (pair? x)
;	 (let ((canonical-pair
;		(hash-table/get the-cons-table x #f)))
;	   (or canonical-pair
;	       (let ((new
;		      (cons (canonical-copy (car x))
;			    (canonical-copy (cdr x)))))
;		 (hash-table/put! the-cons-table new new)
;		 new)))
;	 x)))
;
;(define bar
;  '(define cons-unique
;     ;; I don't want to cons if unnecessary.
;     (let ((the-pair (cons #f #f)))  
;       (define (hashcons x y)
;	 (set-car! the-pair x)
;	 (set-cdr! the-pair y)
;	 (let ((canonical-pair
;		(hash-table/get the-cons-table the-pair #f)))
;	   (or canonical-pair
;	       (let ((new the-pair))
;		 (hash-table/put! the-cons-table new new)
;		 (set! the-pair (cons #f #f))
;		 new))))
;       hashcons)))
;
;(define cfoo
;  (canonical-copy foo))
;;Value: cfoo
;
;(eq? cfoo (canonical-copy foo))
;;Value: #t
;
;(define cbar (canonical-copy bar))
;;Value: cbar
;
;(define baz (caddr (caddr (caddr (caddr (caddr cfoo))))))
;;Value: baz
;
;baz
;;Value: (hash-table/put! the-cons-table new new)
;
;
;(define mum (caddr (caddr (caddr (car (cddddr (caddr (caddr cbar))))))))
;;Value: mum
;
;;Value: mum
;;Value: (hash-table/put! the-cons-table new new)
;
;(eq? baz mum)
;;Value: #t
;|#
