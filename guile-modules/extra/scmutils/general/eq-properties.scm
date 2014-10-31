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

;;;; Traditional LISP property lists
;;; extended to work on any kind of eq? data structure.

;(declare (usual-integrations))

;;; Property lists are a way of creating data that looks like a record
;;; structure without commiting to the fields that will be used until
;;; run time.  The use of such flexible structures is frowned upon by
;;; most computer scientists, because it is hard to statically
;;; determine the bounds of the behavior of a program written using
;;; this stuff.  But it makes it easy to write programs that confuse
;;; such computer scientists.  I personally find it difficult to write
;;; without such crutches.  -- GJS


;(load-option 'hash-table)
(define eq-properties (make-hash-table 10))

(define (eq-put! node property value)
  (let ((plist (hashq-ref eq-properties node #f)))
    (if plist
	(let ((vcell (assq property (cdr plist))))
	  (if vcell
	      (set-cdr! vcell value)
	      (set-cdr! plist
			(cons (cons property value)
			      (cdr plist)))))
	(hashq-set! eq-properties node
			 (list node (cons property value)))))
  'done)

(define (eq-adjoin! node property new)
  (eq-put! node property
	   (eq-set/adjoin new
			  (or (eq-get node property) '())))
  'done)

(define (eq-rem! node property)
  (let ((plist (hashq-ref eq-properties node #f)))
    (if plist
	(let ((vcell (assq property (cdr plist))))
	  (if vcell
	      (hashq-set! eq-properties node (delq! vcell plist))))))
  'done)


(define (eq-get node property)
  (let ((plist (hashq-ref eq-properties node #f)))
    (if plist
	(let ((vcell (assq property (cdr plist))))
	  (if vcell
	      (cdr vcell)
	      #f))
	#f)))

(define (eq-plist node)
  (hashq-ref eq-properties node #f))


(define (eq-path path)
  (define (lp node)
    (if node
	(if (pair? path)
	    (eq-get ((eq-path (cdr path)) node)
		    (car path))
	    node)
	#f))
  lp)
