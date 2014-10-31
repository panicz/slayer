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

;;;; Concrete Tensor Algebra

;;; A concrete tensor is a data structure that takes a list of up
;;; indices and a list of down indices and produces a number.

(define (make-tensor proc dimension n-ups n-downs)
  (list '*tensor* proc dimension n-ups n-downs))

(define (tensor? x)
  (and (pair? x) (eq? (car x) '*tensor*)))

(define (tensor:proc tensor)
  (cadr tensor))

(define (tensor:dimension tensor)
  (caddr tensor))

(define (tensor:n-ups tensor)
  (cadddr tensor))

(define (tensor:n-downs tensor)
  (car (cddddr tensor)))

(define (tensor:range dimension)
  (lambda (i)
    (and (fix:<= 0 i) (fix:< i dimension))))

(define (tensor:ref T ups downs)
  (assert (tensor? T))
  (let ((dimension (tensor:dimension T))
	(nups (tensor:n-ups T))
	(ndowns (tensor:n-downs T)))
    (assert (fix:= (length ups) nups))
    (assert (fix:= (length downs) ndowns))
    (let ((in-range? (tensor:range dimension)))
      (assert (and (for-all? ups in-range?) (for-all? downs in-range?)))
      ((tensor:proc T) ups downs))))

(define (tensor:sum T1 T2)
  (assert (and (tensor? T1) (tensor? T2)))
  (let ((dimension (tensor:dimension T1))
	(nups (tensor:n-ups T1))
	(ndowns (tensor:n-downs T1)))
    (assert (fix:= dimension (tensor:dimension T2)))
    (assert (fix:= nups (tensor:n-ups T2)))
    (assert (fix:= ndowns (tensor:n-downs T2)))
    (define (tensor-sum ups downs)
      (+ (tensor:ref T1 ups downs) (tensor:ref T2 ups downs)))
    (make-tensor tensor-sum dimension nups ndowns)))

(define (tensor:difference T1 T2)
  (assert (and (tensor? T1) (tensor? T2)))
  (let ((dimension (tensor:dimension T1))
	(nups (tensor:n-ups T1))
	(ndowns (tensor:n-downs T1)))
    (assert (fix:= dimension (tensor:dimension T2)))
    (assert (fix:= nups (tensor:n-ups T2)))
    (assert (fix:= ndowns (tensor:n-downs T2)))
    (define (tensor-difference ups downs)
      (- (tensor:ref T1 ups downs) (tensor:ref T2 ups downs)))
    (make-tensor tensor-difference dimension nups ndowns)))

(define (tensor:scale k T)
  (assert (tensor? T))
  (let ((dimension (tensor:dimension T))
	(nups (tensor:n-ups T))
	(ndowns (tensor:n-downs T)))
    (define (tensor-scale ups downs)
      (* k (tensor:ref T ups downs)))
    (make-tensor tensor-scale dimension nups ndowns)))

(define (tensor:product T1 T2)
  (assert (and (tensor? T1) (tensor? T2)))
  (let ((dimension (tensor:dimension T1)))
    (assert (fix:= dimension (tensor:dimension T2)))
    (let ((T1-nups (tensor:n-ups T1))
	  (T1-ndowns (tensor:n-downs T1))
	  (T2-nups (tensor:n-ups T2))
	  (T2-ndowns (tensor:n-downs T2)))
      (let ((nups (fix:+ T1-nups T2-nups))
	    (ndowns (fix:+ T1-ndowns T2-ndowns)))
	(define (tensor-product ups downs)
	  (* (tensor:ref T1
			 (list-head ups T1-nups)
			 (list-head downs T1-ndowns))
	     (tensor:ref T2
			 (list-tail ups T1-nups)
			 (list-tail downs T1-ndowns))))
	(make-tensor tensor-product dimension nups ndowns)))))

(define (tensor:trace T up-index down-index)
  (assert (tensor? T))
  (let ((T-nups (tensor:n-ups T))
	(T-ndowns (tensor:n-downs T)))
    (assert (and (fix:<= 0 up-index) (fix:< up-index T-nups)))
    (assert (and (fix:<= 0 down-index) (fix:< down-index T-ndowns)))
    (let ((nups (fix:+ T-nups -1))
	  (ndowns (fix:+ T-ndowns -1))
	  (dimension (tensor:dimension T)))
      (define (tensor-trace ups downs)
	(sigma (lambda (i)
		 (tensor:ref T
			     (list-with-inserted-coord ups up-index i)
			     (list-with-inserted-coord downs down-index i)))
	       0
	       (fix:- dimension 1)))
      (make-tensor tensor-trace dimension nups ndowns))))

(define (tensor:contract T1 T1-up-index T2 T2-down-index)
  (assert (and (tensor? T1) (tensor? T2)))
  (let ((T1-nups (tensor:n-ups T1))
	(T1-ndowns (tensor:n-downs T1))
	(T2-nups (tensor:n-ups T2))
	(T2-ndowns (tensor:n-downs T2)))
    (assert (and (fix:<= 0 T1-up-index) (fix:< T1-up-index T1-nups)))
    (assert (and (fix:<= 0 T2-down-index) (fix:< T2-down-index T2-ndowns)))
    (let ((nups (fix:+ (fix:+ T1-nups -1) T2-nups))
	  (ndowns (fix:+ T1-ndowns (fix:+ T2-ndowns -1)))
	  (dimension (tensor:dimension T1)))
      (assert (fix:= dimension (tensor:dimension T2)))
      (define (tensor-contraction ups downs)
	(let ((T1-ups (list-head ups (fix:- T1-nups 1)))
	      (T2-ups (list-tail ups (fix:- T1-nups 1)))
	      (T1-downs (list-head downs T1-ndowns))
	      (T2-downs (list-tail downs T1-ndowns)))
	  (sigma (lambda (i)
		   (* (tensor:ref T1
				  (list-with-inserted-coord T1-ups
							    T1-up-index
							    i)
				  T1-downs)
		      (tensor:ref T2
				  T2-ups
				  (list-with-inserted-coord T2-downs
							    T2-down-index
							    i))))
		 0
		 (fix:- dimension 1))))
      (make-tensor tensor-contraction dimension nups ndowns))))

(define (list-with-inserted-coord list index coord)
  (append (list-head list index) (cons coord (list-tail list index))))

(define (up->10-tensor v)
  (make-tensor (lambda (ups downs) (s:ref v (car ups))) (s:dimension v) 1 0))

(define (down->01-tensor v)
  (make-tensor (lambda (ups downs) (s:ref v (car downs))) (s:dimension v) 0 1))




(define (show-02-tensor T)
  (assert (and (= 0 (tensor:n-ups T)) (= 2 (tensor:n-downs T))))
  (let ((indices (iota (tensor:dimension T))))
    (for-each (lambda (i)
		(newline)
		(for-each (lambda (j)
			    (write (simplify (tensor:ref T '() (list i j))))
			    (display "	"))
			  indices))
	      indices)))

(define (show-20-tensor T)
  (assert (and (= 2 (tensor:n-ups T)) (= 0 (tensor:n-downs T))))
  (let ((indices (iota (tensor:dimension T))))
    (for-each (lambda (i)
		(newline)
		(for-each (lambda (j)
			    (write (simplify (tensor:ref T (list i j) '())))
			    (display "	"))
			  indices))
	      indices)))

(define (show-11-tensor T)
  (assert (and (= 1 (tensor:n-ups T)) (= 1 (tensor:n-downs T))))
  (let ((indices (iota (tensor:dimension T))))
    (for-each (lambda (i)
		(newline)
		(for-each (lambda (j)
			    (write (simplify (tensor:ref T (list i) (list j))))
			    (display "	"))
			  indices))
	      indices)))

;;;; Some Useful Tensors

;;; The Levi-Civita tensor in n dimensions is a down tensor with n indices

(define (Levi-Civita n)
  (let ((dims (iota n)))
    (define (epsilon ups downs)
      (assert (and (null? ups) (fix:= (length downs) n)))
      (permutation-parity downs dims))
    (make-tensor epsilon n 0 n)))


(define c 'c)

(define Lorentz-metric-structure
  (down (down -1 0 0 0) (down 0 1 0 0) (down 0 0 1 0) (down 0 0 0 1)))

(define Lorentz-inverse-metric-structure
  (invert Lorentz-metric-structure))

(define Lorentz-metric
  (let ()
    (define (g ups downs)
      (assert (and (null? ups) (fix:= (length downs) 2)))
      (ref-internal Lorentz-metric-structure downs))
    (make-tensor g 4 0 2)))
  
(define Lorentz-inverse-metric
  (let ()
    (define (g^-1 ups downs)
      (assert (and (fix:= (length ups) 2) (null? downs)))
      (ref-internal Lorentz-inverse-metric-structure ups))
    (make-tensor g^-1 4 2 0)))

;;;; Simple tests

;#|
;(show-02-tensor Lorentz-metric)
;-1	0	0	0	
;0	1	0	0	
;0	0	1	0	
;0	0	0	1	
;
;(show-02-tensor (tensor:sum Lorentz-metric Lorentz-metric))
;-2	0	0	0	
;0	2	0	0	
;0	0	2	0	
;0	0	0	2	
;
;(show-02-tensor (tensor:scale 3 Lorentz-metric))
;-3	0	0	0	
;0	3	0	0	
;0	0	3	0	
;0	0	0	3	
;
;(define A
;  (tensor:product (up->10-tensor (up 'a 'b))
;		  (down->01-tensor (down 'd 'e))))
;
;(define B
;  (tensor:product (up->10-tensor (up 'g 'h))
;		  (down->01-tensor (down 'j 'k))))
;
;(show-11-tensor A)
;(* a d)	(* a e)	
;(* b d)	(* b e)	
;
;(show-11-tensor B)
;(* g j)	(* g k)	
;(* h j)	(* h k)	
;
;(pe (tensor:ref (tensor:trace A 0 0) '() '()))
;(+ (* a d) (* b e))
;
;(show-11-tensor (tensor:trace (tensor:product A B) 0 0))
;(+ (* a d g j) (* b e g j))	(+ (* a d g k) (* b e g k))	
;(+ (* a d h j) (* b e h j))	(+ (* a d h k) (* b e h k))	
;
;(show-11-tensor (tensor:trace (tensor:product A B) 0 1))
;(+ (* a d g j) (* b d g k))	(+ (* a e g j) (* b e g k))	
;(+ (* a d h j) (* b d h k))	(+ (* a e h j) (* b e h k))	
;
;(show-11-tensor (tensor:trace (tensor:product A B) 1 0))
;(+ (* a d g j) (* a e h j))	(+ (* a d g k) (* a e h k))	
;(+ (* b d g j) (* b e h j))	(+ (* b d g k) (* b e h k))
;
;(show-11-tensor (tensor:trace (tensor:product A B) 1 1))
;(+ (* a d g j) (* a d h k))	(+ (* a e g j) (* a e h k))	
;(+ (* b d g j) (* b d h k))	(+ (* b e g j) (* b e h k))	
;
;(show-11-tensor
; (tensor:difference (tensor:trace (tensor:product A B) 0 1)
;		    (tensor:contract A 0 B 0)))
;0	0	
;0	0	
;|#

(define ((tensor:star inverse-metric-tensor) T)
  (assert (tensor? inverse-metric-tensor))
  ;;(assert (tensor:symmetric? inverse-metric-tensor))
  (assert (fix:= (tensor:n-downs inverse-metric-tensor) 0))
  (assert (tensor? T))
  (assert (fix:= (tensor:n-ups T) 0))
  ;;(assert (tensor:antisymmetric? T))
  (let ((p (tensor:n-downs T))
	(n (tensor:dimension T)))
    (assert (fix:= n (tensor:dimension inverse-metric-tensor)))
    (assert (fix:<= p n))
    (let ((coeff (/ 1 (factorial p))))
      (let epslp ((k 0) (raised-epsilon (Levi-Civita n)))
	(if (fix:= k p)
	    (begin (assert (fix:= (tensor:n-ups raised-epsilon) p))
		   (let alp ((k 0) (ans (tensor:product T raised-epsilon)))
		     (if (fix:= k p)
			 (tensor:scale coeff ans)
			 (alp (fix:+ k 1)
			      (tensor:trace ans 0 0)))))
	    (epslp (fix:+ k 1)
		   (tensor:contract inverse-metric-tensor
				    1
				    raised-epsilon
				    (fix:- p (fix:+ k 1)))))))))



;#|
;(define (Faraday Ex Ey Ez Bx By Bz)
;  (let ((s (down (down 0      (- Ex)  (- Ey)  (- Ez))
;		 (down Ex     0       Bz      (- By))
;		 (down Ey     (- Bz)  0       Bx    )
;		 (down Ez     By      (- Bx)  0     ))))
;    (define (T ups downs)
;      (assert (and (null? ups) (fix:= (length downs) 2)))
;      (ref-internal s downs))
;    (make-tensor T 4 0 2)))
;
;(show-02-tensor ((tensor:star Lorentz-inverse-metric)
;		 (Faraday 'Ex 'Ey 'Ez 'Bx 'By 'Bz)))
;0		Bx		By		Bz	
;(* -1 Bx)	0		Ez		(* -1 Ey)	
;(* -1 By)	(* -1 Ez)	0		Ex	
;(* -1 Bz)	Ey		(* -1 Ex)	0	
;
;(show-02-tensor ((tensor:star Lorentz-inverse-metric)
;		 ((tensor:star Lorentz-inverse-metric)
;		  (Faraday 'Ex 'Ey 'Ez 'Bx 'By 'Bz))))
;0		Ex		Ey		Ez	
;(* -1 Ex)	0		(* -1 Bz)	By	
;(* -1 Ey)	Bz		0		(* -1 Bx)	
;(* -1 Ez)	(* -1 By)	Bx		0	
;|#

;#|
;(define dt^dx
;  (let ((s (down (down 0  1  0  0)
;		 (down -1 0  0  0)
;		 (down 0  0  0  0)
;		 (down 0  0  0  0))))
;    (define (T ups downs)
;      (assert (and (null? ups) (fix:= (length downs) 2)))
;      (ref-internal s downs))
;    (make-tensor T 4 0 2)))
;
;(show-02-tensor ((tensor:star Lorentz-inverse-metric) dt^dx))
;0	0	0	0	
;0	0	0	0	
;0	0	0	-1	
;0	0	1	0	
;
;;;; Therefore *(dt^dx) = -dy^dz                (negative)
;
;
;(define dy^dz
;  (let ((s (down (down 0 0 0  0)
;		 (down 0 0 0  0)
;		 (down 0 0 0  1)
;		 (down 0 0 -1 0))))
;    (define (T ups downs)
;      (assert (and (null? ups) (fix:= (length downs) 2)))
;      (ref-internal s downs))
;    (make-tensor T 4 0 2)))
;
;
;(show-02-tensor ((tensor:star Lorentz-inverse-metric) dy^dz))
;0		1	0	0	
;-1		0	0	0	
;0		0	0	0	
;0		0	0	0	
;
;;;; Therefore *(dy^dz) = dt^dx                (positive)
;|#

;#|
;(define Tdt
;  (let ((s (down 1 0 0 0)))
;    (define (T ups downs)
;      (assert (and (null? ups) (fix:= (length downs) 1)))
;      (ref-internal s downs))
;    (make-tensor T 4 0 1)))
;
;(define *dt
;  ((tensor:star Lorentz-inverse-metric) Tdt))
;
;(for-each (lambda (i)
;	    (for-each (lambda (j)
;			(for-each (lambda (k)
;				    (let ((x (tensor:ref *dt
;							 '()
;							 (list i j k))))
;				      (if (not (= x 0))
;					  (pp `((,i ,j ,k) ,x)))))
;				  (iota 4)))
;		      (iota 4)))
;	  (iota 4))
;((1 2 3) -1)
;((1 3 2) 1)
;((2 1 3) 1)
;((2 3 1) -1)
;((3 1 2) -1)
;((3 2 1) 1)
;
;;;; Thus *dt = -(dx^dy^dz)
;
;
;(define Tdx^dy^dz+dy^dz^dx+dz^dx^dy
;  (let ()
;    (define (T ups downs)
;      (assert (and (null? ups) (fix:= (length downs) 3)))
;      (cond ((member downs '((1 2 3) (2 3 1) (3 1 2))) -1)
;	    ((member downs '((1 3 2) (2 1 3) (3 2 1))) +1)
;	    (else 0)))
;    (make-tensor T 4 0 3)))
;
;(define *Tdx^dy^dz+dy^dz^dx+dz^dx^dy
;  ((tensor:star Lorentz-inverse-metric) Tdx^dy^dz+dy^dz^dx+dz^dx^dy))
;
;(for-each (lambda (i)
;	    (pp (tensor:ref *Tdx^dy^dz+dy^dz^dx+dz^dx^dy '() (list i))))
;	  (iota 4))
;1
;0
;0
;0
;
;;;; So *(dx^dy^dz) = dt
;|#

;#|
;;;; Now a 0-form ==> 4-form
;
;(define T
;  (let ((s))
;    (define (T ups downs)
;      (assert (and (null? ups) (null? downs)))
;      1)
;    (make-tensor T 4 0 0)))
;
;(define *T
;  ((tensor:star Lorentz-inverse-metric) T))
;
;(for-each (lambda (i)
;	    (for-each (lambda (j)
;			(for-each (lambda (k)
;				    (for-each (lambda (l)
;						(let ((x (tensor:ref *T
;								     '()
;								     (list i j k l))))
;						  (if (not (= x 0))
;						      (pp `((,i ,j ,k ,l) ,x)))))
;					      (iota 4)))  
;				  (iota 4)))
;		      (iota 4)))
;	  (iota 4))
;((0 1 2 3) 1)
;((0 1 3 2) -1)
;((0 2 1 3) -1)
;((0 2 3 1) 1)
;((0 3 1 2) 1)
;((0 3 2 1) -1)
;((1 0 2 3) -1)
;((1 0 3 2) 1)
;((1 2 0 3) 1)
;((1 2 3 0) -1)
;((1 3 0 2) -1)
;((1 3 2 0) 1)
;((2 0 1 3) 1)
;((2 0 3 1) -1)
;((2 1 0 3) -1)
;((2 1 3 0) 1)
;((2 3 0 1) 1)
;((2 3 1 0) -1)
;((3 0 1 2) -1)
;((3 0 2 1) 1)
;((3 1 0 2) 1)
;((3 1 2 0) -1)
;((3 2 0 1) -1)
;((3 2 1 0) 1)
;
;;;; Apparently positive: *1 = dt^dx^dy^dz
;|#

;#|
;(define *T
;  (let ((s '(((0 1 2 3) 1)
;	     ((0 1 3 2) -1)
;	     ((0 2 1 3) -1)
;	     ((0 2 3 1) 1)
;	     ((0 3 1 2) 1)
;	     ((0 3 2 1) -1)
;	     ((1 0 2 3) -1)
;	     ((1 0 3 2) 1)
;	     ((1 2 0 3) 1)
;	     ((1 2 3 0) -1)
;	     ((1 3 0 2) -1)
;	     ((1 3 2 0) 1)
;	     ((2 0 1 3) 1)
;	     ((2 0 3 1) -1)
;	     ((2 1 0 3) -1)
;	     ((2 1 3 0) 1)
;	     ((2 3 0 1) 1)
;	     ((2 3 1 0) -1)
;	     ((3 0 1 2) -1)
;	     ((3 0 2 1) 1)
;	     ((3 1 0 2) 1)
;	     ((3 1 2 0) -1)
;	     ((3 2 0 1) -1)
;	     ((3 2 1 0) 1))))
;    (define (T ups downs)
;      (assert (and (null? ups) (= (length downs) 4)))
;      (let ((v (assoc downs s)))
;	(if v (cadr v) 0)))
;    (make-tensor T 4 0 4)))
;
;(define **T
;  ((tensor:star Lorentz-inverse-metric) *T))
;
;(pp (tensor:ref **T '() '()))
;-1
;|#

;#|
;(define screwy-metric-structure
;  (down (down -1 0 0 0) (down 0 1 0 0) (down 0 0 -1 0) (down 0 0 0 1)))
;
;(define screwy-inverse-metric-structure (invert screwy-metric-structure))
;
;(define screwy-metric
;  (let ()
;    (define (g ups downs)
;      (assert (and (null? ups) (fix:= (length downs) 2)))
;      (ref-internal screwy-metric-structure downs))
;    (make-tensor g 4 0 2)))
;  
;(define screwy-inverse-metric
;  (let ()
;    (define (g^-1 ups downs)
;      (assert (and (fix:= (length ups) 2) (null? downs)))
;      (ref-internal screwy-inverse-metric-structure ups))
;    (make-tensor g^-1 4 2 0)))
;
;(show-02-tensor ((tensor:star screwy-inverse-metric) dy^dz))
;0	-1	0	0	
;1	0	0	0	
;0	0	0	0	
;0	0	0	0	
;
;(show-02-tensor ((tensor:star screwy-inverse-metric) dt^dx))
;0	0	0	0	
;0	0	0	0	
;0	0	0	-1	
;0	0	1	0	
;
;(define dy^dt
;  (let ((s (down (down 0  0  -1  0)
;		 (down 0  0  0   0)
;		 (down 1  0  0   0)
;		 (down 0  0  0   0))))
;    (define (T ups downs)
;      (assert (and (null? ups) (fix:= (length downs) 2)))
;      (ref-internal s downs))
;    (make-tensor T 4 0 2)))
;
;(show-02-tensor ((tensor:star screwy-inverse-metric) dy^dt))
;0	0	0	0	
;0	0	0	1	
;0	0	0	0	
;0	-1	0	0	
;
;(define dz^dx
;  (let ((s (down (down 0  0  0   0)
;		 (down 0  0  0   -1)
;		 (down 0  0  0   0)
;		 (down 0  1  0   0))))
;    (define (T ups downs)
;      (assert (and (null? ups) (fix:= (length downs) 2)))
;      (ref-internal s downs))
;    (make-tensor T 4 0 2)))
;
;(show-02-tensor ((tensor:star screwy-inverse-metric) dy^dt))
;0	0	0	0	
;0	0	0	1	
;0	0	0	0	
;0	-1	0	0	
;|#
