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

;;;; Special Relativity

(define ::c '::c)			; The speed of light.

;;; Boosts: delta-t is a number; velocity, delta-x are 3-tuples.

(define ((coordinate-boost space-velocity) 4vector-prime)
  (let ((delta-t-prime (4vector->time 4vector-prime))
	(delta-x-prime (4vector->space 4vector-prime))
	(beta (/ space-velocity ::c)))
    (let ((betasq (square beta)))
      (if (zero? betasq)
	  (make-4vector delta-t-prime delta-x-prime)
	  (let ((bx (dot-product beta delta-x-prime))
		(gamma (/ 1 (sqrt (- 1 betasq)))))
	    (let ((alpha (/ (- gamma 1) betasq)))
	      (let ((delta-t
		     (* gamma (+ delta-t-prime (/ bx ::c))))
		    (delta-x
		     (+ (* gamma beta ::c delta-t-prime)
			delta-x-prime
			(* alpha beta bx))))
		(make-4vector delta-t delta-x))))))))


(define (make-4vector time space)
  (up time (ref space 0) (ref space 1) (ref space 2)))

(define (4vector->time v)
  (ref v 0))

(define (4vector->space v)
  (up (ref v 1) (ref v 2) (ref v 3)))

(define (Lorentz-length 4vector)
  (sqrt (- (square (4vector->space 4vector))
	   (square (* ::c (4vector->time 4vector))))))

;#|
;;;; Test of booster
;
;;;; Start with an arbitrary 4vector.
;
;(define e-prime
;  (make-4vector 'tp (up 'xp 'yp 'zp)))
;
;(pec (Lorentz-length e-prime))
;#| Result:
;(sqrt (+ (* -1 (expt ::c 2) (expt tp 2))
;	 (expt xp 2)
;	 (expt yp 2)
;	 (expt zp 2)))
;|#
;
;	
;;;; Boost by an arbitrary spatial velocity:
;			     
;(pec
; (Lorentz-length
;  ((coordinate-boost (up 'vx 'vy 'vz)) e-prime)))
;#| Result:
;(sqrt (+ (* -1 (expt ::c 2) (expt tp 2))
;	 (expt xp 2)
;	 (expt yp 2)
;	 (expt zp 2)))
;|#
;
;;;; So the boost of an arbitrary 4vector by an arbitrary
;;;; velocity preserves the Lorentz length of the 4vector.
;|#

;;;;         Special-relativity frames.
;;; A frame is defined by a Lorentz transformation from a
;;; background 4-space frame.  To keep us from going nuts, an
;;; SR frame has a name, which it uses to label coordinates in
;;; its frame.  The background frame is called "the-ether".

(define (make-SR-frame name from-frame boost-components origin)
  (let* ((coord-prototype (up 't0 'x0 'y0 'z0))
	 (access-chains (up (list 0) (list 1) (list 2) (list 3)))
	 (dual-chains (down (list 0) (list 1) (list 2) (list 3)))
	 (point-prototype (up 'x^0 'x^1 'x^2 'x^3))
	 (point-chains (up (list 0) (list 1) (list 2) (list 3))))
    (define (coordinates->point coords)
      (assert (SR-coordinates? coords))
      (assert (eq? me (SR-frame coords)))
      (if (eq? me the-ether)
	  coords
          (make-SR-coordinates the-ether
	    ((from-frame 'coords->event)
	     (make-SR-coordinates from-frame
	       (+ ((coordinate-boost boost-components)
		   (SR-4vector coords))
		  origin))))))
    (define (point->coordinates point)
      (assert (SR-coordinates? point))
      (assert (eq? the-ether (SR-frame point)))
      (if (eq? me the-ether)
	  point
          (make-SR-coordinates me
	    ((coordinate-boost (- boost-components))
	     (- (SR-4vector
		 ((from-frame 'event->coords) point))
		origin)))))
    (define (me m)
      (case m
	((dimension) 4)
	((->point coords->event ->event) coordinates->point)
	((->coords event->coords) point->coordinates)
	((type) SR)
	((name) name)
	((from-frame) from-frame)
	((boost-components) boost-components)
	((origin) origin)
	((typical-coords) (typical-object coord-prototype))
	((access-chains) access-chains)
	((dual-chains) dual-chains)
	((typical-point) (typical-object point-prototype))
	((point-chains) point-chains)
	(else (error "Unknown message: SR" name m))))
    me))

;;; Implementation of the coordinates uses a put/get table.

(define (make-SR-coordinates frame 4vector)
  (assert (vector? 4vector))
  (assert (fix:= (vector-length 4vector) 4))
  (eq-put! 4vector 'type 'SR-coordinates)
  (eq-put! 4vector 'frame frame)
  4vector)

(define (SR-coordinates? coords)
  (eq? (eq-get coords 'type) 'SR-coordinates))

(define (SR-frame coords)
  (eq-get coords 'frame))

(define (SR-name coords)
  ((eq-get coords 'frame) 'name))

(define (SR-4vector coords)
  coords)

(define the-ether
  (make-SR-frame 'the-ether #f #f #f))

;#|
;;;; Velocity addition formula
;
;(define A (make-SR-frame 'andy the-ether #(va 0 0) #(0 0 0 0)))
;(define B (make-SR-frame 'beth A #(vb 0 0) #(0 0 0 0)))
;
;(set! *divide-out-terms* #f)
;
;(pec (let ((foo
;	    ((the-ether 'event->coords)
;	     ((B 'coords->event)
;	      (make-SR-coordinates B
;				   (up 'tau 0 0 0))))))
;       (/ (ref foo 1) (ref foo 0))))
;#| Result:
;(/ (+ (* (expt ::c 2) va) (* (expt ::c 2) vb))
;   (+ (expt ::c 2) (* va vb)))
;|#
;|#

;#|
;;;; Twin paradox: green Schutz p28
;
;;;; We will assume Artemis is moving with unknown velocity
;;;; starting at an arbitrary point, and Diana is traveling
;;;; with respect to him: v_diana=0.96c
;
;(define A
;  (make-SR-frame 'artemis the-ether
;		 #(vx vy vz) #(t0 x0 y0 z0)))
;
;;;; In Artemis coordinates, the path of Diana has two
;;;; segments, outgoing and incoming.
;
;(define (seg1 t)
;  (make-SR-coordinates A (up t (* 96/100 ::c t) 0 0)))
;
;(define (seg2 t)
;  (let ((t2 (- t 25))) 
;    (make-SR-coordinates A
;      (+ (up t2 (* -96/100 ::c t2) 0 0) (seg1 25)))))
;
;;;; We make frames that follow Diana for each segment of her
;;;; trip. 
;
;(define D1
;  (make-SR-frame 'diana1 A (up (* 96/100 ::c) 0 0) (seg1 0)))
;
;(define D2
;  (make-SR-frame 'diana2 A (up (* -96/100 ::c) 0 0) (seg2 0)))
;
;;;; After 25 years of Artemis's time, we look at Diana's
;;;; view of things.
;
;(pec ((compose (D1 'event->coords) (A 'coords->event) seg1) 25))
;#| Result:
;(up 7 0 0 0)
;;;; tau_d = t_d = 7
;|#
;
;;;; She is at her own spatial origin, and she has aged only
;;;; 7 years.  Indeed, after 50 years, she has aged only 14
;;;; years:
;
;(pec ((compose (D2 'event->coords) (A 'coords->event) seg2) 50))
;#| Result:
;(up 14 0 0 0)
;;;; tau_d = t_d = 14
;|#
;
;;;; Seg1 gives the first part of Diana's journey in
;;;; Artemis's coordinates, parameterized by Artemis's time,
;;;; t_a.  The Lorentz length of an event on Diana's path
;;;; gives Diana's "proper time", tau_d, the time elapsed on
;;;; Diana's comoving clock (measured as a length!)
;
;(pec (Lorentz-length (seg1 't_a)))
;#| Result:
;(* +7/25i ::c t_a)
;|#
;
;;;; That the result is imaginary tells us that the path is
;;;; time-like, and at Artemis's time t_a, Diana measures her
;;;; time to be tau_d = 7/25 t_a.  This is "time dilation."
;;;; If we reparameterize the segment, using Diana's proper
;;;; time (t_a = 25/7 tau_d), but using Artemis's coordinates
;;;; we get.
;
;(define seg1_d
;  (compose seg1 (lambda (tau_d) (* 25/7 tau_d))))
;
;(pec (seg1_d 'tau_d))
;#| Result:
;(up (* 25/7 tau_d) (* 24/7 ::c tau_d) 0 0)
;|#
;
;(pec (Lorentz-length (seg1_d 'tau_d)))
;#| Result:
;(* +i ::c tau_d)
;|#
;;;; So, the Lorentz length is the proper time.
;
;(pec (seg1_d 7))
;#| Result:
;(up 25 (* 24 ::c) 0 0)
;|#
;;;; In the 7 years that elapse on Diana's clock, 25 years
;;;; have elapsed on Artemis's clock.
;
;;;; Artemis's path in Diana's coordinates
;
;;;; The initial event is at both A's and D's origin
;(pec ((D1 'event->coords)
;      ((A 'coords->event) 
;       (make-SR-coordinates A (up 0 0 0 0)))))
;#| Result:
;(up 0 0 0 0)
;|#
;
;;;; After 25 years of A's time D's coords of A is
;(pec
; ((D1 'event->coords)
;  ((A 'coords->event) 
;   (make-SR-coordinates A (up 25 0 0 0)))))
;#| Result:
;(up 625/7 (* -600/7 ::c) 0 0)
;|#
;
;;;; D sees A's clock is slow (25 years by A's clock is about
;;;; 89 years by D's clock), but A's world line is still 25
;;;; years long, even in D's coordinate description.
;
;(pec (Lorentz-length (up 625/7 (* -600/7 ::c) 0 0)))
;#| Result:
;(* +25i ::c)
;|#
;;;; The result is imaginary, because the interval is
;;;; time-like.
;
;;;; After 25 years, by A, D has abruptly changed course.
;;;; This causes her description of A to change, losing 165
;;;; years on the spot.
;(pec
; ((D2 'event->coords)
;  ((A 'coords->event) 
;   (make-SR-coordinates A (up 25 0 0 0)))))
;#| Result:
;(up -527/7 (* -600/7 ::c) 0 0)
;|#
;
;(pec (- (up -527/7 (* -600/7 ::c) 0 0)
;	(up 625/7 (* -600/7 ::c) 0 0)))
;#| Result:
;(up -1152/7 0 0 0)
;|#
;
;(/ 1152. 7)
;;Value: 164.57142857142858
;
;;;; After 50 years have passed, A has now "come back to" D.
;;;; (He is at her spatial origin.)  D's coordinate time for
;;;; A is now 14 years, so it appears to D that the trip took
;;;; 14 years.
;(pec
; ((D2 'event->coords)
;  ((A 'coords->event) 
;   (make-SR-coordinates A (up 50 0 0 0)))))
;#| Result:
;(up 14 0 0 0)
;|#
;
;(+  -527/7 625/7)
;;Value: 14
;
;;;; However, the actual proper time for A's path on this
;;;; second segment is also 25 years.  So A has aged 25
;;;; years.
;(pec
; (Lorentz-length
;  (- (up 14 0 0 0)
;     (up -527/7 (* -600/7 ::c) 0 0))))
;#| Result:
;(* +25i ::c)
;|#
;|#

;;;; A differential geometry point-of-view

;;; Assume that we have 4-vector fields with the time
;;; component in the 0 position

(define ((4-metric frame g00 g11 g22 g33 g01 g12 g23 g02 g13 g03)
	 vf1 vf2)
  (let ((es (coordinate-system->1form-basis frame)))
    (let ((dt (ref es 0))
	  (dx (ref es 1))
	  (dy (ref es 2))
	  (dz (ref es 3)))
      (+ (* g00 (square  ::c) (dt vf1) (dt vf2))
	 (* g11 (dx vf1) (dx vf2))
	 (* g22 (dy vf1) (dy vf2))
	 (* g33 (dz vf1) (dz vf2))
	 (* g01
	    (+ (* (dt vf1) (dx vf2))
	       (* (dt vf2) (dx vf1))))
	 (* g12
	    (+ (* (dx vf1) (dy vf2))
	       (* (dx vf2) (dy vf1))))
	 (* g23
	    (+ (* (dy vf1) (dz vf2))
	       (* (dy vf2) (dz vf1))))
	 (* g02
	    (+ (* (dt vf1) (dy vf2))
	       (* (dt vf2) (dy vf1))))
	 (* g13
	    (+ (* (dx vf1) (dz vf2))
	       (* (dx vf2) (dz vf1))))
	 (* g03
	    (+ (* (dt vf1) (dz vf2))
	       (* (dt vf2) (dz vf1))))))))


;;; This does not depend on the frame, so we use the-ether.

(define Lorentz-metric
  (4-metric the-ether -1 1 1 1 0 0 0 0 0 0))

(define (metric-over-map mu:N->M g-on-M)
  (lambda (v1 v2)
    (lambda (n)
      ((g-on-M
	(vector-field-over-map->vector-field v1 n)
	(vector-field-over-map->vector-field v2 n))
       (mu:N->M n)))))

;#|
;(instantiate-coordinates the-real-line 'tau_a)
;(instantiate-coordinates the-real-line 'tau_d)
;
;(define dg-seg1
;  (compose (A 'coords->event) seg1 (the-real-line '->coords)))
;
;(define dg-seg2
;  (compose (A 'coords->event) seg2 (the-real-line '->coords)))
;
;;;; After 25 years of Artemis's time, Diana gets to the
;;;; turning point event.
;
;(define turning-point-event
;  (dg-seg1 ((the-real-line '->point) 25)))
;
;(pec (SR-name turning-point-event))
;#| Result:
;the-ether
;|#
;
;;;; The tangent vector to seg1 (over the map seg1)
;
;(define T1 ((differential dg-seg1) d/dtau_a))
;
;;;; The turning point should be the integral of the tangent
;;;; over 25 years.  Because seg1 is a straight line, this
;;;; can be evaluated at any time.
;
;(pec (- (- turning-point-event (A 'origin))
;	(((* T1 25) identity) ((the-real-line '->point) 1))))
;#| Result:
;(up 0 0 0 0)
;|#
;
;;;; We can also examine the tangent vector parameterized by
;;;; Diana's proper time.
;
;(define dg-seg1_d
;  (compose (A 'coords->event) seg1_d (the-real-line '->coords)))
;
;
;(define T1_d ((differential dg-seg1_d) d/dtau_d))
;
;(pec
; (sqrt
;  (((metric-over-map seg1_d Lorentz-metric) T1_d T1_d)
;   ((the-real-line '->point) 5))))
;#| Result:
;(* +i ::c)
;|#
;
;;;; The 4-velocity of a time-like path, parameterized by its
;;;; proper time, is always c.
;
;;;; Now, from Diana's point of view:
;
;(pec ((D1 'event->coords) turning-point-event))
;#| Result:
;(up 7 0 0 0)
;|#
;;;; So, Diana has only aged 7 years.
;
;;;; Indeed, Diana's proper time is the Lorentz-length of her
;;;; world line, which is an integral of the metric measuring
;;;; the tangent vector over her path.
;
;(pec
; (* (sqrt
;     (- (((metric-over-map dg-seg1 Lorentz-metric) T1 T1)
;	 ((the-real-line '->point) 5))))
;    25))
;#| Result:
;(* 7 ::c)				; measured in length!
;|#
;
;;;; Note, this result is independent of sampling time.
;
;(pec
; (* (sqrt
;      (- (((metric-over-map dg-seg1 Lorentz-metric) T1 T1)
;	 ((the-real-line '->point) 3))))
;    25))
;#| Result:
;(* 7 ::c)
;|#
;
;;;; Now, on the second segment, Diana reverses course.  
;
;(define T2 ((differential dg-seg2) d/dtau_a))
;
;(pec
; (+ (* (sqrt
;	(- (((metric-over-map dg-seg1 Lorentz-metric) T1 T1)
;	    ((the-real-line '->point) 5))))
;       25)
;    (* (sqrt
;	(- (((metric-over-map dg-seg2 Lorentz-metric) T2 T2)
;	    ((the-real-line '->point) 5))))
;       25)))
;#| Result:
;(* 14 ::c)
;|#
;
;;;; So Diana has only aged 14 years while Artemis has aged
;;;; 50 years.  This is 1/2 of the computation.  We must also
;;;; compute Artemis's proper aging from Diana's coordinates.
;|#
