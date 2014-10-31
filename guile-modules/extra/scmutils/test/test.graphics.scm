#!/usr/bin/guile \
-l ../load.scm -s 
!#

; replace + * etc with generic operators
(eval-when (load compile eval)
	   (set-current-module generic-environment))

(define win2 (frame 0. pi/2 0. 1.2))

(define ((L-harmonic m k) local)
  (let ((q (coordinate local)) 
        (v (velocity local)))
    (- (* 1/2 m (square v))
       (* 1/2 k (square q)))))

(define ((parametric-path-action Lagrangian t0 q0 t1 q1) 
         intermediate-qs)
    (let ((path (make-path t0 q0 t1 q1 intermediate-qs)))
      ;; display path
      (graphics-clear win2)
      (gnuplot-function win2 path t0 t1 (/ (- t1 t0) 100))
      ;; compute action
      (Lagrangian-action Lagrangian path t0 t1)))

(find-path (L-harmonic 1. 1.) 0. 1. pi/2 0. 3)
