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

;;;; State advancer for parametric system derivatives with
;;;    arbitrarily-structured states.



(define* ((evolve parametric-sysder #:rest parameters)
	  initial-state monitor dt tmax #:optional eps)
  (set! eps (if (default-object? eps) *default-advancer-tolerance* eps))
  (let ((advance (apply state-advancer parametric-sysder parameters)))
    (let lp ((state initial-state))
      (monitor state)
      (if (< (+ (g:ref state 0) dt) tmax)
	  (let ((nstate (advance state dt eps)))
	    (lp nstate))
	  (let ((end-state
		 (advance state (- tmax (g:ref state 0)) eps)))
	    (monitor end-state)
	    end-state)))))

(define* (state-advancer parametric-sysder #:rest params)
  (define* (advance-state state dt #:optional eps)
    (define (flatten state)
      (list->vector (ultra-flatten state)))
    (define (unflatten fstate)
      (ultra-unflatten state (vector->list fstate)))
    (set! eps
	  (if (default-object? eps)
	      *default-advancer-tolerance*
	      eps))
    (let* ((fstate (flatten state))
	   (parametric-flat-sysder
	    (make-parametric-flat-sysder parametric-sysder
					 params
					 fstate
					 flatten
					 unflatten)))
      (unflatten
       (ode-advancer (parametric-flat-sysder params)
		     fstate
		     dt
		     eps))))
  advance-state)

(define *default-advancer-tolerance* 1e-12)

(define (make-parametric-flat-sysder parametric-sysder params fstate flatten unflatten)

  (define ((parametric-flat-sysder params) fstate)
    (flatten ((apply parametric-sysder params) (unflatten fstate))))

  (cond ((gear? *ode-integration-method*)
	 ;; produce f&df
	 (if *compiling-sysder?
	     (let* ((cpfs
		     (compile-parametric-memoized parametric-sysder	    
						  parametric-flat-sysder 
						  params
						  fstate))
		    (parametric-flat-jacobian
		     (lambda (params)
		       (lambda (fstate)
			 (s->m (compatible-shape fstate)
			       ((g:derivative (parametric-flat-sysder params)) fstate)
			       fstate))))
		    (cpfj
		     (compile-parametric-memoized cpfs
						  parametric-flat-jacobian 
						  params
						  fstate)))
	       
	       (lambda (params)
		 (let ((f (cpfs params)) (df (cpfj params)))
		   (lambda (fstate cont)
		     (cont (f fstate) (df fstate))))))
	     (let ((cs (compatible-shape fstate)))
	       (lambda (params)
		 (let ((f (parametric-flat-sysder params))
		       (df (g:derivative (parametric-flat-sysder params))))
		   (lambda (fstate cont)
		     (cont (f fstate) (s->m cs (df fstate) fstate))))))))
	(*compiling-sysder?
	 ;; produce compiled flat sysder
	 (compile-parametric-memoized
	  parametric-sysder		;for memoizer
	  parametric-flat-sysder	;to be compiled
	  params
	  fstate))
	(else
	 parametric-flat-sysder)))

(define *compiling-sysder? #t)
(define *max-compiled-sysder-table-size* 3)
(define *compiled-sysder-table-size* 0)
(define *compiled-sysder-table* '())

(define compile-parametric-memoized
  (let ((sm1 (fix:- *max-compiled-sysder-table-size* 1)))
    (define (run parametric-sysder parametric-flat-sysder params fstate)
      (let* ((n-params (length params))
	     (n-state (vector-length fstate))
	     (x (list n-params n-state parametric-sysder))
	     (seen (assoc x *compiled-sysder-table*)))
	(if seen
	    (cadr seen)
	    (let ((ans (compile-parametric n-params n-state parametric-flat-sysder)))
	      (cond ((fix:= *compiled-sysder-table-size*
			    *max-compiled-sysder-table-size*)
		     (set! *compiled-sysder-table*
			   (cons (list x ans)
				 (list-head *compiled-sysder-table* sm1))))
		    (else
		     (set! *compiled-sysder-table*
			   (cons (list x ans) *compiled-sysder-table*))
		     (set! *compiled-sysder-table-size*
			   (fix:+ *compiled-sysder-table-size* 1))))
	      ans))))
    run))

;;; For compiling a parametric function of a vector.  
;;; The procedure is of the form (lambda (p1 ... pn) (lambda (v) ...)) 
;;;   The number of parameters is n-params.
;;;   The length of the vector v is n-state-vars.
;;; Makes a compiled procedure which takes a list of parameters
;;;    (lambda (params) (lambda (v) ...))
;;; instead of the parameters spread out as in the source.

(define *compiler-simplifier* #f)

;;; If no "simplification" is desired 
;;;(set! *compiler-simplifier* expression)

;;; Default is to use usual simplifier
(set! *compiler-simplifier* simplify)

(define* (compile-parametric n-params n-state-vars procedure #:optional simplifier compiler)
  (let ((parameter-arity (procedure-arity procedure)))
;#|
;    (if (not (and (number? (cdr parameter-arity))
;		  (fix:= (car parameter-arity) (cdr parameter-arity))))
;	(error "Indeterminate parameters in parametric procedure"))
;    (if (not (fix:= n-params (car parameter-arity)))
;	(error "Wrong number of parameters to parametric procedure"))
;|#
    (if (default-object? simplifier) (set! simplifier *compiler-simplifier*))
    (if (default-object? compiler) (set! compiler lambda->numerical-procedure))
    (let ((param-names
	   (generate-list n-params
			  (lambda (i)
			    (string->uninterned-symbol
			     (string-append "c" (number->string i))))))
	  (params (string->uninterned-symbol "params"))
	  (state-var-names
	   (generate-list n-state-vars
			  (lambda (i)
			    (string->uninterned-symbol
			     (string-append "x" (number->string i))))))
	  (state (string->uninterned-symbol "state")))
      (let* ((state-procedure
	      (with-literal-apply-enabled
		  (lambda () (procedure param-names))))
	     (sderiv-exp
	      (flush-column
	       (simplifier
		(with-literal-apply-enabled
		    (lambda ()
		      (state-procedure
		       (list->vector state-var-names))))))))
	(let ((lexp
	       `(lambda (,params)
		  (let (,@(map (lambda (pn j)
				 `(,pn (list-ref ,params ,j)))
			       param-names
			       (generate-list n-params (lambda (j) j))))
		    (lambda (,state)
		      (let (,@(map (lambda (xi i)
				     `(,xi (vector-ref ,state ,i)))
				   state-var-names
				   (generate-list n-state-vars (lambda (i) i))))
			,sderiv-exp))))))
	  (compiler lexp))))))

(define (flush-column exp)
  (if (eq? (car exp) up-constructor-name)
      (cons 'vector (cdr exp))
      exp))

;#|
;(define ((L-coupled-harmonic m k) state)
;  (let ((q (coordinate state))
;	(qdot (velocity state)))
;    (- (* 1/2 qdot m qdot)
;       (* 1/2 q k q))))
;
;(pe ((phase-space-derivative
;      (Lagrangian->Hamiltonian
;       (L-coupled-harmonic (down (down 'm_1 0)
;				 (down 0 'm_2))
;			   (down (down 'k_1 'c)
;				 (down 'c 'k_2)))))
;     (->H-state 't
;		(coordinate-tuple 'x_1 'x_2)
;		(momentum-tuple 'p_1 'p_2))))
;(up 1
;    (up (/ p_1 m_1)
;	(/ p_2 m_2))
;    (down (+ (* -1 c x_2) (* -1 k_1 x_1))
;	  (+ (* -1 c x_1) (* -1 k_2 x_2))))
;
;
;(define (sysder-HO m1 m2 k1 k2 c)
;  (phase-space-derivative
;   (Lagrangian->Hamiltonian
;    (L-coupled-harmonic (down (down m1 0)
;			      (down 0  m2))
;			(down (down k1 c)
;			      (down c  k2))))))
;
;
;((state-advancer sysder-HO 1. 1. 1. 1. 0.)
; (up 0. (up 1. 2.) (down 3. 4.))
; 10
; 1.e-12)
;;Value: #(10.000000000000004 #(-2.4711348617445603 -3.854227501710379) (*down* #(-1.9731934763399812 -2.2682438945270498)))
;
;(length *compiled-sysder-table*)
;;Value: 1
;
;(pp (cadr (car *compiled-sysder-table*)))
;(lambda (params)
;  (let ((c4 (list-ref params 4)))
;    (lambda (state)
;      (let ((V-212 (&* (vector-ref state 2) -1)) (V-211 (&* (vector-ref state 1) -1)))
;        (vector 1
;                (&/ (vector-ref state 3) (list-ref params 0))
;                (&/ (vector-ref state 4) (list-ref params 1))
;                (&+ (&* V-211 (list-ref params 2)) (&* V-212 c4))
;                (&+ (&* V-212 (list-ref params 3)) (&* V-211 c4)))))))
;
;
;((evolve sysder-HO 1. 1. 1. 1. 0.)
; (up 0 (up 1 2) (down 3 4))
; pe
; 1
; 10
; )
;(up 0 (up 1 2) (down 3 4))
;(up 1. (up 3.064715260291832 4.4464885509678655) (down .7794359327965212 .47826725385676705))
;(up 2. (up 2.3117454439299054 2.8048960342084457) (down -2.157737936467112 -3.483182199839933))
;(up 3. (up -.5666324724208439 -1.4155049609614183) (down -3.111097497861209 -4.24221000252152))
;(up 3.9999999999999996 (up -2.9240511067874 -4.3344972229589365) (down -1.2041283672829095 -1.100969492838598))
;(up 5. (up -2.593110638526191 -3.2683727277261077) (down 1.8099108310528178 3.052497291179177))
;(up 6. (up .1219237920535883 .8026785805050183) (down 3.159926358150026 4.39951214299932))
;(up 7. (up 2.724862050499671 4.13575090356176) (down 1.6047201643111262 1.701635819935653))
;(up 8. (up 2.8225747060615296 3.666432918876308) (down -1.425858348049221 -2.5607166284812077))
;(up 9. (up .3252251938405927 -.17378658280231318) (down -3.1455092708957855 -4.468758018022222))
;(up 10. (up -2.47113486174456 -3.8542275017103753) (down -1.9731934763399899 -2.2682438945270835))
;;Value: #(10. #(-2.47113486174456 -3.8542275017103753) (*down* #(-1.9731934763399899 -2.2682438945270835)))
;|#