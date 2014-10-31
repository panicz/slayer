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



;;; Default settings

(define *ode-integration-method* 'BULIRSCH-STOER)

;;; (set! *ode-integration-method* 'QCRK4)
;;; (set! *ode-integration-method* 'BULIRSCH-STOER)
;;; (set! *ode-integration-method* 'QCCTRAP2)
;;; (set! *ode-integration-method* 'GEAR)

(define *first-step-scale* 1.0)

(define *corrector-convergence-margin* 1.0e-1)

(define *progress-monitor* #f)

(define *last-state* #f)

(define (ode-advancer sysder state dt local-error-tolerance)
  (case *ode-integration-method*
    ((BULIRSCH-STOER bulirsch-stoer Bulirsch-Stoer)
     (bs-advancer sysder state dt local-error-tolerance))
    ((QCRK4 qcrk4)
     (qcrk4-advancer sysder state dt local-error-tolerance))
    ((QCCTRAP2 qcctrap2)
     (qc-ctrap-advancer sysder state dt local-error-tolerance))
    ((Gear Explicit-Gear gear explicit-gear GEAR)
     ;; actually sysder here is f&df
     (gear-advancer sysder state dt local-error-tolerance))
    (else
     (write-line `(methods: bulirsch-stoer qcrk4 qcctrap2 explicit-gear))
     (error "Unknown ode integrator" *ode-integration-method*))))

(define (set-ode-integration-method! method)
  (case method
    ((BULIRSCH-STOER bulirsch-stoer Bulirsch-Stoer)
     (set! *ode-integration-method* 'bulirsch-stoer))
    ((QCRK4 qcrk4)
     (set! *ode-integration-method* 'qcrk4))
    ((QCCTRAP2 qcctrap2)
     (set! *ode-integration-method* 'qcctrap2))
    ((Gear Explicit-Gear gear explicit-gear GEAR)
     ;; actually sysder here is f&df
     (set! *ode-integration-method* 'explicit-gear))
    (else
     (write-line `(available methods: bulirsch-stoer qcrk4 qcctrap2 explicit-gear))
     `(currently: ,*ode-integration-method*))))

(define (advance-monitor ns step-achieved step-suggested cont)
  (if *progress-monitor* (pp `(,ns ,step-achieved ,step-suggested)))
  (set! *last-state* ns)
  (cont))

(define (final-step-monitor ns step-achieved step-suggested)
  (if *progress-monitor* (pp `(,ns ,step-achieved ,step-suggested)))
  (set! *last-state* ns)
  ns)

(define (bs-advancer sysder state dt local-error-tolerance)
  ((advance-generator
    (bulirsch-stoer-lisptran		;integrator
     (system-derivative->lisptran-derivative sysder)
     (vector-length state)
     local-error-tolerance))
   state
   dt
   (* *first-step-scale* dt)
   dt
   advance-monitor
   final-step-monitor))

(define (qcrk4-advancer sysder state dt local-error-tolerance)
  ((advance-generator
    ((quality-control rk4 4)
     sysder	
     local-error-tolerance))
   state
   dt
   (* *first-step-scale* dt)
   dt
   advance-monitor
   final-step-monitor))

(define (qc-ctrap-advancer sysder state dt local-error-tolerance)
  ((advance-generator
    ((quality-control c-trapezoid 2)
     sysder			
     local-error-tolerance 
     (* *corrector-convergence-margin*
	local-error-tolerance)))
   state
   dt 
   (* *first-step-scale* dt)
   dt
   advance-monitor
   final-step-monitor))

(define (gear-advancer f&df state dt local-error-tolerance)
  ((gear-advance-generator
    f&df
    (s:dimension state)			;simple dimension
    local-error-tolerance)		;lte
   state				;initial conditions
   dt					;target advance
   (* *first-step-scale* dt)		;initial step
   dt					;max step
   advance-monitor
   final-step-monitor))

(define (gear? method)
  (memq method '(Gear Explicit-Gear gear explicit-gear GEAR)))