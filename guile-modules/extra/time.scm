(define-module (extra time)
  #:use-module (oop goops)
  #:use-module (extra common)
  #:export (
	    pass-ticks-left
	    seconds->ticks ticks->seconds
	    running-time
	    <ticks> <seconds>
	    microseconds
	    )
  #:export-syntax (
		   measured
		   )
  )


(define (microseconds)
  (match-let (((seconds . microseconds) (gettimeofday)))
    (+ (* 1000000 seconds) microseconds)))


(define-syntax-rule (measured call ...)
  (let ((before (microseconds)))
    (call-with-values (lambda () call ...)
      (lambda args
	(display (- (microseconds) before))
	(newline)
	(apply values args)))))

(define <ticks> <integer>)
(define <seconds> <real>)

(define (seconds->ticks t)
  (inexact->exact (* t internal-time-units-per-second)))

(define (ticks->seconds t)
  (exact->inexact (/ t internal-time-units-per-second)))

(define (pass-ticks-left until-how-many-ticks-elapsed to-procedure)
  "pass-ticks-left performs to-procedure until the given amount of \
internal-clock ticks passes. The operation is expected to take exactly \
one parameter -- the number of ticks for the loop to finish. \
Returns the number of additional ticks the loop took."
  (let ((initial-ticks (get-internal-real-time)))
    (let loop ((elapsed-ticks 0))
      (let ((ticks-left (- until-how-many-ticks-elapsed 
			   elapsed-ticks)))
	(if (< 0 ticks-left)
	    (begin
	      (to-procedure ticks-left)
	      (loop (- (get-internal-real-time) initial-ticks)))
	    ticks-left)))))

(define (running-time f . args)
  "running-time returns two values: the normal return value of f, \
after application of args, and the number of seconds it took f to finish"
  (let ((initial-time (get-internal-run-time))) 
    (values 
     (apply f args) 
     (ticks->seconds (- (get-internal-run-time) initial-time)))))

