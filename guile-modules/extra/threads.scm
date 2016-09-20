(define-module (extra threads)
  #:use-module (ice-9 nice-9)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-18)
  #:export (make-channel channel? send! receive! channel-fold
			 visible snapshot))

(define (make-channel)
  `(,(make-q) ,(make-mutex) ,(make-condition-variable)))

(define (channel? c)
  (and-let* (((queue mutex signal) c)
	     ((q? queue))
	     ((mutex? mutex))
	     ((condition-variable? signal)))))

(define (send! data #;through channel)
  (let (((queue mutex signal) channel))
    (with-mutex mutex
      (enq! queue data))
    (signal-condition-variable signal))) 

(define (receive! #;from channel)
  (let (((queue mutex signal) channel))
    (with-mutex mutex
      (while (q-empty? queue)
	(wait-condition-variable signal mutex))
      (deq! queue))))

(define (channel-fold update state channel)
  (let ((message (receive! #;from channel)))
    (channel-fold update (update state message) channel)))

(define (visible . state)
  (thread-specific-set! (current-thread) state)
  (apply values state))

(define (snapshot thread)
  (and-let* (((items ...) (thread-specific thread)))
    (apply values items)))

