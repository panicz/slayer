(define-module (extra threads)
  #:use-module (ice-9 q)
  #:use-module (ice-9 threads)
  #:use-module (oop goops)
  #:export (get! give! make-queue queue?))

(define-class <queue> ()
  (mutex #:init-thunk make-mutex)
  (q #:init-thunk make-q)
  (condition-variable #:init-thunk make-condition-variable))

(define-method (get! #;from (medium <queue>))
  (let ((condition-variable (slot-ref medium 'condition-variable))
	(mutex (slot-ref medium 'mutex))
	(q (slot-ref medium 'q)))
    (with-mutex mutex
		(let again ()
		  (if (q-empty? q)
		      (if (wait-condition-variable 
			   condition-variable mutex)
			  (deq! q)
			  #;else
			  (again))
		      #;else
		      (deq! q))))))

(define-method (give! item #;through (medium <queue>))
  (with-mutex (slot-ref medium 'mutex)
	      (enq! (slot-ref medium 'q) item))
  (signal-condition-variable (slot-ref medium 'condition-variable)))

(define (make-queue) (make <queue>))

(define (queue? object) (is-a? object <queue>))
