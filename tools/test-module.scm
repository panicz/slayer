#!/usr/bin/guile \
-e main -s
!#
(set-port-encoding! (current-input-port) "UTF-8")
(set-port-encoding! (current-output-port) "UTF-8")
(set-port-encoding! (current-error-port) "UTF-8")
(fluid-set! %default-port-encoding "UTF-8")

(use-modules (extra common) (extra ref) (extra ansi-color))

(define (test-module name)
  (catch #t
    (lambda ()
      (with-fluids ((RUN-TESTS #t)
		    (TEST-RESULTS `(success: ,name)))
	(or (resolve-module name #:ensure #f)
	    (fluid-set! TEST-RESULTS `(not-found: ,name)))
	(reverse (fluid-ref TEST-RESULTS))))
    (lambda key+args
      `(,name failure: ,key+args))))

(define (extract-fluids var)
  (cond ((list? var)
	 (map extract-fluids var))
	((fluid? var)
	 (extract-fluids (fluid-ref var)))
	(else
	 var)))

(define* (print #:key (color '()) #:rest args)
  (let ((args (remove-keyword-args args)))
    (display (apply colorize-string (apply format #f args) 
		    (if (isatty? (current-output-port)) 
			color
			'())))))

(define (main (program-name args ...))
  (let ((error-code 0))
    (match (test-module (map string->symbol args))
      ((module-name 'failure: reasons ...)
       (print "Failed to load module ~a: " module-name)
       (for (key function message . args) in reasons
	    (apply format #t message (extract-fluids args))
	    (newline))
       (set! error-code 77))
      ((module-name 'not-found:)
       (print "Module ~a not found\n" module-name #:color '(MAGENTA))
       (set! error-code 77))
      ((module-name 'success: results ...)
       (let ((number-of-tests (length results))
	     (i 0))
	 (if (= 0 number-of-tests)
	     (print "no tests found\n")
	     (print "found ~a tests:\n" number-of-tests))
	 (for (status expression result comparison value location) in results
	      (set! i (1+ i))
	      (let ((file #[location 'filename])
		    (line #[location 'line]))
		(if (eq? status 'pass:)
		    (color 'CONCEALED)
		    (color 'WHITE 'ON-RED))
		(print "Test ~a/~a: ~a, line ~a:\n"
		       i number-of-tests file line)
		(print "~y" `(,comparison ,expression ,value)
		       #:color (if (eq? status 'pass:) '() '(MAGENTA)))
		(cond ((eq? status 'pass:)
		       (print "===> #t"))
		      (else
		       (print "===> #f" #:color '(WHITE ON-MAGENTA))
		       (set! error-code -1))))
	      (newline)))))
    (exit error-code)))
