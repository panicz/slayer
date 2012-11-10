(display "loading goose.scm\n")

(define-class <goose-client> ()
  (connection #:init-value #f)
  #;(mutex #:init-thunk make-mutex)
  (protocol #:init-thunk make-hash-table)
  (type-hash #:init-thunk make-hash-table)
  ;; the keywords the class is meant to be
  ;; initialized with:
  ;;   #:address "nu.mb.e.r:port"
  ;;   #:types '((typename type) ...)
  ;;   #:username "name"
  ;;   #:password "phrase"
  )

(define-method (initialize (this <goose-client>) args)
  (next-method)
  (let-keywords args 
      #t
      ((address "127.0.0.1:41337")
       (username "anonymous") ;; currently these are dismissed
       (password "")
       (types '()))
    (for-each (match-lambda ((name type)
			     (hash-set! #[this 'type-hash] name type)))
	      types)
    (let ((connection (make-connection address))
	  (protocol (make-client-protocol #[this 'type-hash]))
	  #;(mutex #[this 'mutex])
	  (buffer (make-bytevector 1024)))
      (set! #[this 'connection] connection)
      (set! #[this 'protocol] protocol)
      (let ((code (register-userevent ; this code is executed by the main thread
		   (lambda (data connection)
		     #;(lock-mutex mutex)
		     (match-let (((proc . args) (with-input-from-string data read)))
		       (let ((result (apply #[protocol proc] args)))
			 (if (not (unspecified? result))
			     (transmit connection 
				       (string->utf8 (with-output-to-string (lambda()(write result))))))))
		     #;(unlock-mutex mutex)))))
	(call-with-new-thread 
	 (lambda ()
	   (while #t
	     (match-let (((nread . address) (receive! connection buffer)))
	       (generate-userevent 
		code 
		(substring (utf8->string buffer) 0 nread) 
		connection)))))
	this))))

(define-class <goose-view> (<3d-view> <goose-client>))

(define-method (draw (this <goose-view>))
  #;(lock-mutex #[this 'mutex])
  (set! #[this 'objects] (hash-map->list (lambda (key value) value)  #[this : 'protocol : 'objects]))
  (next-method)
  #;(unlock-mutex #[this 'mutex]))

(display "loaded goose.scm\n")
