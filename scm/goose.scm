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

(define (make-connection address)
  (let*  ((address+port (string-split address #\:))
	  (address (if (not (null? address+port)) 
		      (inet-aton (first address+port)) 
		      INADDR_LOOPBACK))
	  (port (if (> (length address+port) 1)
		   (string->number (second address+port))
		   41337))
	  (socket (socket PF_INET SOCK_DGRAM 0))
	  (address (make-socket-address AF_INET address port)))
    (cons socket address)))


(define (transmit recipent data)
  (match-let (((socket . address) recipent))
	     (sendto socket data address)))

(define (receive! sender  buffer)
  (match-let (((socket . address) sender))
	     (recvfrom! socket buffer)))

(define (make-client-protocol typehash)
  (let ((protocol (make-hash-table))
	(objects (make-hash-table)))
    (set! #[protocol 'objects] objects)
    (set! #[protocol 'set-slots!]
	  (lambda (id . slots)
	    (and-let* ((object #[objects id]))
	      (for-each (match-lambda ((name value)
				       (slot-set! object name value)))
			  slots))))
    (set! #[protocol 'add!]
	  (lambda (type id . slots)
	    (and-let* ((type #[typehash type])
		       (object (make type)))
	      (for-each (match-lambda ((name value)
				       (slot-set! object name value)))
			slots)
	      (set! #[objects id] object))))
    (set! #[protocol 'remove!]
	  (lambda (id)
	    (hash-remove! objects id)))
    protocol))

(define-syntax-rule (export-types typename ...)
  (list ((quote typename) typename) ...))

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
      (let ((code (register-userevent 
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
