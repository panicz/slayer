(display "loading goose.scm\n")

(use-modules (extra network))

(define-class <goose-client> ()
  (socket.address #:init-value #f)
  #;(mutex #:init-thunk make-mutex)
  (protocol #:init-thunk make-hash-table)
  (type-hash #:init-thunk make-hash-table)
  ;; type-hash contains a hash whose keys are type-names (symbols)
  ;; and values are GOOPS types, thus making it closer

  ;; the keywords the class is meant to be
  ;; initialized with:
  ;;   #:address "nu.mb.e.r:port"
  ;;   #:types '((typename type) ...)
  ;;   #:username "name"
  ;;   #:password "phrase"
  )

(define (resolve-address string)
  (let-values (((address port) (match (string-split string #\:)
				 ((address ... port)
				  (values (string-join address "") 
					  (string->number port)))
				 ((address)
				  (values address 41337)))))
    (let ((address (if (string-match "^([0-9]{1,3}[.]){3}[0-9]{1,3}"
				     address)
		       (inet-aton address)
		       INADDR_LOOPBACK)))
      (cons address port))))

(define-method (initialize (this <goose-client>) args)
  (next-method)
  (let-keywords args 
      #t
      ((address "127.0.0.1:41337")
       (username "anonymous") ;; currently these are dismissed
       (password "")
       (types '()))
    (for (name type) in types 
	 (hash-set! #[this 'type-hash] name type))
    (match-let* (((address . port) (resolve-address address))
		 (socket (socket PF_INET SOCK_DGRAM 0))
		 (address (make-socket-address AF_INET address port))
		 (protocol (make-client-protocol #[this 'type-hash]))
	  #;(mutex #[this 'mutex])
	  (buffer (make-bytevector 1024)))
      (set! #[this 'socket.address] (cons socket address))
      (set! #[this 'protocol] protocol)
      (let ((code (register-userevent ; this code is executed 
		   (lambda (data socket.address) ; by the main thread
		     #;(lock-mutex mutex)
		     (match-let (((socket . address) socket.address)
				 ((proc . args) 
				  (with-input-from-string data read)))
		       (let ((result (apply #[protocol proc] args)))
			 (if (not (unspecified? result))
			     (sendto socket
				     (with-output-to-utf8 
				      (\ write result))
				     address))))
		     #;(unlock-mutex mutex)))))
	(call-with-new-thread 
	 (lambda ()
	   (while #t
	     (match-let (((nread . address)(recvfrom! socket buffer)))
	       (generate-userevent 
		code 
		(substring (utf8->string buffer) 0 nread) 
		(cons socket address))))))
	this))))

(define-class <goose-view> (<3d-view> <goose-client>))

(define-method (draw (this <goose-view>))
  #;(lock-mutex #[this 'mutex])
  (set! #[this 'objects] (hash-map->list 
			  (lambda (key value) value) 
			  #[this : 'protocol : 'objects]))
  (next-method)
  #;(unlock-mutex #[this 'mutex]))


(display "loaded goose.scm\n")
