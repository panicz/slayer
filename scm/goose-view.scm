(display "loading goose.scm\n")

(use-modules (extra network)
	     (extra goose))

(define-method (initialize (this <goose-client>) args)
  (next-method)
  (let-keywords args 
      #t
      ((address "127.0.0.1:41337")(username 'panicz)(password 'k0byl4)
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
      (sendto socket (with-output-to-utf8 
		      (\ display `(login ,username ,password)))
	      address)
      (sendto socket (string->utf8 "(join)") address)
      (let ((code (register-userevent ; this code is executed 
		   (lambda (data socket.address) ; by the main thread
		     #;(lock-mutex mutex)
		     #;(begin
		       (display `(received ,data) *stdout*)
		       (newline))
		     (match-let (((socket . address) socket.address))
		       (match (with-input-from-string data read)
			 ((proc args ...)
			  (let ((result(apply #[protocol proc] args)))
			    (if (not (unspecified? result))
				(sendto socket
					(with-output-to-utf8 
					 (\ write result))
					address))))
			 (else
			  (display `(ignoring ,else) *stdout*))))
		     #;(unlock-mutex mutex)))))
	(call-with-new-thread 
	 (lambda ()
	   (while #t
	     (match-let (((nread . address)(recvfrom! socket buffer)))
	       #;(begin (display `(received ,(utf8->string buffer))
			       *stdout*) (newline))
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
  #;(begin(display #[this 'objects])(newline))
  (next-method)
  #;(unlock-mutex #[this 'mutex]))

(display "loaded goose.scm\n")
