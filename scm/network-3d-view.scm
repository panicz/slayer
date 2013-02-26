(display "loading network-3d-view.scm\n")

(use-modules (extra network))

(define-method (initialize (this <network-client>) args)
  (next-method)
  (let-keywords args 
      #t
      ((address "127.0.0.1:41337")
       (username 'panicz)
       (password 'k0byl4)
       (types '()))
    (for (name type) in types 
	 (hash-set! #[this 'type-hash] name type))
    (match-let* (((address . port) (resolve-address address))
		 (socket (socket PF_INET SOCK_DGRAM 0))
		 (address (make-socket-address AF_INET address port))
		 (protocol (make-client-protocol socket this))
	  #;(mutex #[this 'mutex])
		 (buffer (make-bytevector 1024)))
      (set! #[this 'socket.address] (cons socket address))
      (set! #[this 'protocol] protocol)

      (with-fluids ((GATE this))
	(remote `(login ,username ,password))
	(remote '(join))
		
	;;(pretty-print #1expand #1quote
	(chain-request: 
	 this
	 (for-each (\ apply #[protocol 'add!] _) (requested '(owned-objects)))
	 ;;(display `(owned-objects: ,(requested '(owned-objects))))
	 (for-each (\ apply #[protocol 'add-subspace!] _)
		   (requested '(visible-subspaces)))
	 ;(display (hash-map->alist ((hash-ref protocol 'subspaces))))
	 ;;(<< 'visible-spaces: (requested '(visible-subspaces)))
	 (for-each (\ apply #[protocol 'add!] _)
		   (requested '(visible-objects)))
	 ;;(<< 'visible-objects: (requested '(visible-objects)))
	 #;(for-each (lambda(object)
		     (request this `(state-of ,#[object 'id])
			      (lambda (state)
				(if (list? state)
				    (for (slot value) in state
					 (slot-set! slot value))))))
		   (hash-map->list (lambda(id object)object)
				   (#[protocol 'objects])))
	 (let ((result (requested `(transaction 
				    ,(gensym "tr-")
				    (visible-objects+their-states)))))
	   (match result
	     (('begin-transaction id count)
	      (register-transaction 
	       (#[protocol 'transactions]) id count
	       (lambda (objects)
		 ;(<< `(setting slots of ,objects))
		 (for-each (match-lambda ((type id . slots)
					  (apply #[protocol 'add!] type id slots))
			     (else (throw 'invalid-argument else))
			     )
			   objects))))
	     (else
	      (<< `(unsupported transaction result ,result)))))
	 (remote this '(inform-me-about-changes))))
      (let ((code (register-userevent ; this code is executed 
		   (lambda (data socket.address) ; by the main thread
		     #;(lock-mutex mutex)
		     #;(begin
		       (display `(received ,data) *stdout*)
		       (newline))
		     (match-let (((socket . address) socket.address))
		       (match (with-input-from-string data read)
			 ((proc args ...)
			  (let ((result 
				 (safely 
				  (apply #[protocol proc] args))))
			    (if (not (unspecified? result))
				(sendto socket
					(with-output-to-utf8 
					 (\ write result))
					address))))
			 (else
			  (<< `(ignoring ,else)))))
		     #;(unlock-mutex mutex)))))
	(call-with-new-thread 
	 (lambda ()
	   (while #t
	     ;(bytevector-fill! buffer 0)
	     (match-let (((nread . address)(recvfrom! socket buffer)))
	       (let ((data (substring (utf8->string buffer) 0 nread)))
		 ; strange: writing to (current-error-port) causes some strange
		 ; error at some point, so we write to the default port
		 (<< `(received ,nread bytes: ,data));(newline)(force-output)
		 (generate-userevent 
		  code 
		  data
		  (cons socket address)))))))
	this))))


(define-class <network-3d-view> (<3d-view> <network-client>))

(define-method (draw (this <network-3d-view>))
  #;(lock-mutex #[this 'mutex])
  (set! #[this 'objects] (hash-map->list 
			  (lambda (key value) value) 
			  (#[this : 'protocol : 'objects])))
  #;(begin(display #[this 'objects])(newline))
  (next-method)
  #;(unlock-mutex #[this 'mutex]))

(display "loaded network-3d-view.scm\n")
