(display "loading network-3d-view.scm\n")

(use-modules (extra network))

;; the chain-request macro is implemented only partially, only
;; to support currently needed cases -- so the 'requested' clause
;; can only appear at the least nested level

(define (extract-request form)
  (tree-find (match-lambda (('requested command) #t)
	       (else #f))
	     (list form)))


(define-macro (command-request form)
  (or (and-let* ((extract-request (request form)))
	(second request))
      (throw 'invalid-form)))

(define-macro (literal-command xarg form)
  (define (transform-command xarg form)
    (match form
      (('requested command)
       xarg)
      (('quote datum)
       (list 'quote datum))
      ((items ...)
       (map (lambda(form)(transform-command xarg form)) items))
      (else
       else)))
  (transform-command xarg form))

(define-macro (chain-request gate commands)
  (match commands
    ((last-command)
     (let ((request (extract-request last-command)))
       (match request
	 (('requested command)
	  `(request ,gate ,command
		    (lambda (arg)
		      (literal-command arg ,last-command))))
	 (else
	  `(begin ,last-command)))))
    ((first-command . remaining-commands)
     (let ((request (extract-request first-command)))
       (match request
	 (('requested command)
	  `(request ,gate ,command
		    (lambda(arg)
		      (literal-command arg ,first-command)
		      (chain-request ,gate ,remaining-commands))))
	 (else
	  `(begin ,first-command
	    (chain-request ,remaining-commands))))))))

(define-macro (chain-request: gate . commands)
  `(chain-request ,gate ,commands))

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
		 (protocol (make-client-protocol this))
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
	 ;(display `(owned-objects: ,(requested '(owned-objects))))
	 (for-each (\ apply #[protocol 'add-subspace!] _)
		   (requested '(visible-subspaces)))
	 ;(<< 'visible-spaces: (requested '(visible-subspaces)))
	 (for-each (\ apply #[protocol 'add!] _)
		   (requested '(visible-objects)))
	 ;(<< 'visible-objects: (requested '(visible-objects)))
	 #;(for-each (lambda(object)
		     (request this `(state-of ,#[object 'id])
			      (lambda (state)
				(if (list? state)
				    (for (slot value) in state
					 (slot-set! slot value))))))
		   (hash-map->list (lambda(id object)object)
				   (#[protocol 'objects])))
	 (let* ((tid (gensym "tr-"))
		(result (requested `(transaction 
				     ,tid 
				     (visible-objects+their-states)))))
	   (match result
	     (('transaction-begin id count)
	      (register-transaction 
	       id count
	       (lambda (objects)
		 (for-each (match-lambda ((type id . slots)
					  (apply #[protocol 'add!] type id slots)))
			   objects))))
	     (else
	      (<< `(unsupported transaction result ,result)))))))
	 ;(remote this '(inform-me-about-changes))))
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
		 (display `(received ,nread bytes: ,data))(newline)(force-output)
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
