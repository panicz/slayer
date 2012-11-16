(define-module (extra network)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (extra time)
  #:use-module (extra function)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module ((rnrs) :version (6))
  #:export (
	    make-client-protocol
	    handle-packet!
	    input-available
	    handle-clients
	    make-server-cycle
	    <socket>
	    <address>
	    )
  #:export-syntax (define-protocol-generator 
		    protocol-add! 
		    protocol-remove!))

(define <socket> <file-input-output-port>)
(define <address> <vector>)
(define <protocol> <hashtable>)

(define (transmit recipent data)
  (match-let (((socket . address) recipent))
    (sendto socket data address)))

(define (receive! sender  buffer)
  (match-let (((socket . address) sender))
    (recvfrom! socket buffer)))

(define* (make-client-protocol typehash #:key (add-symbol 'add!) 
			       (remove-symbol 'remove!) 
			       (set-slots-symbol 'set-slots!)
			       (objects-symbol 'objects))
  (let ((protocol (make-hash-table))
	(objects (make-hash-table)))
    (set! #[protocol objects-symbol] objects)
    (set! #[protocol set-slots-symbol]
	  (lambda (id . slots)
	    (and-let* ((object #[objects id]))
	      (for (name value) in slots
		   (slot-set! object name value)))))
    (set! #[protocol add-symbol]
	  (lambda (type id . slots)
	    (and-let* ((type #[typehash type])
		       (object (make type)))
	      (for (name value) in slots
		   (slot-set! object name value))
	      (set! #[objects id] object))))
    (set! #[protocol remove-symbol]
	  (lambda (id)
	    (hash-remove! objects id)))
    protocol))

(define (input-available socket seconds)
  (match-let (((reads () ()) 
	       (select `(,socket) '() '() seconds)))
    (not (null? reads))))

(define (handle-packet! socket clients handle-new-client)
  (define buffer (make-bytevector 1024))
  (match-let* (((numread . address) (recvfrom! socket buffer))
	       (data (substring (utf8->string buffer) 0 numread))
	       (client-env (hash-ref clients address)))
    (cond ((and (not client-env) handle-new-client)
	   (set! client-env (handle-new-client address))
	   (if client-env
	       (hash-set! clients address client-env))))
    (if (and client-env address)
	(let ((packet (safely (read (open-input-string data)))))
	  (match packet
	    ((proc . args)
	     (let ((result (safely (apply (hash-ref client-env proc) 
					  args))))
	       (if (not (unspecified? result))
		   (sendto socket (with-output-to-utf8 
				   (\ display result))
			   address)))))))))

(define-method (handle-clients (socket <socket>) 
			       (clients <protocol>) 
			       (register-protocol <procedure> ) 
			       (period <ticks>))
  (pass-ticks-left 
   period 
   (lambda (ticks-left)
     (if (input-available socket  (ticks->seconds ticks-left))
	 (handle-packet! socket clients register-protocol)))))

(define-syntax protocol-add!
  (syntax-rules ()
    ((_ protocol-name ((fname arg ...) body ...) ...)
     (begin 
       (hash-set! protocol-name (quote fname)
		  (proc (arg ...) body ...))
       ...))))

(define-syntax protocol-remove!
  (syntax-rules ()
    ((_ protocol-name fname1 ...)
     (for-each (proc (sym)(hash-remove! protocol-name sym)) 
	       (list (quote fname1) ...)))))

(define-syntax define-protocol-generator
  (syntax-rules ()
    ((_ (protocol-name client-address)
	bindings
	(((fname arg ...) body ...) ...))
     (define protocol-name 
       (lambda (client-address)
	 (let ((protocol-name (make-hash-table)))
	   (let* bindings
	     (hash-set! protocol-name (quote fname)
			(proc (arg ...) body ...))
	     ...)
	   protocol-name))))))

(define-method 
  (make-server-cycle (socket <socket>) 
		     (clients <hashtable>) 
		     (register-protocol <procedure>)
		     (update-world <procedure> #;obj->obj)
		     (respond <procedure> #;cn,pr->?)
		     (period <seconds>))
  (let ((ticks (seconds->ticks period)))
    (lambda ()
      (handle-clients socket clients register-protocol 
		      (seconds->ticks period))
      (update-world)
      (for-each (match-lambda ((address . protocol)
			       (respond socket address protocol)))
		(hash-map->list cons clients)))))


  