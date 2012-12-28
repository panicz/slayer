(define-module (extra network)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)

  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (extra time)
  #:use-module (extra function)
  #:use-module (extra oop)
  #:use-module ((rnrs) :version (6) 
		:select (make-bytevector utf8->string string->utf8))
  #:export (
	    make-client-protocol
	    handle-packet!
	    input-available
	    handle-clients
	    make-server-cycle
	    resolve-address
	    <socket>
	    <address>
	    <network-client>
	    request
	    remote

	    <call-before-slot-set!>
	    <>

	    <unique-id>

	    *object-registry*
	    <registered-object>
	    remove!

	    <network-object>
	    state-of
	    )
  #:export-syntax (define-protocol-generator 
		    protocol-add! 
		    protocol-remove!))


(define <socket> <file-input-output-port>)

(define <address> <vector>)

(define <protocol> <hashtable>)

(define-class <call-before-slot-set!> ())

(define-generic before-slot-set!)

(define-method (before-slot-set! (object <call-before-slot-set!>) slot-name value)
  ;(<< "calling `before-slot-set!` on " object)
  (noop))

(define-method (slot-set! (object <call-before-slot-set!>) slot-name value)
  (before-slot-set! object slot-name value)
  (next-method))

(define-class <unique-id> ()
  (id #:init-thunk (lambda()(gensym "domain"))))

#;(define*-class <unique-id> (<register-write-access>)
  (id #:init-thunk (lambda()(gensym "domain")) #:allow-override #t
      #:on-write (lambda(this name value)
		   (hash-set! #[this '%%write-registry] name value))))

(define-method (display (object <unique-id>) port)
  (display (list (class-name (class-of object)) #[object 'id]) port))

(define-method (write (object <unique-id>) port)
  (write (list (class-name (class-of object)) (slot-ref object 'id)) port))

(define *object-registry* #[])

#;(define-class <registered-object> (<unique-id>)
  (id #:allocation #:virtual
      #:slot-ref (lambda (this) #[this '%id])
      #:slot-set! (lambda (this id)
		    (hash-remove! *object-registry* #[this '%id])
		    (if #[*object-registry* id] (throw 'id-already-exists))
		    (set! #[this '%id] id)
		    (set! #[*object-registry* id] this))))

(define-class <registered-object> (<unique-id> <call-before-slot-set!>))

(define-method (before-slot-set! (object <registered-object>) slot-name value)
  (cond ((equal? slot-name 'id)
	 (hash-remove! *object-registry* #[object 'id])
	 (if #[*object-registry* value] (throw 'id-already-exists))
	 (set! #[*object-registry* value] object)))
  (next-method))

(define-method (initialize (this <registered-object>) args)
  (next-method)
  (set! #[*object-registry* #[this 'id]] this))

(define-generic remove!)

(define-method (remove! (object <registered-object>))
  (hash-remove! #[object 'registry] #[object 'id]))

(define-class <register-write-access> (<call-before-slot-set!>)
  (%%write-registry #:init-thunk make-hash-table))

(define-method (before-slot-set! (object <register-write-access>) slot-name value)
  (and-let* ((peer-layer (find (lambda(layer)(in? <register-write-access> layer))
			       (superclass-layers (class-of object))))
	     (slot-names (difference (class-slot-names (class-of object))
				     (append-map class-slot-names peer-layer)))
	     ((in? slot-name slot-names)))
    (set! #[object : '%%write-registry : slot-name] #t))
  (next-method))

(define-class <proto-network-object> (<registered-object>)
  (owners #:init-value #f #:init-keyword #:owners)
  (context #:init-value #f) ; the subspace to which it belongs
  (private-slots #:init-value '() #:allocation #:each-subclass)
  (client-slots #:init-value '() #:allocation #:each-subclass))

(define-class <network-object> (<proto-network-object> <register-write-access>))

(define* (state-of object #:optional (owner #t))
  (map (lambda (slot) (list slot #[object slot]))
       (difference (filter (lambda(symbol)
			     (not (string-match "^%.+" 
						(symbol->string symbol))))
			   (map first (class-slots (class-of object))))
		   (map first (class-slots <network-object>))
		   #[object 'client-slots]
		   (if owner
		       '()
		       #[object 'private-slots]))))

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

(define-class <network-client> ()
  (socket.address #:init-value #f)
  #;(mutex #:init-thunk make-mutex)
  (protocol #:init-thunk make-hash-table)
  (type-hash #:init-thunk make-hash-table)
  ;; type-hash contains a hash whose keys are type-names (symbols)
  ;; and values are GOOPS types, thus making it closer
  (receive #:init-value noop) ; this is a procedure called
  ;; when a packet is received
  ;; 
  ;; the keywords the class is meant to be
  ;; initialized with:
  ;;   #:address "nu.mb.e.r:port"
  ;;   #:types '((typename type) ...)
  ;;   #:username "name"
  ;;   #:password "phrase"
  )

(define-method (request (gate <network-client>) content handler)
  (match-let (((socket . address) #[gate 'socket.address])
	      (requests (#[#[gate 'protocol] 'requests]))
	      (request-id (gensym "r-")))
    (set! #[requests request-id] handler)
    (sendto socket (with-output-to-utf8 
		    (\ display `(request ,request-id ,content)))
	    address)))

(define-method (remote (gate <network-client>) content)
  (match-let (((socket . address) #[gate 'socket.address]))
    (sendto socket (with-output-to-utf8
		    (\ display content))
	    address)))

(define (input-available socket seconds)
  (match-let (((reads () ()) 
	       (select `(,socket) '() '() seconds)))
    (not (null? reads))))

(define (handle-packet! socket clients handle-new-client)
  (define buffer (make-bytevector 1024))
  (match-let* (((numread . address) (recvfrom! socket buffer))
	       (data (substring (utf8->string buffer) 0 numread))
	       (client-env (hash-ref clients address)))
    (begin
      (display `(received ,data from ,address))
      (newline))
    (cond ((and (not client-env) handle-new-client)
	   (set! client-env (handle-new-client address))
	   (begin (display `(a new client connected from ,address))
		  (newline))
	   (if client-env
	       (hash-set! clients address client-env))))
    (if (and client-env address)
	(and-let*((packet(safely(with-input-from-string data read))))
	  (match packet
	    ((fn args ...)
	     (let ((result (safely (apply #[client-env fn] args))))
	       (if (not (unspecified? result))
		   (begin
		     (display `(sending ,result to ,address))
		     (newline)
		     (sendto socket (with-output-to-utf8 
				     (\ display result))
			     address)))))
	    (else
	     (display `(ignoring ,else from ,address))))))))

(define-method (handle-clients (socket <socket>) 
			       (clients <protocol>) 
			       (register-protocol <procedure> ) 
			       (period <ticks>))
  (pass-ticks-left 
   period 
   (lambda (ticks-left)
     (if (input-available socket (ticks->seconds ticks-left))
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
  (syntax-rules (define)
    ((_ (protocol-name client-address)
	((binding value) ...)
	(define (fname . args) body ...) ...)
     (define protocol-name 
       (lambda (client-address)
	 (let* ((protocol-name #[])
		(binding value) ...)
	   (hash-set! protocol-name (quote fname)
		      (proc args body ...))
	   ...
	   protocol-name))))))

(define-protocol-generator (make-client-protocol client)
  
  ((protocol #[])
   (subspaces #[])
   (objects #[])
   (requests #[]))

  (define (objects)
    objects)

  (define (requests)
    requests)

  (define (set-slots! id . slots)
    (or (and-let* ((object #[objects id]))
	  (for (name value) in slots
	       (if (equal? name 'context)
		   (display `(setting context to ,value) (current-error-port)))
	       (slot-set! object name value)))
	(begin 
	  (<< `(failed to set slots of object ,id))
	  (remote client `(type-of ,id)))))

  (define (add! type id . slots)
    (or (and-let* 
	    ((type #[#[client 'type-hash] type])
	     (object 
	      (or 
	       (and-let* ((object 
			   #[*object-registry* id])
			  (class
			    (class-of object))
			  ((equal? class type)))
		 object)
	       (make type))))
	  #;(apply set-slots! id slots)
	  (for (name value) in slots
	       (slot-set! object name value))
	  ;; the structure guarantees, that at this point of execution
	  ;; there is no object with id "id", but only if we're 
	  ;; cooperating with a single server, so it has to be fixed 
	  (set! #[objects id] object) ; in the future!
	  (if #f #f))
	(begin 
	  (<< `(unknown type ,type)))))

  (define (remove! id)
    (hash-remove! objects id))

  (define (response request-id . data)
    (or (and-let* ((request #[requests request-id])
		   ((procedure? request)))
	  (safely (apply request data))
	  (hash-remove! requests request-id)
	  (if #f #f))
	(begin 
	  (display `(invalid request ,request-id))
	  (newline))))
  #;(define-protocol-generator (make-client-protocol client)))

(define-method 
  (make-server-cycle (socket <socket>) 
		     (clients <hashtable>) 
		     (register-protocol <procedure>)
		     (update-world <procedure> #;obj->obj)
		     (broadcast <procedure> #;sock,addr,pr->?)
		     (period <seconds>))
  (let ((ticks (seconds->ticks period)))
    (lambda ()
      (handle-clients socket clients register-protocol 
		      (seconds->ticks period))
      (update-world)
      (for-each (match-lambda ((address . protocol)
			       (broadcast socket address protocol)))
		(hash-map->list cons clients)))))
