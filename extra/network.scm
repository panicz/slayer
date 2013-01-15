(define-module (extra network)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module ((oop goops) #:hide (slot-ref slot-set!))

  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (extra time)
  #:use-module (extra function)
  #:use-module (extra oop)
  #:use-module (extra hset)
  #:use-module ((rnrs) :version (6) 
		:select (make-bytevector 
			 utf8->string string->utf8 
			 bytevector-fill!)
		)
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
	    GATE

	    signature

	    <call-before-slot-set!>
	    register-transaction
	    <unique-id>

	    *object-registry*
	    <registered-object>
	    <register-write-access>
	    remove!

	    <network-object>

	    state-of
	    network-slot-value
	    network-state-of
	    modified?
	    modified-state-of
	    reset-write-registry!

	    
	    )
  #:export-syntax (define-protocol-generator 
		    protocol-add! 
		    protocol-remove!

		    literal-command
		    command-request
		    chain-request
		    chain-request:
		    ))

(define <socket> <file-input-output-port>)

(define <address> <vector>)

(define <protocol> <hashtable>)

(define-class <call-before-slot-set!> ())

(define-generic before-slot-set!)

(define-method (before-slot-set! (object <call-before-slot-set!>) slot-name value)
  ;(<< "calling `before-slot-set!` on " object " with " slot-name ", " value)
  (noop))

(define-method (slot-set! (object <call-before-slot-set!>) slot-name value)
  (before-slot-set! object slot-name value)
  (next-method))

(define-class <unique-id> ()
  (id #:init-thunk (lambda()(gensym "domain")) #:init-keyword #:id))

#;(define*-class <unique-id> (<register-write-access>)
  (id #:init-thunk (lambda()(gensym "domain")) #:allow-override #t
      #:on-write (lambda(this name value)
		   (hash-set! #[this '%%write-registry] name value))))

(define-generic signature)

(define-method (signature (object <unique-id>))
  (list (class-name (class-of object)) (slot-ref object 'id)))

(define-method (display (object <unique-id>) port)
  (display (signature object) port))

(define-method (write (object <unique-id>) port)
  (write (signature object) port))

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
  ;;(<< `(SETTING SLOT ,slot-name OF ,object TO ,value))
  (cond ((equal? slot-name 'id)
	 (hash-remove! *object-registry* #[object 'id])
	 (if #[*object-registry* value] (throw 'id-already-exists))
	 (set! #[*object-registry* value] object)))
  (next-method))

(define-method (initialize (this <registered-object>) args)
  (next-method)
  #;(if (in? (class-name (class-of this)) '(<subspace> <passage>))
      (<< `(ADDING OBJECT ,this TO *object-registry*)))
  (set! #[*object-registry* #[this 'id]] this))

(define-generic remove!)

(define-method (remove! (object <registered-object>))
  (hash-remove! #[object 'registry] #[object 'id]))

(define-class <register-write-access> (<call-before-slot-set!>)
  (%%write-registry #:init-thunk make-hash-table))

(define-generic write-register-slots)

(define-method (write-register-slots (object <register-write-access>))
  (let ((peer-layer (find (lambda(layer)(in? <register-write-access> layer))
			   (superclass-layers 
			    (filter (lambda (class)
				      (in? <register-write-access>
					   (class-ancestors class)))
				    (class-direct-supers (class-of object)))))))
    (difference (class-slot-names (class-of object))
		(append-map class-slot-names peer-layer))))

(define-method (before-slot-set! (object <register-write-access>) slot-name value)
  (and-let* ((slot-names (write-register-slots object))
	     ((in? slot-name slot-names)))
    ;;(<< "REMEMBERING " slot-name)
    (set! #[object : '%%write-registry : slot-name] #t))
  (next-method))


(define-method (modified? (object <register-write-access>))
  (not (hash-empty? #[object '%%write-registry])))

(define-method (reset-write-registry! (object <register-write-access>))
  (for-each (\ hash-remove! #[object '%%write-registry] _) 
	    (hash-keys #[object '%%write-registry])))

(define-class <proto-network-object> (<registered-object>)
  (owners #:init-value #f #:init-keyword #:owners)
  (private-slots #:init-value '() #:allocation #:each-subclass)
  (client-slots #:init-value '() #:allocation #:each-subclass))

(define-class <network-object> (<proto-network-object> <register-write-access>)
  (context #:init-value #f)) ; the subspace to which it belongs

(define-method (write-register-slots (object <network-object>))
  (cons 'context (difference (class-slot-names (class-of object))
			     (class-slot-names <network-object>))))

(define-generic network-slot-value)

(define-method (network-slot-value (slot <top>))
  ;(<< "network-slot-value " slot)
  slot)

(define-method (network-slot-value (object <registered-object>))
  #[object 'id])

(define* (state-of object #:optional (owner #t))
  (map (lambda (slot) (list slot #[object slot]))
       (cons 'context
	     (difference (filter (lambda(symbol)
				   (not (string-match "^%.+" 
						      (symbol->string symbol))))
				 (map first (class-slots (class-of object))))
			 (map first (class-slots <network-object>))
			 #[object 'client-slots]
			 (if owner
			     '()
			     #[object 'private-slots])))))

(define* (network-state-of object #:optional (owner #t))
  (map (match-lambda((slot-name value)
		     (list slot-name (network-slot-value value))))
	 (state-of object owner)))

(define-method (modified-state-of (object <network-object>) (owner <boolean>))
  (map (lambda (slot) (list slot #[object slot]))
       (difference
	(hset->list #[object '%%write-registry])
	(if owner
	    '()
	    #[object 'private-slots]))))

(define-method (modified-state-of (object <network-object>))
  (modified-state-of object #t))

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
    (<< 'sending `(request ,request-id ,content) 'to address)
    (sendto socket (with-output-to-utf8 
		    (\ display `(request ,request-id ,content)))
	    address)))

(define GATE (make-fluid))

(define-method (request content (handler <procedure>))
  (request #[GATE] content handler))

(define-method (remote (socket <socket>) (address <address>) content)
  (<< 'sending content 'to address)
  (sendto socket (with-output-to-utf8
		  (\ display content))
	  address))

(define-method (remote (gate <network-client>) content)
  (match-let (((socket . address) #[gate 'socket.address]))
    (remote socket address content)))

(define-method (remote content)
  (remote #[GATE] content))

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

(define (input-available socket seconds)
  (match-let (((reads () ()) 
	       (select `(,socket) '() '() seconds)))
    (not (null? reads))))

(define (handle-packet! socket clients handle-new-client)
  (define buffer (make-bytevector 1024))
  ;(bytevector-fill! buffer 0)
  (match-let* (((numread . address) (recvfrom! socket buffer))
	       (data (string-take (utf8->string buffer) numread))
	       (client-env (hash-ref clients address)))
    (begin
      (<< `(received ,numread bytes: ,data from ,address)))
    (cond ((and (not client-env) handle-new-client)
	   (set! client-env (handle-new-client socket address))
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
		     (<< `(sending ,result to ,address))
		     (sendto socket (with-output-to-utf8 
				     (\ display result))
			     address)))))
	    (else
	     (<< `(ignoring ,else from ,address))))))))

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
    ((_ (protocol-name connection-socket client-address)
	((binding value) ...)
	(define (fname . args) body ...) ...)
     (define protocol-name 
       (lambda (connection-socket client-address)
	 (let* ((protocol-name #[])
		(binding value) ...)
	   (hash-set! protocol-name (quote fname)
		      (proc args body ...))
	   ...
	   protocol-name))))))

(define-method (try-finalize-transaction (transactions <hashtable>) id)
  (and-let* ((transaction #[transactions id])
	     (finalize #[transaction 'finalize])
	     (count #[transaction 'count])
	     ((equal? (hash-size transaction) (+ count 2)))
	     (stock 
	      (map (match-lambda ((key . value) value))
		   (sort (filter (match-lambda ((key . value)
						(number? key))) 
				 (hash-map->list cons transaction))
			 (match-lambda* (((k1 . v1) (k2 . v2))
					 (< k1 k2)))))))
    (finalize stock)
    (hash-remove! transactions id)))

(define-method (register-transaction transactions id count callback)
  (let ((transaction (or #[transactions id]
			 (let ((transaction #[]))
			   (set! #[transactions id] transaction)
			   transaction))))
    (set! #[transaction 'count] count)
    (set! #[transaction 'finalize] callback)
    (try-finalize-transaction transactions id)))

(define (set-slots! client objects id . slots)
  (or (and-let* ((object #[objects id]))
	(for (name value) in slots
	     (if (equal? name 'context)
		 (<< `(setting context to ,value)))
	     (if (not (equal? name 'id))
		 (if (symbol? value)
		     (begin
		       #;(<< `(trying to replace symbol ,value with object
		       ,#[*object-registry* value]))
		       (slot-set! object name (or #[*object-registry* value]
						  (and (<< `(no object with id 
								,value - setting
								value of a symbol
								instead)) 
						       value))))
		     (slot-set! object name value)))))
      (begin 
	(<< `(failed to set slots of object ,id))
	(remote client `(type-of ,id)))))

(define-protocol-generator (make-client-protocol socket client)
  ((protocol #[])
   (subspaces #[])
   (objects #[])
   (requests #[])
   (transactions #[]))

  (define (objects) ;; those procedures are exported not because server is
    objects) ;; ever going to call them, but because we want to have access
  (define (requests) ;; to these variables from within the client. maybe it's
    requests) ;; not the most elegant solution, but for now i don't have any
  (define (transactions) ;; better idea how to solve it
    transactions)
  (define (subspaces)
    subspaces)

  (define (set-slots! id . slots)
    (apply set-slots! client objects id slots))

  (define (add-subspace! type id . slots)
    ;;(<< `(DEALING WITH ,type ,id))
    (if (not (and-let* ((subspace #[*object-registry* 'id])
			((<< 'SUBSPACE: subspace(class-name(class-of subspace))))
			((equal? (class-name (class-of subspace)) type)))))
	(and-let* ((type-hash #[client 'type-hash])
		   (type #[type-hash type])
		   (subspace (make type #:id id)))
	  ;;(<< `(ADDING ,subspace TO subspaces hash))
	  (set! #[subspaces id] subspace)
	  (for (slot (type* id*)) in slots
	       (cond ((and (in? slot '(left-portal right-portal))
			   (equal? type* '<portal>)
			   (symbol? id*))
		      (let ((portal 
			     (or #[objects id*]
				 (let ((portal (make #[type-hash type*]
						 #:id id*)))
				   (set! #[objects id*] portal)
				   (<< `(we should ask about some
					    details of portal ,id*))
				   portal))))
			(set! #[subspace slot] portal)))
		     (else
		      (<< `(strange value (,slot (,type* ,id*)) in ,slots)))))
	  #;(<< `(FINALLY CREATED ,subspace)))
	;;else
	#;(<< `(SKIPPING ,type ,id))))

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
	       (make type #:id id))))
	  #;(apply set-slots! id slots)
	  ;(apply #[make-client-protocol 'set-slots!] id slots)
	  (apply set-slots! client objects id slots)
	  #;(for (name value) in slots
	       (if (equal? name 'context)
		   (<< `(setting context to ,value)))
	       (if (not (equal? name 'id))
		   (if (symbol? value)
		       (begin
			 ;(<< `(trying to replace symbol ,value with object,#[*object-registry* value]))
			 (slot-set! object name (or #[*object-registry* value]
						    (and (<< `(no object with id 
								  ,value - setting
								  value of a symbol
								  instead)) 
							 value))))
		       (slot-set! object name value))))
	  ;; the structure guarantees, that at this point of execution
	  ;; there is no object with id "id", but only if we're 
	  ;; cooperating with a single server, so it has to be fixed 
	  (set! #[objects id] object) ; in the future!
	  (if #f #f))
	(begin 
	  (<< `(unknown type ,type)))))

  (define (subspaces-becomes-visible! . subspace-ids)
    ;; zapytujemy o szczegoly dotyczace podprzestrzeni
    
    ...)
  
  (define (subspace-no-logner-visible! . subspace-ids)
    ;; usuwamy podprzeszczenie z listy
    ...)

  (define (new-object! subspace-id id . slots)
    ...)

  (define (remove-object! id)
    ...)
  
  (define (move-object! source-id dest-id)
    ...)

  #;(define (remove! id)
    (hash-remove! objects id))
  

  (define (transaction id order data)
    (let ((transaction (or #[transactions id]
			   (let ((transaction #[]))
			     (set! #[transactions id] transaction)
			     transaction))))
      (set! #[transaction order] data)
      (try-finalize-transaction transactions id)))

  (define (response request-id . data)
    (or (and-let* ((request #[requests request-id])
		   ((procedure? request)))
	  (<< `(applying ,data to ,request-id))
	  (safely (apply request data))
	  (hash-remove! requests request-id)
	  (if #f #f))
	(<< `(invalid request ,request-id))))
  #;(define-protocol-generator (makes-client-protocol client)))

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
