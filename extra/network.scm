(define-module (extra network)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (extra time)
  #:use-module (extra function)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (oop goops)
  #:use-module ((rnrs) :version (6))
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
	    command


	   <unique-id>
	   *object-registry*
	   <registered-object>
	   <network-object>
	   state-of

	    )
  #:export-syntax (define-protocol-generator 
		    protocol-add! 
		    protocol-remove!))

(define <socket> <file-input-output-port>)
(define <address> <vector>)
(define <protocol> <hashtable>)


(define-class <unique-id> ()
  (id #:init-value 0))

(define-method (initialize (this <unique-id>) args)
  (next-method)
  (match-let (((type ... id)
	       (with-input-from-string 
		   (with-output-to-string (\ display this)) read)))
    (set! #[this 'id] id)))


(define *object-registry* (make-hash-table))
(define-class <registered-object> (<unique-id>)
  (registry #:init-value *object-registry*
	    #:allocation #:class))

(define-method (initialize (this <registered-object>) args)
  (next-method)
  (set! #[#[this 'registry] #[this 'id]] this))

(define-generic remove!)

(define-method (remove! (object <registered-object>))
  (hash-remove! #[object 'registry] #[object 'id]))

(define-class <network-object> (<registered-object>)
  (owners #:init-value #f #:init-keyword #:owners)
  (context #:init-value #f) ; the subspace to which it belongs
  (private-slots #:init-value '() #:allocation #:each-subclass)
  (client-slots #:init-value '() #:allocation #:each-subclass))

(define* (state-of object #:optional (owner #t))
  (map (lambda (slot) (list slot #[object slot]))
       (lset-difference equal?
			(map first (class-slots (class-of object)))
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
  (receive #:init-value noop) ; this is a procedure called
  ;; when a packet is received
  ;; type-hash contains a hash whose keys are type-names (symbols)
  ;; and values are GOOPS types, thus making it closer
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
	      (requests #[#[gate 'protocol] 'requests])
	      (request-id (gensym "r-")))
    (set! #[requests request-id] handler)
    (sendto socket (with-output-to-utf8 
		    (\ display `(request ,request-id ,content)))
	    address)))

(define-method (command (gate <network-client>) content)
  (match-let (((socket . address) #[gate 'socket.address]))
    (sendto socket (with-output-to-utf8
		    (\ display content))
	    address)))

(define* (make-client-protocol client #:key (add-symbol 'add!)
 			       (remove-symbol 'remove!) 
			       (set-slots-symbol 'set-slots!)
			       (response-symbol 'response)
			       (requests-symbol 'requests)
			       (objects-symbol 'objects))
  (let ((protocol (make-hash-table))
	(objects (make-hash-table))
	(requests (make-hash-table)))
    (set! #[protocol objects-symbol] objects)
    (set! #[protocol requests-symbol] requests)
    (set! #[protocol set-slots-symbol]
	  (lambda (id . slots)
	    (or (and-let* ((object #[objects id]))
		  (for (name value) in slots
		       (slot-set! object name value)))
		(begin 
		  (display `(failed to set slots of object ,id))
		  (command client `(type-of ,id))
		  (newline)))))
    (set! #[protocol add-symbol]
	  (lambda (type id . slots)
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
		  (for (name value) in slots
		       (slot-set! object name value))
		  (set! #[objects id] object)
		  (if #f #f))
		(begin 
		  (display `(unknown type ,type))
		  (newline)))))
    (set! #[protocol remove-symbol]
	  (lambda (id)
	    (hash-remove! objects id)))
    (set! #[protocol response-symbol]
	  (lambda (request-id . data)
	    (or (and-let* ((request #[requests request-id])
			   ((procedure? request)))
		  (safely (apply request data))
		  (hash-remove! requests request-id)
		  (if #f #f))
		(begin 
		  (display `(invalid request ,request-id))
		  (newline)))))
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
  (syntax-rules ()
    ((_ (protocol-name client-address)
	bindings
	(((fname arg ...) body ...) ...))
     (define protocol-name 
       (lambda (client-address)
	 (let ((protocol-name (make-hash-table)))
	   (let* bindings
	     (hash-set! protocol-name (quote fname)
			(proc (arg ...) 
			      #;(begin 
			      (display `(received (fname arg ...) from ,client-address))
			      (newline))
			      body ...))
	     ...)
	   protocol-name))))))

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



(read-hash-extend 
 #\<
 (lambda (char port)
   (let* ((chars->data (lambda(l)
			 (let ((s (list->string (reverse l))))
			   (cond 
			    ((string-match "^[0-9a-fA-F]+$" s)
			     (string->number (string-append "#x" s)))
			    (else
			     (string->symbol s))))))
	  (return 
	   (lambda (tokens current-token)
	     (reverse
	      (if (null? current-token)
		  tokens
		  (cons (chars->data current-token) tokens))))))
     (let loop ((level 0)
		(current-token '())
		(tokens '()))
       (let ((char (read-char port)))
	 (cond ((eof-object? char)
		(return tokens current-token))
	       ((char-whitespace? char)
		(if (null? current-token)
		    (loop level current-token tokens)
		    (loop level '() (cons (chars->data current-token)
					  tokens))))
	       ((equal? char #\<)
		(loop (1+ level) (cons char current-token) tokens))
	       ((equal? char #\>)
		(if (= level 0)
		    (return tokens current-token)
		    (loop (1- level) (cons char current-token)
			  tokens)))
	       (else
		(loop level (cons char current-token) tokens))))))))
