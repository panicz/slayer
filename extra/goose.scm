(define-module (extra goose)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-2)
  :use-module (srfi srfi-11)
  :use-module (oop goops)
  :use-module (ice-9 match)
  :use-module (ice-9 optargs)
  :use-module (ice-9 regex)
  ;:use-module (extra slayer)
  :use-module (extra ref)
  :use-module (extra common)
  :use-module (extra network)
  :use-module ((rnrs) :version (6))
  :export (
	   objects-visible-to
	   <goose>
	   <goose-client>
	   request
	   command
	   state-of
	   ))

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
	  (return (lambda(tokens current-token)
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

(define-class <goose> ()
  (owners #:init-value #f #:init-keyword #:owners)
  (context #:init-value #f) ; the subspace to which it belongs
  (private-slots #:init-value '()) ; slots visible only to the owners
  (client-slots #:init-value '())
  (id #:init-value 0))

(define-method (initialize (this <goose>) args)
  (next-method)
  (match-let (((type ... id)
	       (with-input-from-string (with-output-to-string (\ display this)) read)))
    (set! #[this 'id] id)))

(define* (state-of object #:optional (owner #t))
  (map (lambda (slot) (list slot #[object slot]))
       (lset-difference equal?
			(map first (class-slots (class-of object)))
			(map first (class-slots <goose>))
			#[object 'client-slots]
			(if owner
			    #[object 'private-slots]
			    '()))))

(define-method (objects-visible-to object)
  (list object))

(define-class <goose-client> ()
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

(define-method (request (gate <goose-client>) content handler)
  (match-let (((socket . address) #[gate 'socket.address])
	      (requests #[#[gate 'protocol] 'requests])
	      (request-id (gensym "r-")))
    (set! #[requests request-id] handler)
    (sendto socket (with-output-to-utf8 
		    (\ display `(request ,request-id ,content)))
	    address)))

(define-method (command (gate <goose-client>) content)
  (match-let (((socket . address) #[gate 'socket.address]))
    (sendto socket (with-output-to-utf8
		    (\ display content))
	    address)))
