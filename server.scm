#!/usr/bin/guile -s
!#

(use-modules 
 (extra ref) (extra common) (extra network) (extra function) (extra hset)
 (extra oop)
 (extra subspace)
 #;((rnrs) :version (6)))

(define *users* #[])

(define *logged-users* '())

;; server is stupid and reliable; client is fancy and fallable
(load "game.scm") ; defines update-world!, as well as procedures
;; that can be called remotely by the client

(for-each
 (match-lambda((username password)
	       (hash-set! *users* username password)))
 '((panicz k0byl4)
   (drcz 12345)
   (polak żółć)))

(define current-step 0)
(define (step!)
  (set! current-step (bitwise-and #xffff (1+ current-step))))

(define *observers* '())

;;(load "map.scm")
(let ((subspace-1 (make <subspace>))
      (subspace-2 (make <subspace>)))
  (let* ((passage (make <passage>))
	 (left-portal (make <portal> #:to passage))
	 (right-portal (make <portal> #:to passage)))
    (set! *subspaces* (list subspace-1 subspace-2 passage))
    (set! #[passage 'left-portal] left-portal)
    (add! left-portal subspace-1)
    (set! #[passage 'right-portal] right-portal)
    (add! right-portal subspace-2)))

(define-protocol-generator (kutasa-protocol socket connection)
  ((username #f)
   (player #f)
   (address connection)
   (objects '())) ; objects owned by the player

  (define (login name password)
    (if (and (not username) password 
	     (equal? (hash-ref *users* name) password))
	(begin
	  (set! username name)
	  (set! *logged-users* (cons name *logged-users*))
	  #t)
	#f))

  (define (request id message)
    (match message
      (((? symbol? fn) args ...)
       (let ((result (and-let* ((fn #[kutasa-protocol fn])
				((procedure? fn)))
		       (safely (apply fn args)))))
	 `(response ,id ,(if (unspecified? result)
			     'unspecified
			     result))))
      (else
       `(response ,id ,message))))

  (define (transaction id message)
    (match message
      (((? symbol? fn) args ...)
       (let ((result (and-let* ((fn #[kutasa-protocol fn])
				((procedure? fn)))
		       (safely (apply fn args)))))
	 (if (list? result)
	     (for-each (lambda(element number)
			 (remote socket connection 
				 `(transaction ,id ,number ,element)))
		       result
		       (iota (length result)))
	     #;else 
	     (if (not (unspecified? result))
		 (remote socket connection result)))
	 `(begin-transaction ,id ,(or (and (list? result) (length result))
				      (and (unspecified? result) 0)
				      1))))
      (else
       (<< `(invalid transaction request ,message for ,id)))))

  (define (join)
    (if username
	(begin
	  ;(display `(user ,username joins the game!))
	  (set! player (make <player> #:owners (list address)))
	  (push! objects player)
	  (spawn-object! player)
	  (protocol-add! kutasa-protocol
			 ((jump) (jump! player))
			 ((shoot) (shoot! player))
			 ((turn degs) (turn! player degs))
			 ((crouch) (crouch! player))
			 ((walk) (walk! player)))
	  `(add! <player> ,#[player 'id] ,@(network-state-of player)))
	'not-logged-in))

  (define (leave)
    (set! player #f)
    (protocol-remove! kutasa-protocol jump shoot turn crouch walk))

  (define (echo)
    '(echo))

  (define (owned-objects)
    objects)

  (define (display message)
    (display message))

  (define (type-of id)
    (and-let* ((object #[*object-registry* id])
	       (type (class-name (class-of object))))
      `(add! ,type ,id)))

  (define (state-of id)
    (and-let* ((object #[*object-registry* id]))
      (cons id (network-state-of object (in? object objects)))))
  
  (define (visible-subspaces)
    (apply union (map (lambda(object)
			(subspaces-visible-from #[object 'context]))
		      objects)))

  (define (visible-objects)
    (apply union (map objects-visible-to objects)))

  (define (visible-objects+their-states)
    (let ((visible-objects (#[kutasa-protocol 'visible-objects])))
      (map (lambda(object)
	     (append (list (class-name (class-of object)) #[object 'id])
		     (network-state-of object)))
	   visible-objects)))

  (define (inform-me-about-changes)
    (set! *observers* (adjoin *observers* connection)))
  
  (define (describe-protocol)
    (hash-map->list
     (lambda (name proc)
       (cons name
	     (procedure-args proc)))
     kutasa-protocol))

  (define (logout)
    (if username (set! username #f))
    (set! *logged-users*
	  (delete username *logged-users*)))
  #;(define-protocol-generator (kutasa-protocol connection)))

#;(for ($ <modification> 
	(= 'subject subject) 
	(= 'source (? (\ is-a? _1 <subspace>) source)) 
	(= 'destination destination))
     in *modifications*)


"trzeba teraz określić reguły, w oparciu o które informujemy
klientów o modyfikacjach. generalnie istnieja dwie mozliwosci: po pierwsze,
może być tak, że jakiś nowy potal pojawi się w polu widzenia,

1. kiedy wywołać subspaces-become-visible! ?
- kiedy w polu widzenia pojawi sie nowy portal.

2. kiedy wywołać subspaces-no-longer-visible! ?
- kiedy jakis portal zniknie z pola widzenia

3. kiedy wywołać new-object! ?
- jeżeli jakiś obiekt pojawi się w widzianym sektorze

4. kiedy wywołać remove-object! ?
- jeżeli jakiś obiekt zniknie z widzianego sektora

5. kiedy wywołać move-object! ?
- jeżeli jakiś obiekt zostanie przemieszczony z jednego
widzianego sektora do innego

A co jeżeli to posiadany przez nas obiekt zostaje przemieszczony?
OCH, ZGROZA! TYLKO NIE TO ;]
"

(define (respond sock addr proto)
  (and-let* (((in? addr *observers*))
	     (owned-objects (#[proto 'owned-objects]))
	     (visible-subspaces (apply union
				       (map subspaces-visible-to owned-objects)))
	     (visible-objects (unique (append-map #[_ 'objects]
						  visible-subspaces)))
	     (modified-objects (filter modified? visible-objects)))
    (let-values (((own-changes other-changes) (partition (\ in? _ owned-objects)
							 *modifications*)))
      (let* ((prior-subspaces (filter-map #[_ 'source] own-changes))
	     (new-subspaces (filter-map #[_ 'destination] own-changes))
	     (formerly-visible-subspaces
	      (difference (apply union 
				 (map subspaces-visible-from prior-subspaces))
			  visible-subspaces))
	     (newly-visible-subspaces
	      (difference (apply union
				 (map subspaces-visible-from new-subspaces))
			  visible-subspaces)))
	(if (not (null? formerly-visible-subspaces))
	    (remote sock addr 
		    `(subspaces-no-longer-visible! ,@formerly-visible-subspaces)))
	(if (not (null? newly-visible-subspaces))
	    (remote sock addr
		    `(subspaces-become-visible! ,@newly-visible-subspaces))))
      (for (subject source target) in (map slot-values other-changes)
	   (cond ((is-a? subject <portal>)
		  (<<"OMG! a portal moved! I never thought it's that possible!"))
		 (else
		  (let ((target-visible
			 (and target (in? target visible-subspaces)))
			(source-visible
			 (and source (in? source visible-subspaces))))
		    (cond
		     ((and source-visible target-visible)
		      (remote sock addr `(move-object! ,#[subject 'id] ,target)))
		     (source-visible
		      (remote sock addr `(remove-object! ,#[subject 'id])))
		     (target-visible
		      (remote sock addr `(add! ,(class-name (class-of subject))
				 ,#[subject 'id]
				 ,@(network-state-of subject)
				 )))
		     (else 
		      (<< "NOT sending modification of "subject" to "addr))))
		  ))))
    #;(for i in (iota (length *modifications*))
    (<<"MODIFICATION "i" " #[*modifications* i]))
    (for object in modified-objects
	 (let ((message 
		(with-output-to-utf8
		 (\ display 
		  `(set-slots! 
		    ,#[object 'id]
		    ,@(network-modified-state-of
		       object
		       (in? object owned-objects)))))))
	   (sendto sock message addr)
	   (begin
	     (display `(sending
			,(utf8->string message)))
	     (newline))
	   ))))

(let ((socket (socket PF_INET SOCK_DGRAM 0)))
  (bind socket AF_INET INADDR_ANY 41337)
  (let ((server-cycle 
	 (make-server-cycle 
	  socket
	  #[]
	  kutasa-protocol
	  update-world!
	  respond
	  (lambda()
	    (set! *modifications* '()))
	  1.0)))
    (while #t (server-cycle))))
