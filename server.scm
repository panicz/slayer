#!/usr/bin/guile -s
!#
(set! %load-path (append (list "." "..")  %load-path))

(use-modules 
 (oop goops)
 (srfi srfi-1) (srfi srfi-2) (srfi srfi-11) 
 (ice-9 match) (ice-9 format) (ice-9 optargs) 
 (ice-9 pretty-print) (ice-9 local-eval) (ice-9 regex)
 (ice-9 session)
 (system base compile) (system syntax)
 (extra ref) (extra common) (extra network) (extra function)
 ((rnrs) :version (6)))

(define *world* (make-hash-table))
(define *users* (make-hash-table))
(define *logged-users* '())

;; server is stupid and reliable; client is fancy and fallable
(load "game.scm")

(for-each
 (match-lambda((username password)
	       (hash-set! *users* username password)))
 '((panicz k0byl4)
   (drcz 12345)
   (polak żółć)))

(define current-step 0)
(define (step!)
  (set! current-step (bitwise-and #xffff (+ current-step 1))))

(define-protocol-generator (kutasa-protocol connection)
  ((username #f)
   (player #f)
   (address connection)
   (objects '()))
  (((login name password)
    (if (and (not username) password 
	     (equal? (hash-ref *users* name) password))
	(begin
	  (set! username name)
	  (set! *logged-users* (cons name *logged-users*))
	  #t)
	#f))
   ((join)
    (if username
	(begin
	  (set! player (make <player> #:owners `(,address)))
	  (set! objects (cons player objects))
	  (spawn-object! *world* player)
	  (protocol-add! kutasa-protocol
			 ((jump) (jump! player))
			 ((shoot) (shoot! player))
			 ((turn degs) (turn! player degs))
			 ((crouch) (crouch! player))
			 ((walk) (walk! player)))
	  `(add! <player> ,#[player 'id] ,@(state-of player)))
	'not-logged-in))
   ((leave)
    (set! player #f)
    (protocol-remove! kutasa-protocol jump shoot turn crouch walk))
   ((echo)
    '(echo))
   ((owned-objects)
    objects)
   ((display message)
    (display message))
   ((describe-protocol)
    (hash-map->list
     (lambda (name proc)
       (cons name
	     (procedure-args proc)))
     kutasa-protocol))
   ((logout)
    (if username (set! username #f))
    (set! *logged-users*
	  (remove (\ equal? _ username) *logged-users*)))))

(let ((socket (socket PF_INET SOCK_DGRAM 0)))
  (bind socket AF_INET INADDR_ANY 41337)
  (let ((server-cycle 
	 (make-server-cycle 
	  socket
	  (make-hash-table)
	  kutasa-protocol
	  (lambda()
	    (for-each update!
		      (hash-map->list
		       (lambda(id object)
			 ;(display object)
			 object)
		       *world*))
	    (newline))
	  (lambda (sock addr proto)
	    (for object in (#[proto 'owned-objects])
		 (for object in (objects-visible-to object)
		      (let ((message 
			     (with-output-to-utf8
			      (\ display `(set-slots! 
					   ,#[object 'id] 
					   ,@(state-of object))))))
			(sendto sock message addr)))))
	  0.3)))
    (while #t (server-cycle))))
