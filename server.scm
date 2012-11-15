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
 (extra math) (extra 3d)
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

(define-protocol-generator (kutasa-protocol connection)
  ((username #f)
   (player #f)
   (address connection))
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
	  (spawn-object! *world* player)
	  (protocol-add! kutasa-protocol
			 ((jump) (jump! player))
			 ((shoot) (shoot! player))
			 ((turn degs) (turn! player degs))
			 ((crouch) (crouch! player))
			 ((walk) (walk! player)))
	  #t)
	'not-logged-in))
   ((leave)
    (set! player #f)
    (protocol-remove! kutasa-protocol jump shoot turn crouch walk))
   ((prn)
    '(prn))
   ((display message)
    (display message))
   ((describe-protocol)
    (hash-map->list (lambda (name proc)
		      (cons name
			    (procedure-args proc)))
		    kutasa-protocol))
   ((logout)
    (if username (set! username #f))
    (set! *logged-users*
	  (remove (\ equal? _ username) *logged-users*)))))

(let ((socket (socket PF_INET SOCK_DGRAM 0)))
  (bind socket AF_INET INADDR_ANY 41337)
  (let ((server-cycle (make-server-cycle 
		       socket
		       (make-hash-table)
		       kutasa-protocol
		       (lambda()
			 (display "update!\n")
			 (for-each update! 
				   (hash-map->list
				    (lambda(id object)
				      object)
				    *world*)))
		       (lambda x x)
		       3
		       )))
  (while #t (server-cycle))))
