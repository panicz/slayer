#!/usr/bin/guile \
-e main -s
!#
(use-modules (extra common)
	     (extra ref))

(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((datum (read)) 
		 (content '()))
	(if (eof-object? datum)
	    (reverse content)
	    (loop (read) (cons datum content)))))))

(define (main (program-name files ...))
  (catch 'read-error
    (lambda ()
      (let ((contents (map read-file files)))
	(if (apply equal? contents)
	    (exit 0)
	    (exit -1))))
    (lambda _
      (exit 77))))
