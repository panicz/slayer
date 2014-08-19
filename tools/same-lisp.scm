#!/usr/bin/guile \
-L ./guile-modules -L ../guile-modules -e main -s
!#
(use-modules (extra common))

(define (main (program-name files ...))
  (catch 'read-error
    (lambda ()
      (let ((contents (map read-file files)))
	(if (apply equal? contents)
	    (exit 0)
	    (exit -1))))
    (lambda _
      (exit 77))))
