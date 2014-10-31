;#| -*-Scheme-*-
;
;$Id: copyright.scm,v 1.4 2005/12/13 06:41:00 cph Exp $
;
;Copyright 2005 Massachusetts Institute of Technology
;
;This file is part of MIT/GNU Scheme.
;
;MIT/GNU Scheme is free software; you can redistribute it and/or modify
;it under the terms of the GNU General Public License as published by
;the Free Software Foundation; either version 2 of the License, or (at
;your option) any later version.
;
;MIT/GNU Scheme is distributed in the hope that it will be useful, but
;WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with MIT/GNU Scheme; if not, write to the Free Software
;Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
;USA.
;
;|#

(declare (usual-integrations))

(define (make-line-prefix-port port prefix)
  (make-port line-prefix-port-type
	     (vector port prefix #t)))

(define line-prefix-port-type
  (make-port-type
   `((write-char
      ,(lambda (port char)
	 (let ((v (port/state port)))
	   (let ((port (vector-ref v 0))
		 (prefix (vector-ref v 1)))
	     (if (vector-ref v 2)
		 (write-string prefix port))
	     (write-char char port)
	     (vector-set! v 2 (char=? char #\newline)))))))
   #f))


(define ((pp-line-prefix prefix) object #!optional port . rest)
  (let ((port
	 (make-line-prefix-port
	  (if (default-object? port)
	      (current-output-port)
	      port)
	  prefix)))
    (apply pp (cons object (cons port rest)))))


(define pp-comment (pp-line-prefix ";"))

(set! repl:write-result-hash-numbers? #f)

