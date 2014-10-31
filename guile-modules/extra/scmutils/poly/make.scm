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

;;; Polynomial stuff

(error "This file (poly/make.scm) shouldn't be loaded.")

(if (lexical-unreferenceable? user-initial-environment
			      'scmutils-base-environment)
    (with-working-directory-pathname
     "../kernel/"
     (lambda ()
       (load "make" user-initial-environment))))

(in-package user-initial-environment

  (define (polysys:make subsystem-name)

    (case subsystem-name
      ((root-finder poly->roots poly:->roots) 
       (if (lexical-unreferenceable? scmutils-base-environment 'root-finder-package)
	   (load "make-rootfinder" scmutils-base-environment)))
      (else
       (error "I don't know about" subsystem-name)))))

