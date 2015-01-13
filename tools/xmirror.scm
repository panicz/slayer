#!/usr/bin/guile \
-L ./guile-modules -L ../guile-modules -e main -s
!#
(use-modules (extra common)
	     (extra ref))

(define (main (_ input-file-names ...))
  (for input-file-name in input-file-names
    (let* (((path ... file) (string-split input-file-name #\/))
	   (base-name (string-remove-prefix "left-" file))
	   (output-file-name (string-join `(,@path ,(string-append 
						     "right-" base-name)) "/")))
      (with-output-file output-file-name
	(with-input-file input-file-name
	  (match (read)
	    (('mesh . details)
	     (pretty-print `(mesh . ,(map x-mirror details))))))))))

(define (x-mirror spec)
  (match spec
    (((and (or 'vertices 'normals) element-type) elements)
     (let ((type (array-type elements))
	   (elements (array->list elements)))
       `(,element-type ,(list->typed-array type 2 (map (lambda ((x y z)) 
							 `(,(- x) ,y ,z))
						       elements)))))
    (else
     spec)))
