(define-module (extra scmutils)
  #:use-module (extra common)
  #:use-module (extra math)
  #:export (svd svd-invert array->matrix matrix->array))

(load "scmutils/load.scm")

(set-current-module generic-environment)
