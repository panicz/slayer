(define-module (extra common)
  #:use-module (srfi srfi-1)
  #:export (in?))

;(use-modules (srfi srfi-1))

(define (in? obj list)
  (any (lambda(x)(equal? x obj)) list))

