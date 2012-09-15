(define-module (extra overload)
  #:use-module (oop goops)
  #:use-module (extra vector-lib)
  #:use-module (srfi srfi-1)
  #:export (length append map for-each))

(define-generic length)

(define-method (length (v <vector>))
  (vector-length v))

(define-method (length (s <string>))
  (string-length s))

(define-generic append)

(define-method (append (v <vector>) . rest)
  (apply vector-append v rest))

(define-method (append (s <string>) . rest)
  (apply string-append s rest))

(define-generic map)

(define-method (map f (v <vector>) . rest)
  (apply vector-map f v rest))

(define-method (map f (s <string>) . rest)
  (apply string-map f s rest))

(define-generic for-each)

(define-method (for-each f (v <vector>) . rest)
  (apply vector-for-each f v rest))

(define-method (for-each f (s <string>) . rest)
  (apply string-for-each f s rest))
