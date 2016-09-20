(define-module (extra attributes)
  #:use-module (ice-9 nice-9)
  #:export (from attributes? attributes+children children))

(define ((from attributes) attribute)
  (and-let* (((key value . rest) attributes))
    (if (eq? attribute key)
	value
	((from rest) attribute))))

(define (attributes? list)
  (or (null? list)
      (and-let* ((((? keyword?) value . rest) list))
	(attributes? rest))))

(define (attributes+children attribute-list)
  (match attribute-list
    (((? keyword? key) value . rest)
     (let* ((attributes children (attributes+children rest)))
       (values
	`(,key ,value . ,attributes)
	children)))
    (_
     (values '() attribute-list))))

(define (children attribute-list)
  (match attribute-list
    (((? keyword?) value . rest)
     (children rest))
    (_
     attribute-list)))
