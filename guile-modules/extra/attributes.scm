(define-module (extra attributes)
  #:use-module (extra common)
  #:use-module (ice-9 nice-9)
  #:export (from
	    default-attribute
	    attributes?
	    attributes+children
	    children
	    remove-attributes
	    merge-attributes
	    update-attributes))

(define ((from attributes) attribute)
  (and-let* (((key value . rest) attributes))
    (if (eq? attribute key)
	value
	((from rest) attribute))))

(define ((default-attribute attributes) attribute default)
  (match attributes
    ((key value . rest)
     (if (eq? key attribute)
	 value
	 ((default-attribute rest) attribute default)))
    (_
     default)))
     

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

(define (remove-attributes attributes #;from attribute-list)
  (match attribute-list
    (((? keyword? key)  value . rest)
     (if (member key attributes)
	 (remove-attributes attributes rest)
	 `(,key ,value . ,(remove-attributes attributes #;from rest))))
    (_
     attribute-list)))

(define (merge-attributes original #;with delta)
  (fold-left (lambda (original (attribute new-value))
	       (let* ((front rest (break (lambda (x) (eq? x attribute))
					 original)))
		 (match rest
		   ((attribute old-value . rest)
		    `(,@front ,attribute ,new-value ,@rest))
		   (_
		    `(,attribute ,new-value ,@original)))))
	     original
	     (chunks delta 2)))

(e.g.
 (merge-attributes '(#:a 1 #:b 2 #:c 3) #;with '(#:b 5 #:d 4))
 ===> (#:d 4 #:a 1 #:b 5 #:c 3))

(define (update-attributes original . attributes)
  (merge-attributes original attributes))

(e.g.
 (update-attributes '(#:a 1 #:b 2 #:c 3) #:d 4 #:b 5)
 ===> (#:d 4 #:a 1 #:b 5 #:c 3))
