(define-module (extra oop)
  #:use-module (oop goops)
  #:export (
	    class-tree-append-map
	    class-names+classes
	    ))

(define (class-tree-append-map f root)
  "class-tree-append-map returns a flat list containing the result of application of f to the class object in the root, appended with the list of all its descendants, appended with the list of all their descenants' descentants and so on. f should always return a list."
  (apply append (f root) (map (lambda(class)(class-tree-append-map f class)) (class-direct-subclasses root))))

(define (class-names+classes root)
  (class-tree-append-map
   (lambda(class)
     (if (slot-bound? class 'name)
	 `((,(class-name class) ,class))
	 '()))
   root))
