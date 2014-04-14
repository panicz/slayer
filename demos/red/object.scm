(define-module (red object)
  #:use-module (oop goops)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra 3d)
  #:export (<editable-object>))

(define-class <editable-object> (<3d-object>)
  (quaternion 
   #:allocation #:virtual
   #:slot-ref
   (lambda(self)
     #[self 'orientation])
   #:slot-set!
   (lambda(self value)
     (set! #[self 'orientation] value)))
  (mesh-cache #:allocation #:each-subclass #:init-thunk make-hash-table))
