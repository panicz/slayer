(define-module (red object)
  #:use-module (oop goops)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra 3d)
  #:use-module (widgets 3d)
  #:export (<editable-object> )
  #:re-export ( add-object! )
  )

(define-class <editable-object> (<3d-object>)
  (quaternion 
   #:allocation #:virtual
   #:slot-ref
   (lambda(self)
     #[self 'orientation])
   #:slot-set!
   (lambda(self value)
     (set! #[self 'orientation] value)))
  (mesh-cache #:allocation #:each-subclass #:init-thunk make-hash-table)
  (rig #:init-value #f))

(define-method (add-object! (object <editable-object>) #;to (stage <3d-stage>))
  (next-method)
  (set! #[object 'rig] stage))
