(define-module (butterfly parameters)
  #:use-module (butterfly categories)
  #:use-module (extra attributes)
  #:export (current current-editor-state))

(define current-editor-state (make-parameter (editor-state-category)))

(define (current attribute)
  ((from (current-editor-state)) attribute))
