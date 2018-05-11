(define-module (butterfly parameters)
  #:use-module (butterfly categories)
  #:use-module (grand scheme)
  #:use-module (extra attributes)
  #:use-module (slayer)
  #:export (current current-editor-state available-width available-height))


(define (screen-size) '(320 200))

(define available-height
  (make-parameter
   (let (((width height) (screen-size)))
     width)))

(define available-width
  (make-parameter
   (let (((width height) (screen-size)))
     height)))

(define window-size
  (make-parameter
   (screen-size)
   (lambda (size)
     (let (((w h) size))
       (available-width w)
       (available-height h)
       size))))

(define current-editor-state (make-parameter (editor-state-category)))

(define (current attribute)
  ((from (current-editor-state)) attribute))
