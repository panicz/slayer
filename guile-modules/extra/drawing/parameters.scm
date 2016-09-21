(define-module (extra drawing parameters)
  #:export (current-background-color
	    current-font-size
	    current-text-color
	    current-font
	    current-text-background-color))

(define current-background-color (make-parameter '(0 0 0)))
(define current-text-color (make-parameter '(1 1 1)))
(define current-text-background-color (make-parameter #f))
(define current-font-size (make-parameter 12))
(define current-font (make-parameter "cairo:monospace"))
