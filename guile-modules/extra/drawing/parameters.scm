(define-module (extra drawing parameters)
  #:export (current-background-color
	    current-font-size
	    current-text-color
	    current-font
	    current-text-background-color
	    current-font-slant
	    current-font-weight
	    ))

(define current-background-color (make-parameter '(0 0 0)))
(define current-text-color (make-parameter '(1 1 1)))
(define current-text-background-color (make-parameter #f))
(define current-font-size (make-parameter 16))
(define current-font (make-parameter "Times New Roman"))
(define current-font-slant (make-parameter 'italic))
(define current-font-weight (make-parameter 'normal))
