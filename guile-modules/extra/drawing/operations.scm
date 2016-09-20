(define-module (extra drawing operations)
  #:use-module (extra common)
  #:use-module (ice-9 nice-9)
  #:use-module (extra drawing primitives)
  #:export (overlay beside below above))

(define* (overlay background #;with foreground #:optional (position '(0 0)))
  (group background (displaced foreground #;by position)))

(define (beside . drawings)
  (fold-left (lambda (previous next)
	       (overlay previous #;with next #;at `(,(width previous) 0)))
	     '()
	     drawings))

(define (below . drawings)
  (fold-left (lambda (previous next)
	       (overlay previous #;with next #;at `(0 ,(height previous))))
	     '()
	     drawings))

(define (above . drawings)
  (apply below (reverse drawings)))

(define (superscript drawing)
  ...)

(define (subscript drawing)
  ...)

  
