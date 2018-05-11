(define-module (extra drawing operations)
  #:use-module (extra common)
  #:use-module (ice-9 nice-9)
  #:use-module (extra attributes)
  #:use-module (extra drawing primitives)
  #:export (overlay beside below above box right-up down-left))

(define* (overlay foreground #;on background #:optional (position '(0 0)))
  (group background (displaced foreground #;by position)))

(define (box image . attributes)
  (let* ((default (default-attribute attributes))
	 (margin (default #:margin 0))
	 (margin-left (default #:margin-left margin))
	 (margin-right (default #:margin-right margin))
	 (margin-top (default #:margin-top margin))
	 (margin-bottom (default #:margin-bottom margin))
	 ((left top right bottom) (extents image))
	 (L (- left margin-left))
	 (R (+ right margin-right))
	 (T (- top margin-top))
	 (B (+ bottom margin-bottom)))
    (displaced
     (overlay image
	      `(shape ,@attributes
		      (,L ,T) (,R ,T) (,R ,B) (,L ,B) (,L ,T)))
     `(,margin-left ,margin-top))))

(define (beside drawing . drawings)
  (fold-left (lambda (previous next)
	       (overlay next previous
			#;at `(,(width previous) 0)))
	     drawing
	     drawings))

(define (below drawing . drawings)
  (fold-left (lambda (previous next)
	       (overlay next #;on previous #;at `(0 ,(height previous))))
	     drawing
	     drawings))

(define (above . drawings)
  (apply below (reverse drawings)))

(define (right-up drawing . drawings)
  (fold-left (lambda (previous next)
	       (let (((x y w h) (extents next)))
		 (overlay next previous
			  `(,(- (width previous) x) ,(- y)))))
	     drawing
	     drawings))

(define down-left below)

(define (superscript drawing)
  ...)

(define (subscript drawing)
  ...)

  
