(define-module (butterfly render)
  #:use-module (grand scheme)
  #:use-module (extra drawing)
  #:use-module (butterfly cursor)
  #:use-module (butterfly selection)
  #:use-module (butterfly parameters)
  #:export (render edited))

(define (indentation)
  (space #:width 10))

(define (_) (space #:width 5))

(define (subcursors cursor list . skip)
  (map (lambda (n)
	 `(,@cursor ,n))
       (apply iota (length list) skip)))

(define+ (render (first inner ... last) #;at cursor)
  (let ((inner-subcursors (subcursors cursor inner 1))
	(last-subcursor `(,@cursor ,(+ (length inner) 1))))
    (beside
     (caption "(")
     (render first `(,@cursor 0))
     (apply beside (map render inner inner-subcursors))
     (_)
     (render last last-subcursor)
     (caption ")"))))

(define+ (render (only) #;at cursor)
  (beside (caption "(") (render only `(,@cursor 0)) (caption ")")))

(define+ (render s-exp #;at cursor)
  (apply caption (->string s-exp)
	 (if (equal? cursor (current #:cursor))
	     '(#:color (1 0 0))
	     '())))

(define+ (render ('+ . args) #;at cursor)
  (let ((args-subcursors (subcursors cursor args 1)))
    (apply beside
	   (intersperse (caption " +  ")
			(map render args args-subcursors)))))

(define+ (render ('let (bindings ...) body ...) #;at cursor)
  (let ((binding-subcursors (subcursors `(,@cursor 1) bindings))
	(body-subcursors (subcursors cursor body 2)))
    (below
     (beside
      (render 'let `(,@cursor 0))
      (indentation)
      (apply
       below
       (map (lambda ((variables ... value) cursor)
	      (let ((var-subcursors (subcursors cursor variables))
		    (value-cursor `(,@cursor ,(length variables))))
		(beside (apply beside
			       (map render variables var-subcursors))
			(caption " =  ")
			(render value value-cursor))))
	    bindings binding-subcursors)))
     (beside
      (indentation)
      (apply below (map render body body-subcursors))))))

(define+ (render ('expt x y) #;at cursor)
  (beside
   (render x `(,@cursor 1))
   (superscript (render y `(,@cursor 2)))))

(define+ (render ('quote x) #;at cursor)
  (parameterize ((current-font "Courier New")
		 (current-font-size 14)
		 (current-font-slant 'normal)
		 (current-font-weight 'bold))
    (beside (caption "'") (render x `(,@cursor 1)))))

(define render-quasiquote-level (make-parameter '()))

(define+ (render ('quasiquote x) #;at cursor)
  (parameterize ((render-quasiquote-level
		  `((,(current-font)
		     ,(current-font-size)
		     ,(current-font-weight)
		     ,(current-font-slant))
		    . ,(render-quasiquote-level)))
		 (current-font "Courier New")
		 (current-font-size 14)
		 (current-font-slant 'normal)
		 (current-font-weight 'bold))
    (beside (caption "`") (render x `(,@cursor ,1)))))

(define+ (render ('unquote x) #;at cursor)
  (beside
   (caption ",")
   (match (render-quasiquote-level)
     (()
      (render x `(,@cursor 1)))
     (((font size weight slant) . lower-level)
      (parameterize ((current-font font)
		     (current-font-size size)
		     (current-font-weight weight)
		     (current-font-slant slant)
		     (render-quasiquote-level lower-level))
	(render x `(,@cursor 1)))))))

