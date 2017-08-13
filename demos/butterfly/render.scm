(define-module (butterfly render)
  #:use-module (grand scheme)
  #:use-module (extra drawing)
  #:use-module (extra attributes)
  #:use-module (butterfly cursor)
  #:use-module (butterfly selection)
  #:use-module (butterfly parameters)
  #:export (render uglify))

(define (indentation)
  (space #:width 10))

(define (_) (space #:width 5))

(define (subcursors cursor list . skip)
  (map (lambda (n)
	 (and cursor `(,@cursor ,n)))
       (apply iota (length list) skip)))

(define+ (uglify (first inner ... last) #;at cursor)
  (let ((inner-subcursors (subcursors cursor inner 1))
	(last-subcursor `(,@cursor ,(+ (length inner) 1))))
    (beside
     (caption "(")
     (uglify first `(,@cursor 0))
     (apply beside (map uglify inner inner-subcursors))
     (_)
     (uglify last last-subcursor)
     (caption ")"))))

(define+ (uglify (only) #;at cursor)
  (beside (caption "(") (uglify only `(,@cursor 0)) (caption ")")))

(define+ (uglify s-exp #;at cursor)
  (apply caption (->string s-exp)
	 (if (equal? cursor (current #:cursor))
	     '(#:color (1 0 0))
	     '())))

(define+ (uglify ('+ . args) #;at cursor)
  (let ((args-subcursors (subcursors cursor args 1)))
    (apply beside
	   (intersperse (caption " +  ")
			(map uglify args args-subcursors)))))

(define+ (uglify ('let (bindings ...) body ...) #;at cursor)
  (let ((binding-subcursors (subcursors `(,@cursor 1) bindings))
	(body-subcursors (subcursors cursor body 2)))
    (below
     (beside
      (uglify 'let `(,@cursor 0))
      (indentation)
      (apply
       below
       (map (lambda ((variables ... value) cursor)
	      (let ((var-subcursors (subcursors cursor variables))
		    (value-cursor `(,@cursor ,(length variables))))
		(beside (apply beside
			       (map uglify variables var-subcursors))
			(caption " =  ")
			(uglify value value-cursor))))
	    bindings binding-subcursors)))
     (beside
      (indentation)
      (apply below (map uglify body body-subcursors))))))

(define+ (uglify ('expt x y) #;at cursor)
  (beside
   (uglify x `(,@cursor 1))
   (superscript (uglify y `(,@cursor 2)))))

(define+ (uglify ('quote x) #;at cursor)
  (parameterize ((current-font "Courier New")
		 (current-font-size 14)
		 (current-font-slant 'normal)
		 (current-font-weight 'bold))
    (beside (caption "'") (uglify x `(,@cursor 1)))))

(define render-quasiquote-level (make-parameter '()))

(define+ (uglify ('quasiquote x) #;at cursor)
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
    (beside (caption "`") (uglify x `(,@cursor ,1)))))

(define+ (uglify ('unquote x) #;at cursor)
  (beside
   (caption ",")
   (match (render-quasiquote-level)
     (()
      (uglify x `(,@cursor 1)))
     (((font size weight slant) . lower-level)
      (parameterize ((current-font font)
		     (current-font-size size)
		     (current-font-weight weight)
		     (current-font-slant slant)
		     (render-quasiquote-level lower-level))
	(uglify x `(,@cursor 1)))))))

(define (render-cursor)
  (let ((c (caption "|")))
    (if (even? (current-time))
	c
	(space #:width (width c) #:height (height c)))))

(define (binding-keyword? x)
  (and (symbol? x)
       (symbol-match "^(with|let)" x)))

(define (render-edited-expression)
  (caption "<<edited-expression>>"))

;; "make-symbol" should actually be called "make-uninterned-symbol"
;; or "make-unique-symbol"

(define (render expression #;at position)
  (let ((cursor (current #:cursor))
	(document (current #:document)))
    (cond ((eq? expression cursor-tag)
	   (render-cursor))

	  ((embraced? expression)
	   (beside
	    (caption "[")
	    (render (unbrace expression) position)
	    (caption "]")))
     
	  ((not (pair? expression))
	   (caption (->string expression) #:cursor position))
	  
	  (else
	   (layout expression position)))))

(define (layout expression #;at position)
  (match expression
    (((? binding-keyword? keyword) (bindings ...) body ...)
     (below
      (beside
       (caption "(" #:cursor position)
       (render keyword `(,@position 0))
       (_)
       (caption "(")
       (if (embraced? bindings)
	   `(,(caption "{")
	     ,@(map render (unbrace bindings)
		    (subcursors `(,@position 1) (unbrace bindings)))
	     ,(caption "}"))
	   (map render bindings
	     (subcursors `(,@position 1) bindings)))
       (caption ")"))
      (beside
       (indentation)
       (apply
	below
	(map render body (subcursors position body 2))))
      (caption ")" #:cursor position)))
    (_
     (apply
      beside
      `(,(caption "(" #:cursor position)
	#;,@(map render expression
	       (subcursors position expression))
	,@(intersperse (_)
		       (map render expression
			    (subcursors position expression)))
	,(caption ")" #:cursor position))))))
