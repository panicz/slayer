(define-module (butterfly render)
  #:use-module (grand scheme)
  #:use-module (extra drawing)
  #:use-module (extra attributes)
  #:use-module (extra math)
  #:use-module (butterfly cursor)
  #:use-module (butterfly selection)
  #:use-module (butterfly parameters)
  #:export (render+ render-s-expression with-cursor parenthesized))

(define unique-object make-symbol)

(define cursor-tag (unique-object "cursor"))

(define (cursor-placeholder? x)
  (eq? x cursor-tag))

(define embracement-tag (unique-object "embracement"))

(define (embracement? x)
  (eq? x embracement-tag))

(define (embraced? x)
  (and-let* (((tag expression) x))
    (embracement? tag)))

(define (embrace expression)
  `(,embracement-tag ,expression))

(define dot-tag (unique-object "dot"))

(define breadth (space #:width 20 #:height 5))

(define (dot? x)
  (eq? x dot-tag))

(define (with-cursor cursor expression)
  (cond ((cursor-points-to-location? cursor)
	 (splice-subexpression `(,cursor-tag)
			       #;into expression #;at cursor))
	((cursor-points-to-expression? cursor)
	 (replace-subexpression
	  #;of expression #;at cursor
	       #;with (embrace (focus expression #;on cursor))))
	(else
	 (throw 'invalid-cursor cursor expression))))

(define (indentation)
  (space #:width 10))

(define (subcursors cursor list . skip)
  (map (lambda (n)
	 (and cursor `(,@cursor ,n)))
       (apply iota (length list) skip)))


(define (render-cursor)
  (let ((c (caption "|")))
    (if (even? (current-time))
	c
	c
	#;(space #:width (width c) #:height (height c)))))

(define (binding-keyword? x)
  (and (symbol? x)
       (symbol-match "^(with|let)" x)))

(define+ (render+ expression)
  (render-s-expression expression))

(define (optional property value)
  (if value
      `(,property ,value)
      '()))

(define* (circle #:radius radius #:= 1
		 #:center center #:= '(0 0)
		 #:contour-color color #:= #f
		 #:contour-thickness thickness #:= #f
		 #:fill-color fill-color #:= #f)
  (let ((`(,x ,y) center))
    `(shape
      ,@(optional #:contour-color color)
      ,@(optional #:contour-thickness thickness)
      ,@(optional #:fill-color fill-color)
      (,(+ x radius) ,y)
      (arc #:radius ,radius #:center ,center
	   #:from 0 #:to ,(* 2 pi)))))

(define* (node name #:at position #:= '(0 0))
  ;;(assert (string? name))
  (let ((label (caption name #:at position)))
    (overlay label
	     (circle #:radius (diameter label)
		     #:contour-thickness 2
		     #:contour-color '(1 1 1)
		     #:center (center label)))))

(define (random-point n)
  `(,(random n) ,(random n)))


(define* (point-on-circle #:radius r #:angle a #:center c #:= '(0 0))
  (let* ((o (apply make-rectangular c))
	 (d (* r (exp (* 0+i a))))
	 (x* y* (real+imag (+ o d))))
    `(,x* ,y*)))

(define* (evenly-distributed-points-on-circle N #:radius r #:= 1
					      #:center c #:= '(0 0))
  (let ((slice (/ 2pi N)))
     (map (lambda (k)
	    (point-on-circle #:radius r #:center c #:angle (+ (* 3/2 pi)
							      (* k slice))))
	  (iota N))))


(define (find-index satisfying? list)
  (define (find-from n list)
    (and-let* ((`(,head . ,tail) list))
      (if (satisfying? head)
	  n
	  (find-from (+ n 1) tail))))
  (find-from 0 list))

(define (unit c)
  (/ c (magnitude c)))

(define* (arrow #:from start #:to end)
  (let* ((starting-position (list->complex (center start)))
	 (ending-position (list->complex (center end)))
	 (direction (unit (- ending-position starting-position))))
    `(shape ,(complex->list (+ starting-position (* direction (radius start))))
	    ,(complex->list (- ending-position (* direction (radius end)))))))

(define+ (render+ `(simple-graph . ,graph))
  (let* ((nodes (map (lambda (`(,name . ,neighbors) p)
		       (node (->string name) #:at p))
		     graph
		     (evenly-distributed-points-on-circle (length graph)
							  #:center `(100 100)
							  #:radius 50)))
	 (arrows (append-map
		  (lambda (`(,name . ,neighbors) node)
		    (map (lambda (neighbor)
			   (arrow #:from node
				  #:to (list-ref
					nodes
					(find-index (lambda (`(,name . ,_))
						      (equal? neighbor
							      name))
						    graph))))
			 neighbors))
		  graph nodes)))
    (overlay `(group . ,arrows) `(group . ,nodes))))


#|




(define+ (render+ expression)

  (cond ((cursor-placeholder? expression)
	 (caption "cursor")#;(render-cursor))

	((embraced? expression)
	 (let (((tag original) expression))
	   (box (render+ original)
		#:contour-thickness 2.0
		#:contour-color '(1.0 1.0 1.0)
		#:margin 10)))
	
	((not (pair? expression))
	 (caption (->string expression)))
	
	(else
	 (let* ((text (with-output-to-string
			(lambda ()
			  (pretty-print expression))))
		(lines (string-split text #\newline)))
	   (apply below (map caption lines))))))
|#
	 
(define quotation-abbreviations
  '((quote "'")
    (quasiquote "`")
    (unquote ",")
    (unquote-splicing ",@")))

(define (abbreviation symbol)
  (and (symbol? symbol)
       (any (lambda ((quotation abbreviation))
	      (and (eq? symbol quotation)
		   abbreviation))
	    quotation-abbreviations)))

(define (rendered-operator+proper-operands expression)

  (define (proper-operands operands)
    (let ((operands tail (proper-list+dotted-tail operands)))
      (match tail
	(()
	 operands)
	(_
	 `(,@operands ,dot-tag ,tail)))))

  (match expression
    (((? cursor-placeholder?) operator . operands)
     (values (right-up (render-cursor) breadth
		       (render-operator+ operator))
	     (proper-operands operands)))

    ((operator (? cursor-placeholder?) . operands)
     (values (right-up (render-operator+ operator)
		       breadth
		       (render-cursor))
	     (proper-operands operands)))

    (((? cursor-placeholder?) . operands)
     (values (render-cursor)
	     breadth
	     (proper-operands operands)))

    ((operator . operands)
     (values (render-operator+ operator)
	     (proper-operands operands)))))

(define+ (render-operator+ operator)
  (render-s-expression operator))

(define+ (render-atom+ atom)
  (cond ((embracement? atom)
	 (caption "@"))

	((cursor-placeholder? atom)
	 (render-cursor))

	((dot? atom)
	 (caption "."))

	(else
	 (caption (->string atom)))))

(define (render-quoted expression)
  (render-s-expression expression))

(define (parentheses height)
  ;; trzeba by odpowiednio powiekszyc czcionke!
  (let ((size (* 1.3 height)))
    (values (caption "(" #:size size) (caption ")" #:size size))))

(define (parenthesized drawing)
  (let* ((height (height drawing))
	 (open close (parentheses height)))
    (right-up open drawing close)))

;; teraz taka kwestia: chcielibyśmy mieć ruchome parametry

(define ((compose f g) x)
  (f (g x)))

(define+ (layout+ operator operands)
  (let ((operator-width (width operator))
	(operand-widths (map width operands)))
    (parenthesized
     (cond ((and (is operator caption?)
		 (is (sum operand-widths) < (available-width)))
	    (apply right-up operator breadth
		   (intersperse breadth operands)))

	   ((is (apply max 0 operand-widths) < (available-width))
	    (right-up operator breadth
		      (apply down-left operands)))

	   (else
	    (apply down-left operator operands))))))

(define (render-s-expression expression)
  (match expression
    (((? abbreviation quotation) literal)
     (beside (caption (abbreviation quotation))
	     (render-quoted literal)))

    (((? embracement?) embraced)
     (let ((rendered (render-s-expression embraced)))
       (box rendered
	    #:contour-thickness 2.0
	    #:contour-color '(1.0 1.0 1.0)
	    #:margin 10)))
    
    ((_ . _)
     (let* ((operator operands (rendered-operator+proper-operands
				expression)))
       (parameterize ((available-width (- (available-width)
					  (width operator))))
	 (layout+ operator
		  (map render-s-expression operands)))))

    (_
     (render-atom+ expression))))
