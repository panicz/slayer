(define-module (extra drawing primitives)
  #:use-module (slayer)
  #:use-module (slayer drawing)
  #:use-module (slayer image)
  #:use-module (grand scheme)
  #:use-module (extra math)
  #:use-module (extra attributes)
  #:use-module (extra drawing parameters)
  #:use-module (extra define-keywords)

  #:export (shape?
	    bitmap?
	    space?
	    space
	    caption?
	    caption
	    drawing?
	    group?
	    group
	    draw!
	    displaced
	    extents
	    dimensions
	    center
	    width
	    height
	    diameter
	    radius
	    drawing))

;; note that the operations are cleverly defined so that '(),
;; i.e. -- an empty group -- is a legitimate drawing

(define (shape? x)
  (and-let* ((`(shape . ,attributes) x))))

(define (bitmap? x)
  (and-let* ((`(bitmap . ,attributes) x))))

(define (space? x)
  (and-let* ((`(space . ,attributes) x))))

(define (group? x)
  (and-let* ((`(group . ,attributes) x))))

(define* (space #:at position #:= '(0 0)
		#:width w #:= 0
		#:height h #:= 0
		#:size size #:= `(,w ,h) . rest)
  `(space #:position ,position #:size ,size . ,rest))


(define (caption? x)
  (and-let* ((`(caption . ,attributes) x))))


(use-modules (extra drawing parameters) (grand scheme))

(define* (caption text #:at top-left #:= '(0 0)
		  #:position position #:= top-left
		  #:color color #:= (current-text-color)
		  #:size size #:= (current-font-size)
		  #:font font #:= (current-font)
		  #:background-color bg #:= (current-text-background-color)
		  #:slant slant #:= (current-font-slant)
		  #:weight weight #:= (current-font-weight)
		  . rest)
  `(caption #:position ,position #:text ,text #:color ,color #:size ,size
	    #:font ,font #:slant ,slant #:weight ,weight
	    #:background-color ,bg ,@rest))

(define (drawing? x)
  (or (shape? x)
      (bitmap? x)
      (space? x)
      (group? x)))

(define (group . attributes+drawings)
  `(group . ,attributes+drawings))
(define (set-color! c)
  ;; there's something messed up with the color order!
  (match c
    ((r g b)
     (set-source-rgb! r g b))
    ((r g b a)
     (set-source-rgba! r g b a))
    ))

(define (extents? object)
  (and-let* (((left top right bottom) object)
	     ((every real? object))
	     ((is left <= right))
	     ((is top <= bottom)))))

;;(assert (unique-type? extents?))

(define (extents drawing)
  (match drawing

    (`(shape . ,attributes)
     (measure-without-drawing (lambda () (draw! drawing))))

    (`(caption . ,attributes)
     (measure-without-drawing
			    (lambda () (draw! `(caption . ,attributes))))
     #;(let* ((extended (update-attributes attributes
					 #:text (string-append
						 "|" ((from attributes)
						      #:text))))
	    ((L T R B) (measure-without-drawing
			(lambda () (draw! `(caption . ,extended)))))
	    (extension (update-attributes attributes #:text "|"))
	    ((L- T- R- B-) )
	    (W- (- R- L-)))
       `(,(- L W-) ,T ,(- R W-) ,B)))

    (`(space . ,attributes)
     (let* ((attribute (from attributes))
	    ((x y) (attribute #:position))
	    ((w h) (attribute #:size)))
       `(,x ,y ,(+ x w) ,(+ y h))))

     (`(bitmap . ,attributes)
     (throw 'not-implemented/yet?))

     (`(group . ,attributes)
      (let ((attributes drawings (attributes+children attributes)))
	(fold-left (lambda ((left top right bottom) drawing*)
		     (let (((left* top* right* bottom*) (extents drawing*)))
		       `(,(min left left*) ,(min top top*)
			 ,(max right right*) ,(max bottom bottom*))))
		   '(+inf.0 +inf.0 -inf.0 -inf.0)
		   drawings)))))

(define (dimensions drawing)
  (let (((left top right bottom) (extents drawing)))
    `(,(- right left) ,(- bottom top))))

(define (width drawing)
  (let (((width _) (dimensions drawing)))
    width))

(define (height drawing)
  (let (((_ height) (dimensions drawing)))
    height))

(define (diameter drawing)
  (let ((`(,width ,height) (dimensions drawing)))
    (sqrt (+ (expt width 2) (expt height 2)))))

(define (radius drawing)
  (/ (diameter drawing) 2))

(define (center drawing)
  (let ((`(,left ,top ,right ,bottom) (extents drawing))
	(`(,width ,height) (dimensions drawing)))
    `(,(+ left (/ width 2)) ,(+ top (/ height 2)))))

(define (optionally perform-action #;on . arguments)
  (when (and (not (null? arguments))
	     (first arguments))
    (apply perform-action #;on arguments)))

(define (draw! drawing)
  (match drawing

    (`(shape . ,attributes)
     (let* ((attributes (firstborn . children)
			(attributes+children attributes))
	    (attribute (from attributes))
	    (draw-contour! (lambda (thickness)
			     (set-line-width! thickness)
			     (stroke!)))
	    (fill-contour! (lambda (color)
			     (set-color! color)
			     ;;(close-path!)
			     (fill!))))
       (apply move-to! firstborn)
       (for child in children
	 (match child
	   (`(,x ,y)
	    (line-to! x y))
	   
	   (`(arc #:radius ,radius
		  #:center (,x ,y)
		  #:from ,angle-1
		  #:to ,angle-2)
	    (let ((x0 y0 (real+imag (exp (* 0+i angle-1)))))
	      (arc! x y radius angle-1 angle-2)))
	   
	   (`((,x-in ,y-in) (,x ,y) (,x-out ,y-out))
	    (curve-to! x-in y-in x-out y-out x y))))
       (optionally set-color! (attribute #:contour-color))
       (optionally draw-contour! (attribute #:contour-thickness))
       (optionally fill-contour! (attribute #:fill-color))
       ))

    (`(caption . ,attributes)
     (let ((attribute (from attributes)))
       (optionally set-color! #;to (attribute #:color))
       (optionally set-font-size! #;to (attribute #:size))
       (optionally set-font-face! #;to (attribute #:font)
		   (attribute #:slant) (attribute #:weight))
       (apply move-to! (attribute #:position))
       (show-text! (attribute #:text))))

    (`(space . ,_)
     (noop))

    (`(bitmap . ,attributes)
     (throw 'not-implemented/yet?))

    (`(group . ,attributes)
     (let ((attributes drawings (attributes+children attributes)))
       (for-each draw! drawings)))))

;; displaced : drawing (real? real?) -> drawing
(define (displaced drawing #;by (dx dy))

  (define (update-position #;in attributes)
    (map-n 2 (lambda (key value)
	       (match key
		 (#:position
		  (let (((x y) value))
		    (values key `(,(+ x dx) ,(+ y dy)))))
		 (_
		  (values key value))))
	   attributes))
  (match drawing
    (`(shape . ,attributes)
     (let* ((attributes children (attributes+children attributes))
	    (children (map (lambda (child)
			     (match child
			       (`(,x ,y)
				`(,(+ x dx) ,(+ y dy)))
			       (`(arc #:radius ,radius
				      #:center (,x ,y)
				      #:from ,angle-1
				      #:to ,angle-2)
				`(arc #:radius ,radius
				      #:center (,(+ x dx) ,(+ y dy))
				      #:from ,angle-1
				      #:to ,angle-2))
			       (`((,x-in ,y-in) (,x ,y) (,x-out ,y-out))
				`((,(+ x-in dx) ,(+ y-in dy))
				  (,(+ x dx) ,(+ y dy))
				  (,(+ x-out dx) ,(+ y-out dy))))))
			   children)))
       `(shape ,@attributes ,@children)))

    (`(group . ,attributes)
     (let ((attributes drawings (attributes+children attributes)))
       `(group ,@attributes
	       ,@(map (lambda (drawing) 
			(displaced drawing #;by `(,dx ,dy)))
		      drawings))))
    
    ((type . attributes)
     `(,type . ,(update-position #;in attributes)))

    ))
