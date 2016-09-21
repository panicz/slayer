(define-module (extra drawing primitives)
  #:use-module (slayer)
  #:use-module (slayer drawing)
  #:use-module (slayer image)
  #:use-module (extra common)
  #:use-module (extra attributes)
  #:use-module (extra drawing parameters)
  #:use-module (extra define-keywords)
  #:use-module (ice-9 nice-9)
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
	    width
	    height))

;; note that the operations are cleverly defined so that '(),
;; i.e. -- an empty group -- is a legitimate drawing

(define (shape? x)
  (and-let* ((('shape . attributes) x))))

(define (bitmap? x)
  (and-let* ((('bitmap . attributes) x))))

(define (space? x)
  (and-let* ((('space . attributes) x))))

(define* (space #:at position #:= '(0 0)
		#:width w #:= 0
		#:height h #:= 0
		#:size size #:= `(,w ,h) . rest)
  `(space #:position ,position #:size ,size . ,rest))

(define (caption? x)
  (and-let* ((('caption . attributes) x))))

(define* (caption text #:at position #:= '(0 0)
		  #:color color #:= (current-text-color)
		  #:size size #:= (current-font-size)
		  #:font font #:= (current-font)
		  #:background-color bg #:= (current-text-background-color)
		  . rest)
  `(caption #:position ,position #:text ,text #:color ,color #:size ,size
	    #:font ,font #:background-color ,bg . ,rest))

(define (drawing? x)
  (or (shape? x)
      (bitmap? x)
      (space? x)
      (group? x)))

(define (group? x)
  (and (list? x)
       (every drawing? x)))

(define (group . drawings)
  (append-map (lambda (drawing)
		(match drawing
		  (((children ...) ...)
		   children)
		  (_
		   `(,drawing))))
	      drawings))

(define (set-color! c)
  ;; there's something messed up with the color order!
  (match c
    ((r g b)
     (set-source-rgb! b g r))
    ((r g b a)
     (set-source-rgba! b g r a))
    ))

(define (extents? object)
  (and-let* (((left top right bottom) object)
	     ((every real? object))
	     ((<= left right))
	     ((<= top bottom)))))

;;(assert (unique-type? extents?))

(define (extents drawing)
  (match drawing

    (('shape . attributes)
     (measure-without-drawing (lambda () (draw! drawing))))

    (('caption . attributes) 
     (measure-without-drawing (lambda () (draw! drawing))))

    (('space . attributes)
     (let* ((attribute (from attributes))
	    ((x y) (attribute #:position))
	    ((w h) (attribute #:size)))
       `(,x ,y ,(+ x w) ,(+ y h))))

     (('bitmap . attributes)
     (throw 'not-implemented/yet?))

     (((group ...) ...)
     (fold-left (lambda ((left top right bottom) drawing*)
		  (let (((left* top* right* bottom*) (extents drawing*)))
		    `(,(min left left*) ,(min top top*)
		      ,(max right right*) ,(max bottom bottom*))))
		'(0 0 0 0)
		group))))

(define (dimensions drawing)
  (let (((left top right bottom) (extents drawing)))
    `(,(- right left) ,(- bottom top))))

(define (width drawing)
  (let (((width _) (dimensions drawing)))
    width))

(define (height drawing)
  (let (((_ height) (dimensions drawing)))
    height))

(define (optionally perform-action #;on argument)
  (when argument
    (perform-action #;on argument)))

(define (draw! drawing)
  (match drawing

    (('shape . attributes)
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
	   ((x y)
	    (line-to! x y))
	   (((x-in y-in) (x y) (x-out y-out))
	    (curve-to! x-in y-in x-out y-out x y))))
       (optionally set-color! (attribute #:contour-color))
       (optionally draw-contour! (attribute #:contour-thickness))
       (optionally fill-contour! (attribute #:fill-color))
       ))

    (('caption . attributes)
     (let ((attribute (from attributes)))
       (optionally set-color! #;to (attribute #:color))
       (optionally set-font-size! #;to (attribute #:size))
       (optionally set-font-face! #;to (attribute #:font))
       (apply move-to! (attribute #:position))
       (show-text! (attribute #:text))))

    (('space . _)
     (noop))

    (('bitmap . attributes)
     (throw 'not-implemented/yet?))

    (((group ...) ...)
     (for-each draw! group))))

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
    (('shape . attributes)
     (let* ((attributes children (attributes+children attributes))
	    (children (map (lambda (child)
			     (match child
			       ((x y)
				`(,(+ x dx) ,(+ y dy)))
			       (((x-in y-in) (x y) (x-out y-out))
				`((,(+ x-in dx) ,(+ y-in dy))
				  (,(+ x dx) ,(+ y dy))
				  (,(+ x-out dx) ,(+ y-out dy))))))
			   children)))
       `(shape ,@attributes ,@children)))

    (('caption . attributes)
     `(caption . ,(update-position #;in attributes)))

    (('space . attributes)
     `(space . ,(update-position #;in attributes)))

    (('bitmap . attributes)
     `(bitmap . ,(update-position #;in attributes)))

    (((group ...) ...)
     (map (lambda (drawing) 
	    (displaced drawing #;by `(,dx ,dy)))
	  group))))
