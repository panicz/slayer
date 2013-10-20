(define-module (extra slayer)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (extra common)
  #:export (rgba highlighted subtract-image force-redisplay!))

(define rgba
  (case-lambda 
    ((color)
     (decompose-color-to-rgba color))
    ((r g b a)
     (compose-color-from-rgba r g b a))))

(define* (highlighted image #:key (red 0) (green 0) (blue 0))
  (array->image
   (array-map (lambda(pixel)
		(match-let (((r g b a) (rgba pixel))
			    (+ (lambda (x y) (min 255 (+ x y)))))
		  (rgba (+ r red) (+ g green) (+ b blue) a)))
	      (image->array image))))

(define* (subtract-image subtrahend minuend #:key (tolerance 100))
  (array->image
   (array-map (lambda (x y)
		(let ((xrgba (rgba x))
		      (yrgba (rgba y)))
		  (let ((diffs (map abs (map - xrgba yrgba))))
		    (if (< (apply max diffs) 100)
			0 ;; make this pixel of checker transparent
			x))))  ;; or leave it as it is
	      (image->array subtrahend)
	      (image->array minuend))))

(define redisplay-event (register-userevent! noop))

(define (force-redisplay!)
  (generate-userevent! redisplay-event))
