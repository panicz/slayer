(define-module (extra slayer)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:export (rgba highlighted subtract-image force-redisplay! key
		 shift? ctrl? alt?)
  #:export-syntax (with-video-output-to key-bindings)
  #:replace (
	     (keydn-replacement . keydn)
	     ))

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
		    (if (< (apply max diffs) tolerance)
			0 ;; make this pixel of checker transparent
			x))))  ;; or leave it as it is
	      (image->array subtrahend)
	      (image->array minuend))))

(define redisplay-event (register-userevent! noop))

(define (force-redisplay!)
  (generate-userevent! redisplay-event))

(define-syntax-rule (with-video-output-to screen action . *)
  (call-with-video-output-to screen (lambda () action . *)))

(define-syntax-rule (key-bindings bindings ...)
  (let ((fresh-bindings (fresh-key-bindings)))
    (with-fluids ((KEY-BINDINGS fresh-bindings))
      bindings ...
      fresh-bindings)))

(define modes #[])

(add-timer! 
 30 #;ms
 (lambda()
   (for (key => proc) in modes
     (proc))))

(define (key name fun)
  (keydn name 
    (lambda()
      (hash-set! modes name fun)))
  (keyup name 
    (lambda()
      (hash-remove! modes name))))

(define (shift?)
  (modifier-pressed? 'shift))

(define (ctrl?)
  (modifier-pressed? 'ctrl))

(define (alt?)
  (modifier-pressed? 'alt))

(define modifiers #[])

(define (expand-modifier modifier)
  (let ((substitutions '((ctrl lctrl rctrl)
			 (alt lalt ralt)
			 (shift lshift rshift)
			 (meta lmeta rmeta)
			 (super lsuper rsuper))))
    (or (assoc-ref substitutions modifier)
	(list modifier))))

(define (keydn* (modifier target) action)
  (let ((old-bindings (current-key-bindings)))
    (for modifier in (expand-modifier modifier)
      (set-key-bindings! (or #[modifiers modifier] (fresh-key-bindings)))
      (keydn target action)
      (keyup target noop)
      (keyup modifier 
	(lambda () 
	  (set-key-bindings! old-bindings)))
      (set! #[modifiers modifier] (current-key-bindings))
      (set-key-bindings! old-bindings)
      (keydn modifier 
	(lambda () 
	  (set! old-bindings (current-key-bindings))
	  (set-key-bindings! #[modifiers modifier]))))))

(define (keydn-replacement key action)
  (match key
    ((_ . _)
     (keydn* key action))
    (_
     (keydn key action))))
