(define-module (extra slayer)
  #:use-module (slayer)
  #:use-module (slayer image)
  ;;#:use-module (extra common)
  #:use-module (grand scheme)
  #:use-module (extra array)
  #:use-module (extra ref)
  #:export (now rgba highlighted subtract-image force-redisplay! key
		shift? ctrl? alt? add-mode! remove-mode!
		event-triggered-hook
		event-handling-log-hook
		procedure-origin binding-origins
		call-event-handler hook
		current-event-source)
  #:export-syntax (with-video-output-to key-bindings event define-event)
  #:replace (
	     (keydn-replacement . keydn)
	     (mousemove/log . mousemove)
	     ))

(define ((hook . args))
  (make-hook (length args)))

(define (now)
  (let ((`(,seconds . ,microseconds) (gettimeofday)))
    (+ (* seconds 1000000) microseconds)))

(define event-triggered-hook ((hook 'event)))

(define event-handling-log-hook ((hook 'message)))

(define (procedure-origin procedure)
  (and (procedure? procedure)
       (or (procedure-source procedure)
	   (procedure-name procedure)
	   #;procedure)))

(define (binding-origins key)
  (let* ((keydn-origin (procedure-origin (keydn key)))
	 (keyup-origin (procedure-origin (keyup key))))
    (match `(,keydn-origin ,keyup-origin)
      (`((lambda () (add-mode! ,key ,function))
	 (lambda () (remove-mode! ,key)))
       `((key ,key ,function)))
      (_
       `(,@(if (and keydn-origin
		    (not (eq? keydn-origin 'noop)))
	       `((keydn ',key ,keydn-origin))
	       '())
	 ,@(if (and keyup-origin
		    (not (eq? keyup-origin 'noop)))
	       `((keyup ',key ,keyup-origin))
	       '()))))))

(define-syntax (event args . body)
  (with-procedure-properties ((designated-handler #true))
    (lambda args . body)))

(define (designated-handler? procedure)
  "A designated handler is a procedure which should not \
be reported to the logging system, because it contains some \
explicit logging code in its definition."
  (or (eq? procedure noop)
      (procedure-property procedure 'designated-handler)))

(define-syntax (define-event (name . args) . body)
  (define name (event args . body)))

(define current-event-source (make-parameter #false))

(define (apply-event-handler handler args)
  (unless (designated-handler? handler)
    (let ((message (or (and-let* ((proc (procedure-origin handler)))
			 `(,proc . ,args))
		       (with-output-to-string
			 (lambda ()
			   (print ";; no source for "handler" "args))))))
      (run-hook event-handling-log-hook message)))
  (apply handler args))

(define (call-event-handler handler . args)
  (apply-event-handler handler args))

(define (triggering-event event action)
  (with-procedure-properties ((source (procedure-source action)))
    (lambda args
      (let ((source-event `(,@event ,@args)))
	(parameterize ((current-event-source source-event))
	  (run-hook event-triggered-hook source-event)
	  (apply-event-handler action args))))))

(define (keydn/log key reaction)
  (keydn key (triggering-event `(key-down ,key) reaction)))

(define (keyup/log key reaction)
  (keyup key (triggering-event `(key-up ,key) reaction)))

(define (mousemove/log reaction)
  (mousemove (triggering-event '(mouse-move) reaction)))

(define rgba
  (case-lambda 
    ((color)
     (decompose-color-to-rgba color))
    ((r g b a)
     (compose-color-from-rgba r g b a))))

(define* (highlighted image red #:= 0 green #:= 0 blue #:= 0)
  (array->image
   (array-map (lambda(pixel)
		(let ((`(,r ,g ,b ,a) (rgba pixel))
		      (+ (lambda (x y) (min 255 (+ x y)))))
		  (rgba (+ r red) (+ g green) (+ b blue) a)))
	      (image->array image))))

(define* (subtract-image subtrahend minuend tolerance #:= 100)
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

(define *modes* #[])

(define (add-mode! source handler)
  (hash-set! *modes* source handler))

(define (remove-mode! source)
  (hash-remove! *modes* source))

(add-timer! 
 30 #;ms
 (lambda()
   (for (key => proc) in *modes*
     ;;(run-hook log-event '(timer) proc)
     (proc))))

(define (key name fun)
  (keydn/log name
	     (with-procedure-properties
		 ((source `(lambda () (add-mode! ',name ,(name/source fun)))))
	       (lambda ()
		 (add-mode! name fun))))
  (keyup/log name
	     (with-procedure-properties
		 ((source `(lambda () (remove-mode! ',name))))
	       (lambda()
		 (remove-mode! name)))))

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
  (for modifier in (expand-modifier modifier)
    (let ((old-bindings (current-key-bindings)))
      (set-key-bindings! (or #[modifiers modifier] (fresh-key-bindings)))
      (keydn/log target action)
      (keyup/log target noop)
      (keyup/log modifier 
		 (lambda () 
		   (set-key-bindings! old-bindings)))
      (set! #[modifiers modifier] (current-key-bindings))
      (set-key-bindings! old-bindings)
      (keydn/log modifier 
		 (lambda () 
		   (set! old-bindings (current-key-bindings))
		   (set-key-bindings! #[modifiers modifier]))))))

(define (keydn-replacement . key+action)
  (match key+action
    (`((,modifier ,target) ,action)
     (keydn* `(,modifier ,target) action))
    (`(,key ,action)
     (keydn/log key action))
    (`(,key)
     (keydn key))))
