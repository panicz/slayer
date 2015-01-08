(define-module (extra noobs)
  #:use-module ((oop goops) #:renamer (symbol-prefix-proc 'goops:))
  #:use-module (extra common)
  #:export (maybe/class list-of/class make-class)
  #:export-syntax (define-class)
  #:re-export ((goops:slot-ref . slot-ref)
	       (goops:slot-set! . slot-set!)
	       (goops:<top> . <any>)
	       ;...
	       ))

;; This module defines a wrapper system around the GOOPS system.
;; It has a different syntax that's based mainly on signalling
;; mechanism.
;; The drawback is that some slots are created silently, and the
;; setters for signals silently append the corresponding hooks,
;; rather than replacing them. This is historical issue, whose
;; purpose was backwards-compatibility. As such it may change
;; in the future

(define make-class goops:make-class)

(define ((maybe/class class) object)
  (or (goops:is-a? object class)
      (not object)))

(define ((list-of/class class) object)
  (and (list? object)
       (every (lambda(x)(goops:is-a? x class)) object)))

(define-syntax define-class
  (lambda (stx)
    (define (->syntax datum) (datum->syntax stx datum))
    (define (proper-name? syntax-object)
      (and (identifier? syntax-object)
	   (let ((name (symbol->string (syntax->datum syntax-object))))
	     (and (not (string-match "^%" name))
		  (not (string-match "-hook$" name))))))
    (define (syntax-name . syntaxes)
      (->syntax (string->symbol 
		 (fold-right string-append "" 
			     (map ->string 
				  (map syntax->datum syntaxes))))))
    (define (getter syntax-object)
      (syntax-case syntax-object (?)
	(((name %name) ())
	 #'(lambda(self)(goops:slot-ref self '%name)))
	(((name %name) (? value . _))
	 #'(lambda(self) value))
	(((name %name) (_ value . rest))
	 (getter #'(*name rest)))))
    (define (setter syntax-object)
      (define ((arg/value name value) arg)
	(if (bound-identifier=? arg name)
	    value
	    #`(goops:slot-ref self '#,arg)))
      (syntax-case syntax-object (? !)
	(((name %name) ())
	 #'(lambda(self value)
	     (goops:slot-set! self '%name value)))
	(((name %name) (? value))
	 #'noop)
	(((name %name) (! (hook args ...) . _))
	 (with-syntax (((args ...) (map (arg/value #'name #'value) 
					#'(args ...))))
	   #'(lambda(self value)
	       (unless (equal? (goops:slot-ref self '%name) value)
		 ((goops:slot-ref self 'hook) 
		  args ...)
		 (goops:slot-set! self '%name value)))))
	(((name %name) (_ value . rest))
	 (setter #'(*name rest)))))
    (define (goopsify syntax-object)
      (syntax-case syntax-object ()
	(((name arguments ...))
	 (proper-name? #'name)
	 (with-syntax ((name-hook (syntax-name #'name #'-hook)))
	   #'((name-hook #:init-thunk 
			 (lambda () (make-hook (length '(arguments ...)))))
	      (name 
	       #:allocation #:virtual
	       #:slot-ref 
	       (lambda (self)
		 (lambda args
		   (apply run-hook (goops:slot-ref self 'name-hook) args)))
	       #:slot-set!
	       (lambda (self value)
		 (add-hook! (goops:slot-ref self 'name-hook) value #t))))))
	((type name accessors ...)
	 (proper-name? #'name)
	 (with-syntax ((%name (syntax-name #'% #'name)))
	   (with-syntax ((ref (getter #'((name %name) (accessors ...))))
			 (set! (setter #'((name %name) (accessors ...)))))
	     #'((%name #:init-value #f)
		(name #:allocation #:virtual
		      #:slot-ref ref
		      #:slot-set! set!)))))))
    (syntax-case stx ()
      ((_ class-name (supers ...)
	  slot-specifications ...)
       (with-syntax (((goops-slots ...) 
		      (append-map goopsify #'(slot-specifications ...))))
	 #'(goops:define-class 
	    class-name (supers ...)
	    goops-slots ...))))))

(e.g.
 (define-class <widget> ()
   ((? (maybe/class <widget>)) parent)
   ((? (list-of/class <widget>)) children ? '())
   ((left-mouse-down x y))
   ((left-mouse-up x y))
   ((left-click x y))
   ((right-mouse-down x y))
   ((right-mouse-up x y))
   ((right-click x y))
   ((move x y))
   ((resize w h))
   ((? integer?) x ! (move x y))
   ((? integer?) y ! (move x y))
   ((? natural?) w ! (resize w h))
   ((? natural?) h ! (resize w h))))
