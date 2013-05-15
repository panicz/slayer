(define-module (libs physics)
  #:use-module (extra common)
  #:export (define-rig-for make-rig
	     primitive-make-simulation
	     simulation-step!
	     set-simulation-rig-maker!
	     simulation-rig-maker
	     simulation-rigs
	     simulation-property
	     set-simulation-property!
	     
	     primitive-make-rig
	     rig-bodies

	     make-body
	     set-body-property!
	     body-property
	     body-type
	     body-named

	     force!
	     torque!

	     make-joint
	     set-joint-property!
	     joint-property
	     joint-type
	     )
  #:export-syntax (define-rigs-for))

(load-extension "physics" "init")

(define-syntax define-rigs-for
  (syntax-rules ()
    ((define-rigs-for sim (rig1-name rig1-def) (rig2-name rig2-def)...)
     (begin (define-rig-for sim (quote rig1-name) rig1-def)
	    (define-rig-for sim (quote rig2-name) rig2-def)
	    ...))))

(define (define-rig-for simulation rig-name rig-def)
  (set-simulation-rig-maker! 
   simulation rig-name
   (lambda (sim) 
     (match rig-def
       (; STRUCTURE
	('rig
	 ('bodies
	  body-spec ...)
	 ('joints
	  joint-defs ...))
	;; ACTION
	(let ((rig (primitive-make-rig sim)))
	  (for (name (type props ...)) in body-spec
	       (let ((body (make-body rig type name)))
		 (for (property value) in (map-n 2 list props)
		      (set-body-property! body 
					  (keyword->symbol property) 
					  value))))
	  (for (type props ...) in joint-defs
	       (let ((joint (make-joint rig type))
		     (literal (rec (literal x)
				   (match x
				     ((? symbol? body-name)
				      (body-named body-name rig))
				     (('? property-name body-name)
				      (body-property (body-named body-name rig)
						     property-name))
				     (else 
				      else)))))
		 (for (property value) in (map-n 2 list props)
		      (set-joint-property! joint 
					   (keyword->symbol property)
					   (literal value)))))
	  rig))))))

(define (make-rig simulation name)
  (let ((rig-maker (simulation-rig-maker simulation name)))
    (if (procedure? rig-maker)
	(rig-maker simulation)
	(throw 'undefined-rig simulation name))))

(define* (force! body force #:key (local #f) (at #f) (relative #f))
  (if local
      (if relative
	  (if at 
	      (add-body-local-force-at-relative-position! body force at)
	      (add-body-local-force! body force))
	  (if at
	      (add-body-local-force-at-position! body force at)
	      (add-body-local-force! body force)))
      (if relative
	  (if at 
	      (add-body-force-at-relative-position! body force at)
	      (add-body-force! body force))
	  (if at
	      (add-body-force-at-position! body force at)
	      (add-body-force! body force)))))

(define* (torque! body torque #:key (local #f))
  (if local 
      (add-body-local-torque! body torque)
      (add-body-torque! body torque)))
