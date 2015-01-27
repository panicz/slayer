(define-module (scum physics)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra trimesh)
  #:export (define-rig-for make-rig
	     primitive-make-simulation
	     make-simulation-step!
	     current-simulation-step
	     set-simulation-rig-maker!
	     simulation-rig-maker
	     simulation-rigs
	     simulation-property
	     set-simulation-property!
	     simulation-bodies
	     simulation-joints
	     simulation?

	     primitive-make-rig
	     rig-bodies
	     rig-joints
	     rig?

	     make-body
	     set-body-property!
	     body-property
	     body-rig
	     body-type
	     body-named
	     body-name
	     body-distance
	     body?

	     force!
	     torque!

	     make-joint
	     set-joint-property!
	     joint-property
	     joint-rig
	     joint-type
	     joint-named
	     joint-name
	     joint?
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
   (lambda (sim position orientation) 
     (WARN "define-rig-for: position and orientation not supported")
     (match rig-def
       (; STRUCTURE
	('rig ('bodies body-spec ...)
	      ('joints joint-defs ...))
	;; ACTION
	(let ((rig (primitive-make-rig sim)))
	  (for (name (type props ...)) in body-spec
	    (let ((body (make-body rig type name)))
	      (for (property value) in (map-n 2 list props)
		(let ((property (keyword->symbol property))
		      (actual (lambda (value)
				(match value
				  (('load-mesh name)
				   (format #t "~a: " name)
				   (3d->trimesh (with-input-from-file
						    name read)))
				  (else
				   value)))))
		  (set-body-property! body property (actual value))))))
	  (for (name (type props ...)) in joint-defs
	    (let ((joint (make-joint rig type name))
		  (literal (rec (literal value)
				(match value
				  ((? symbol? body-name)
				   (body-named body-name rig))
				  (('? property-name body-name)
				   (body-property (body-named body-name rig)
						  property-name))
				  (else 
				   else)))))
	      (for (property value) in (map-n 2 list props)
		(let ((property (keyword->symbol property)))
		  (set-joint-property! joint property (literal value))))))
	  #;return rig))))))

(define* (make-rig simulation name #:key (position #f32(0 0 0))
		   (orientation '(1 . #f32(0 0 0))))
  (let ((rig-maker (simulation-rig-maker simulation name)))
    (if (procedure? rig-maker)
	(rig-maker simulation position orientation)
	(throw 'undefined-rig simulation name))))

(define* (force! body force #:key (local #f) (at #f) (relative #f))
  (if local
      (if relative
	  (if at 
	      (body-add-local-force-at-relative-position! body force at)
	      (body-add-local-force! body force))
	  (if at
	      (body-add-local-force-at-position! body force at)
	      (body-add-local-force! body force)))
      (if relative
	  (if at 
	      (body-add-force-at-relative-position! body force at)
	      (body-add-force! body force))
	  (if at
	      (body-add-force-at-position! body force at)
	      (body-add-force! body force)))))

(define* (torque! body torque #:key (local #f))
  (if local 
      (body-add-local-torque! body torque)
      (body-add-torque! body torque)))

(define (simulation-bodies sim)
  (append-map rig-bodies (simulation-rigs sim)))

(define (simulation-joints sim)
  (append-map rig-joints (simulation-rigs sim)))
