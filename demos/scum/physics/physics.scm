(define-module (scum physics)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (extra trimesh)
  #:export (define-rig make-rig
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
	     ))

(load-extension "physics" "init")

(define rig-definitions #[])

(define-syntax-rule (define-rig rig-name rig-definition)
  (let ((('rig . definition) rig-definition))
    (set! #[rig-definitions 'rig-name] definition)))

(define* (make-rig simulation rig-name)
  (let* (((('bodies body-spec ...)
	   ('joints joint-defs ...)) #[rig-definitions rig-name])
	 (rig (primitive-make-rig simulation rig-name)))
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
    #;return rig))

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

(define (reset-rig! rig)
  (for body in (rig-bodies rig)
    ...))

(define (reset-simulation! sim)
  (for rig in (simulation-rigs sim)
    (reset-rig! rig)))
