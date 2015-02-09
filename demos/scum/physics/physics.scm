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
	     reset-simulation!

	     primitive-make-rig
	     rig-bodies
	     rig-joints
	     rig?
	     rig-simulation
	     stop-rig!
	     reset-rig!
	     rig-state
	     set-rig-state!
	     rig-mass-center

	     make-body
	     set-body-property!
	     body-property
	     body-rig
	     body-type
	     body-named
	     body-name
	     body-distance
	     body?
	     stop-body!
	     reset-body!
	     body-state
	     set-body-state!

	     bodies-mass-center

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
	     joint-state
	     set-joint-state!

	     force-hinge!
	     ))

(load-extension "physics" "init")

(define rig-definitions #[])

(define-syntax-rule (define-rig rig-name rig-definition)
  (let ((('rig . definition) rig-definition))
    (set! #[rig-definitions 'rig-name] definition)))

(define (reset-body! body)
  (define (actual value)
    (match value
      (('load-mesh name)
       (load-trimesh name))
      (_
       value)))
  (let* ((rig (body-rig body))
	 ((('bodies . bodies-spec) _) #[rig-definitions (rig-name rig)])
	 (((type . body-spec)) #[bodies-spec (body-name body)])
	 (properties (map-n 2 (lambda (property value)
				      `(,(keyword->symbol property) ,value))
				  body-spec)))
    (for (property value) in properties
      (set-body-property! body property (actual value)))))

(define (reset-joint! joint)
  (define (literal value)
    (match value
      ((? symbol? body-name)
       (body-named body-name (joint-rig joint)))
      (('? property-name body-name)
       (body-property (body-named body-name (joint-rig joint))
		      property-name))
      (_
       value)))
  (let* ((rig (joint-rig joint))
	 ((_ ('joints . joints-spec)) #[rig-definitions (rig-name rig)])
	 (((type . joint-spec)) #[joints-spec (joint-name joint)])
	 (properties (map-n 2 (lambda (property value)
				      `(,(keyword->symbol property) ,value))
			    joint-spec)))
    (for (property value) in properties
      (set-joint-property! joint property (literal value)))))

(define* (make-rig simulation rig-name)
  (let* (((('bodies body-spec ...)
	   ('joints joint-defs ...)) #[rig-definitions rig-name])
	 (rig (primitive-make-rig simulation rig-name)))
    (for (name (type . _)) in body-spec
      (let ((body (make-body rig type name)))
	(reset-body! body)))
    (for (name (type . _)) in joint-defs
      (let ((joint (make-joint rig type name)))
	(reset-joint! joint)))
    rig))

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

(define (force-hinge! joint value)
  (assert (and (eq? (joint-type joint) 'hinge)
	       (real? torque)))
  (and-let* ((axis (joint-property joint 'axis))
	     (body-1 (joint-property joint 'body-1))
	     (body-2 (joint-property joint 'body-2)))
    (torque! body-1 (* (* +0.5 value) axis))
    (torque! body-2 (* (* -0.5 value) axis))))

(define (simulation-bodies sim)
  (append-map rig-bodies (simulation-rigs sim)))

(define (simulation-joints sim)
  (append-map rig-joints (simulation-rigs sim)))

(define (stop-body! body)
  (set-body-property! body 'velocity #f32(0 0 0))
  (set-body-property! body 'angular-velocity #f32(0 0 0))
  (set-body-property! body 'force #f32(0 0 0))
  (set-body-property! body 'torque #f32(0 0 0)))

(define (reset-rig! rig)
  (for body in (rig-bodies rig)
    (stop-body! body)
    (reset-body! body))
  (for joint in (rig-joints rig)
    (reset-joint! joint)))

(define (stop-rig! rig)
  (for body in (rig-bodies rig)
    (stop-body! body)))

(define (reset-simulation! sim)
  (for rig in (simulation-rigs sim)
    (reset-rig! rig)))

(define (rig-state rig)
  `(rig-state (bodies ,@(map (lambda (body)
			       `(,(body-name body) . ,(body-state body)))
			     (rig-bodies rig)))
	      (joints ,@(map (lambda (joint)
			       `(,(joint-name joint) . ,(joint-state joint)))
			     (rig-joints rig)))))

(define (rig-mass-center rig)
  (bodies-mass-center (rig-bodies rig)))

(define (bodies-mass-center bodies)
  (let ((((positions . masses) ...)
	 (map (lambda (body) 
		`(,(body-property body 'position)
		  . ,(body-property body 'mass)))
	      bodies)))
    (/ (fold + #f32(0 0 0) (map * positions masses))
       (fold + 0.0 masses))))

(define (set-rig-state! rig state)
  (let ((('rig-state ('bodies . bodies-states)
		     ('joints . joints-states)) state))
    (for (body-name . state) in bodies-states
      (set-body-state! (body-named body-name #;from rig) state))
    (for (joint-name . state) in joints-states
      (set-joint-state! (joint-named joint-name #;from rig) state))))

(define (body-state body)
  (map (lambda (property)
	 `(,property . ,(body-property body property)))
       '(position quaternion)))

(define (set-body-state! body state)
  (for (property . value) in state
    (set-body-property! body property value)))

(define (joint-state joint)
  (map (lambda (property)
	 `(,property . ,(joint-property joint property)))
       '(anchor axis)))

(define (set-joint-state! joint state)
  (for (property . value) in state
    (set-joint-property! joint property value)))

(define (joint-simulation joint)
  (rig-simulation (joint-rig joint)))

(define (body-simulation body)
  (rig-simulation (body-rig body)))
