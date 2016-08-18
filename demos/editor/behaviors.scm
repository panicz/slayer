(define-module (editor behaviors)
  #:use-module (ice-9 nice-9)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (editor modes)
  #:use-module (editor relations)
  #:use-module (editor posed undo)
  #:use-module (editor limbs)
  #:use-module (oop goops)
  #:use-module (widgets 3d)
  #:use-module (scum physics)
  #:use-module (widgets physics)
  #:export (<ragdoll-movement-around-physics-view>
	    <pose-modification>))

(define-method (save-state! (object <physical-object>))
  (save-rig-state! #[object 'rig]))

(define-method (save-state! object)
  (format #t "save-state! for ~a not implemented\n"
	  (class-name (class-of object))))

(define-method (same-rig-bodies (object <physical-object>))
  (let ((bodies (rig-bodies #[object 'rig]))
	(body=>object #[object 'body=>object]))
    (map (lambda (body)
	   (hashq-ref body=>object body))
	 bodies)))

(define-method (same-rig-bodies (object <phantom-body>))
  (let* ((target #[object 'target])
	 (bodies (rig-bodies #[target 'rig]))
	 (body=>object #[target 'body=>object])
	 (object=>phantom #[object 'object=>phantom]))
    (map (lambda (body)
	   (let* ((object (hashq-ref body=>object body))
		  (phantom (hashq-ref object=>phantom object)))
	     phantom))
	 bodies)))

(define-class <ragdoll-movement-around-physics-view> (<3d-view-drag-behavior>)
  #;(walls #:init-value '())
  (dragged-bodies #:init-value '())
  (original-positions #:init-value '())
  #;(tips #:init-value '())
  (original-position #:init-value #f))

(define-method (initialize (this <ragdoll-movement-around-physics-view>) args)
  (next-method)
  (let* ((view #[this 'view])
	 (object #[this 'target])
	 (bodies (same-rig-bodies object))
	 (position #[object 'position])
	 ((_ _ z/screen) (3d->screen view position)))
    
    (save-state! object)
    (set! #[this 'screen-depth] z/screen)
    #;(set! #[this 'walls]
	  (filter-map (lambda (rig)
			(and-let* (((wall) (rig-bodies rig))
				   ('plane (body-type wall)))
			  wall))
		      (simulation-rigs (rig-simulation rixog))))
    (set! #[this 'dragged-bodies] bodies)
    (set! #[this 'original-positions] (map #[_ 'position] bodies))
    (set! #[this 'original-position] #[object 'position])
    (<< bodies)
    #;(set! #[this 'tips] (filter tip? bodies))))

(define-method (perform! (movement <ragdoll-movement-around-physics-view>)
			 x y dx dy)
  (with-context-for-joint/body-relation
   (let* ((view #[movement 'view])
	  (z #[movement 'screen-depth])
	  (displacement (screen->3d view x y z)))
     (for (body position) in (zip #[movement 'dragged-bodies]
				  #[movement 'original-positions])
       (set! #[body 'position]
	     (+ (- position #[movement 'original-position])
		displacement))))
   #;(for wall in #[movement 'walls]
     (for tip in #[movement 'tips]
       (and-let* ((distance (body-distance wall tip))
		  (normal (body-property wall 'normal))
		  ((negative? distance))
		  ((parent) (bodies-attached-to tip))
		  (position (body-property parent 'position))
		  (displacement (* (- distance) normal))
		  (desired-position (+ position displacement)))
	 (for body in #[movement 'dragged-bodies]
	   (set-body-property! body 'position
			       (+ (body-property body 'position)
				  displacement))))))))

(define-class <pose-modification> (<3d-view-drag-behavior>)
  (dragged-limb #:init-value #f))

(define-method (initialize (this <pose-modification>) args)
  (next-method)
  (let* ((object #[this 'target])
	 (view #[this 'view])
	 #;(rig #[object 'rig])
	 (body #[object 'body])
	 (position #[object 'position])
	 ((_ _ z/screen) (3d->screen view position)))
    #;(save-rig-state! rig)
    (set! #[this 'screen-depth] z/screen)
    (set! #[this 'dragged-limb] body)))

(define-method (perform! (modification <pose-modification>) x y dx dy)
   (let* ((view #[modification 'view])
	  (z #[modification 'screen-depth])
	  (dragged-limb #[modification 'dragged-limb])
	  (desired-position (screen->3d view x y z)))
     (match (current-editing-mode)
       ('demiurge
	(with-context-for-joint/body-relation
	 (apply-inverse-kinematics! #;of dragged-limb
					 #;to desired-position #;at hub?)))
       (_
	(<< `(pose modification not implemented for ,(current-editing-mode)))))))


