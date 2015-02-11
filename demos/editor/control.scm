(define-module (editor control)
  #:use-module (extra common)
  #:use-module (scum physics)
  #:use-module (extra math)
  #:use-module (extra ref)
  #:use-module (editor poses)
  #:export (
	    control!
	    reset-behaviors!
	    initiate-sequence!
	    rig-poses
	    specify-pose!
	    freeze!

	    attach-muscles-to-rig!
	    attach-pid-muscles-to-all-joints!
	    attach-velocity-muscles-to-all-joints!
	    ))

(define (pose-distance pose-1 pose-2)
  (let ((('pose . params-1) pose-1)
	(('pose . params-2) pose-2))
    (fold + 0.0
	  (map (lambda((id . value))
		 (let ((value-2 (assoc-ref params-2 id)))
		   (if (or (not value) (not value-2))
		       0
		       (abs (- value value-2)))))
	       params-1))))

(define rig-behaviors #[])

(define rig-poses #[])

(define rig-muscles #[])

(define (attach-muscles-to-rig! rig create-muscle)
  (let ((muscles #[]))
    (for joint in (rig-joints rig)
      (set! #[muscles joint] (create-muscle #;for joint)))
    (set! #[rig-muscles rig] muscles)))

#|
(define (expected-final-position #;of joint #;assuming constant-torque)
  (let ((the (lambda (property) (joint-property joint property))))
    (let ((initial-position (the 'angle))
	  (rotation-axis (the 'axis))
	  (initial-rate (the 'angle-rate)))
      (if (= (sgn initial-rate) (sgn constant-torque))
	  +inf.0
	  (let* ((left right (split-bodies-at joint))
		 (inertia^-1 (inverse (+ (bodies-inertia-tensor left axis)
					 (bodies-inertia-tensor right axis))))
		 (a b c (values (* 0.5 inertia^-1 constant-torque)
				initial-rate initial-position))
		 (delta (- (* b b) (* 4 a c)))

|#

#| 
	(body-1 (joint-property joint 'body-1))
	(body-2 (joint-property joint 'body-2))
	(name (joint-name joint))
	(axis (joint-property joint 'axis))

		 (rate-1 (norm (projection 
			   #:of (body-property body-1 'angular-velocity)
			   #:onto axis)))
	    (rate-2 (norm (projection 
			   #:of (body-property body-2 'angular-velocity)
			   #:onto axis)))
	;;(<< name 'rate: rate 'rate-2-rate-1: (- rate-1 rate-2))
|#	    

(define ((pid-muscle kp ki kd) joint)
  (let ((error-integral 0.0))
    (lambda (desired-position)
      (let ((angle (joint-property joint 'angle))
	    (rate (joint-property joint 'angle-rate)))
	(let ((error (- desired-position angle)))
	  (increase! error-integral error)
	  (force-hinge! joint (+ (* kp error)
				 (* ki error-integral)
				 (* kd rate))))))))


(define ((fake-velocity-muscle get-max-force get-max-velocity) joint)
  ;; I call these muscles fake, because in the real world we're only
  ;; able to directly control the force, and not the velocity
  (let ((simulation (rig-simulation (joint-rig joint))))
    (lambda (desired-position)
      (let* ((current-position (joint-property joint 'angle))
	     (error (- desired-position current-position))
	     (time-step (simulation-property simulation 'time-step))
	     (just-velocity (/ (abs error) time-step))
	     (velocity ((clamp (- just-velocity) just-velocity)
			(* (get-max-velocity) (sgn error)))))
	(set-joint-property! joint 'max-force (get-max-force))
	(set-joint-property! joint 'angle-rate velocity)))))

#|
(define ((optimal-muscle max-torque) joint)
  (let ((previous-external-torque-estimate 0.0)
	(previous-torque 0.0)
	(previous-angular-momentum 0.0)
	(simulation (joint-simulation joint))
	(body-1 (joint-property joint 'body-1))
	(body-2 (joint-property joint 'body-2))
	(left-bodies right-bodies (split-bodies-at joint))
	#;...)
    (lambda (desired-position)
      (let ((time-step (simulation-property  'time-step))
	    (current-position (joint-property joint 'angle))
	    (angular-velocity (joint-property joint 'angle-rate))
	    (moment-of-inertia ...))
	(let* ((position-error (- desired-position current-position))
	       (angular-momentum (* moment-of-ineria angular-velocity))
	       (angular-momentum-increment (- angular-momentum
					      previous-angular-momentum))
	       (expected-final-position (some-function
					 current-position angular-velocity
					 max-torque (- max-torque)))
	       (expected-final-position-error (- expected-final-position
						 desired-position))
	       (external-torque-estimate (- (/ angular-momentum-increment 
					       time-step) previous-torque))
	       (break-point-reached? (> 0 (* expected-final-position-error
					     position-error)))
	       (desired-torque (if break-point-reached?
				   'break!
				   'accelerate-to-breakpont!
				   ))
	       (actual-torque ((clamp (- max-torque) max-torque) 
			       desired-torque)))
	  (force-hinge! joint )
	  (set! previous-external-torque-estimate external-torque-estimate)
	  (set! previous-torque torque)
	  (set! previous-angular-momentum angular-momentum))))))
|#

(define (attach-pid-muscles-to-all-joints! #;in rig #;with-parameters kp ki kd)
  (attach-muscles-to-rig! rig (pid-muscle kp ki kd)))

(define (attach-velocity-muscles-to-all-joints! #;in rig #;with-parameters
						     max-force max-velocity)
  (attach-muscles-to-rig! rig (fake-velocity-muscle max-force max-velocity)))
							  
(define (specify-pose! #;of rig #;to pose)
  (let ((('pose . pose) pose))
    (set! #[rig-poses rig]
	  (replace-alist-bindings (or #[rig-poses rig] (tail (null-pose rig)))
				  #;with pose))))

;; wszystko fajnie, ale idealne rozwiązanie byłoby dla nas takie:
;; - rejestrujemy pozę + callback
;; - pozy zarejestrowane wcześniej mają wyższy priorytet, tzn. w razie
;;   wykrycia ich callbacki są uruchamiane jako pierwsze
;; - w każdym kroku przeglądamy wszystkie zarejestrowane pozy, i jeżeli
;;   któreś z nich są dostatecznie podobne do bieżącej, wykonujemy
;;   jej callbacka. jedna poza może być wykryta więcej niż raz, jeżeli
;;   wielokrotnie znajdzie się na liście. oczywiście można to zoptymalizować,
;;   ale nasze API powinno być jak najprostsze.

;; NASZE API:
;; dwie strategie:
;; - albo po każdej reakcji na pozę oczekiwanie zostaje usunięte. wówczas
;; żeby zachowanie było powtarzalne, jego reakcja musi o sobie przypominać
;; - albo wręcz przeciwnie -- reakcja musi o sobie zapominać, jeśli nie chcemy
;; jej wywołać w kolejnym kroku

;; interfejs:
(define (reset-behaviors! rig)
  (set! #[rig-behaviors rig] '()))

(define (register-behavior! rig pose reaction)
  (set! #[rig-behaviors rig] `((,pose . ,reaction) 
			       ,@(or #[rig-behaviors 'rig] '()))))

(define (unregister-behavior! rig pose reaction)
  (set! #[rig-behaviors rig] 
	(delete-first `(,pose . ,reaction) #;from #[rig-behaviors rig])))

(define (initiate-sequence! sequence rig)
  (if (null? sequence)
      (display "sequence must be non-empty\n")
      (letrec ((next-pose! (lambda _
			     (<< "next-pose!")
			     (match* sequence
			       ((current . rest)
				(unregister-behavior! rig current next-pose!)
				(set! sequence rest)
				(match* rest
				  ((next . _)
				   (register-behavior! rig next next-pose!)
				   (specify-pose! #;of rig #;to next))))))))
	(register-behavior! rig (first #;in sequence) next-pose!)
	(specify-pose! #;of rig #;to (first #;in sequence)))))

(define (freeze! rig)
  (let ((pose (pose #;of rig)))
    (reset-behaviors! #;of rig)
    (specify-pose! #;of rig #;to pose)))

(define ((pd-drive kp kd) joint desired-value)
  (let ((angle (joint-property joint 'angle))
	(rate (joint-property joint 'angle-rate)))
    (let ((error (- desired-value angle)))
      (force-hinge! joint (+ (* kp error) (* kd rate))))))

(define (control!)
  (for (rig => behaviors) in rig-behaviors
    (for (trigger-pose . reaction!) in behaviors
      (let ((d (pose-distance trigger-pose (pose #;of rig))))
	(when (< d 0.2)
	  (reaction! rig pose)))))

  (for (rig => rig-pose) in rig-poses
    (let ((muscles #[rig-muscles rig]))
      ;;(<< (pose-distance `(pose ,@rig-pose) (pose #;of rig)))
      (for (joint-name . value) in rig-pose
	(let* ((joint (joint-named joint-name #;from rig))
	       (muscle-joint! #[muscles joint]))
	  (when value
	    (muscle-joint! #;to value))))
      )))
