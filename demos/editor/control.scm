(define-module (editor control)
  #:use-module (extra common)
  #:use-module (scum physics)
  #:use-module (extra math)
  #:use-module (extra ref)
  #:use-module (editor poses)
  #:use-module (editor limbs)
  #:use-module (editor relations)
  #:use-module (editor modes)
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


  ;; no, i tutaj sie zaczyna! 
  ;; plan działania jest następujący:

  ;; 1. dokonujemy rzutowania punktów kontaktowych
  ;; na płaszczyznę grawitacji. w ten sposób otrzymujemy
  ;; figurę stabilności, oraz transformację liniową (macierz
  ;; albo kwaternion), pozwalającą na transformację naszych
  ;; rozważań na płaszczyznę (x, y).

  ;; 2. określamy sobie kończyny podpierające i rozpisujemy
  ;; środek masy (albo rzut środka masy) jako funkcję
  ;; kątów kończyny podpierającej (dla każdej kończyny)

  ;; 3. dla każdej z kończyn wyliczamy (z SVD) takie kąty,
  ;; żeby rzut środka masy znalazł się w położeniu środka
  ;; geometrycznego figury stabilności. Te nowe kąty
  ;; wyznaczają naszą nową pozę (gdyby strategia prostej
  ;; superpozycji nie działała, moglibyśmy spróbować
  ;; strategii foldowania)

  ;; Teraz na nowo musimy sobie przypomnieć, jak wygląda
  ;; działanie funkcji body-contacts:
  ;; (body-contacts b1) 
  ;;    -> ((b2 (pos1 norm1 depth1) (pos2 norm2 depth2) ...) ...)
  ;; 

(define (contact-bodies+points rig)
  (let* ((own-bodies (rig-bodies rig))
	 (mass-center (rig-mass-center rig))
	 (((support-bodies (contact-points normals depths) ...) ...)
	  (append-map (lambda (body)
		 (filter-map (lambda ((touched-body . details))
			       (and (not (in? touched-body 
					      own-bodies))
				    `(,body . ,details)))
			     (body-contacts body)))
	       own-bodies))
	 (support-bodies (delete-duplicates support-bodies)))
    (values 
     support-bodies 
     (concatenate contact-points))))

(define (stability-region contact-points to-XY-plane)
  (let* ((points/complex (map (lambda (point)
				(let (((x y z)
				      (uniform-vector->list
				       (rotate point 
					       to-XY-plane))))
				  (make-rectangular x y)))
			      contact-points))
	 (convex-hull/complex points/complex))))

(with-default ((debug-stabilize noop))
  (define (stabilize pose #;of rig)
    (specify ((joint-property-getter joint-property)
	      (body-rig-getter body-rig)
	      (rig-joints-getter rig-joints))
      (or
       (and-let* ((simulation (rig-simulation rig))
		  (gravity (simulation-property simulation 'gravity))
		  (mass-center (rig-mass-center rig))
		  (support-bodies contact-points (contact-bodies+points rig))
		  ((and (>= (length support-bodies) 0) (> (length contact-points) 3)))
		  (to-XY-plane (rotation-quaternion #;from gravity #;to #f32(0 0 1)))
		  (stability-region (stability-region contact-points to-XY-plane))
		  (mass-center/XY (let (((x y z) (uniform-vector->list 
						  (rotate mass-center to-XY-plane))))
				    (make-rectangular x y)))
		  ;; we don't want to fix pose if we're already inside hull
		  ((not (inside-hull?/complex mass-center/XY stability-region)))
		  ;; i teraz tak: pożądanym punktem, zamiast środka otoczki,
		  ;; powinien być jakiś dogodny punkt znajdujący się wewnątrz
		  ;; otoczki. Rodzi to pytanie następujące: jak wyznaczyć najbliższy
		  ;; punkt względem danej otoczki?
		  (desired-center/XY (box-center/complex stability-region))
		  (displacement (let* (((x y) (complex->list (- desired-center/XY
								mass-center/XY)))
				       (displacement/XY (list->uniform-vector 
							 'f32 `(,x ,y ,0))))
				  (rotate displacement/XY (~ to-XY-plane))))
		  (desired-mass-center (+ mass-center displacement))
		  (joint-sequences (map (lambda (body)
					  (joint-sequence-to-nearest-member 
					   hub? #;from body))
					support-bodies))
		  (joint-sequences (filter (lambda (sequence)
					     (>= (length sequence) 2))
					   joint-sequences))
		  (joint-sequences (remove (lambda (sequence)
					     (any (lambda (other-sequence)
						    (proper-suffix? sequence 
								    other-sequence))
						  joint-sequences))
					   joint-sequences))
		  ((not (null? joint-sequences)))
		  (functions/angles (map (lambda (sequence)
					   (tip/angles sequence mass-center))
					 joint-sequences))
		  (desired-poses (map (lambda (mass-center/angles sequence)
					(let* ((angles (map (lambda (joint)
							      (joint-property joint 'angle))
							    sequence))
					       (names (map joint-name sequence))
					       (desired (desired-configuration
							 mass-center
							 desired-mass-center
							 angles
							 mass-center/angles)))
					  (assert (eq? mass-center (mass-center/angles)))
					  `(pose . ,(map cons names desired))))
				      functions/angles joint-sequences))
		  (('pose . pose) (fold-left combine-poses `(pose . ,pose) desired-poses)))
	 ((specific debug-stabilize) desired-mass-center stability-region contact-points
	  to-XY-plane)
	 pose)
       pose))))

(define (control!)
  (for (rig => behaviors) in rig-behaviors
    (for (trigger-pose . reaction!) in behaviors
      (let ((d (pose-distance trigger-pose (pose #;of rig))))
	(when (< d 0.2)
	  (reaction! rig pose)))))

  (for (rig => rig-pose) in rig-poses
    (let ((muscles #[rig-muscles rig])
	  (rig-pose (stabilize rig-pose rig)))
      ;;(<< (pose-distance `(pose ,@rig-pose) (pose #;of rig)))
      (for (joint-name . value) in rig-pose
	(let* ((joint (joint-named joint-name #;from rig))
	       (muscle-joint! #[muscles joint]))
	  (when value
	    (muscle-joint! #;to value))))
      )))


;; plan działania: (chyba już coś takiego było robione na lenovie,
;; ale dopóki nie odzyskam dysku twardego, będę musiał improwizować
;; od nowa).
;; chcemy mieć nową funkcję, "stabilize", która pobiera punkty
;; kontaktowe i zwraca skorygowaną pozę. Może zresztą powinniśmy
;; móc sterować w trybie stabilizacji? Nie wiadomo.

;; Zasadniczo więc jesteśmy w następującej sytuacji. Albo się
;; poruszamy (jesteśmy w trakcie wykonywania ruchu), albo stoimy
;; w miejscu