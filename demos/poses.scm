#!../src/slayer -e3d
!#
(use-modules (slayer)
	     (slayer 3d)
	     (widgets base)
	     (widgets physics)
	     (oop goops)
	     (extra ref)
	     (extra common)
	     (extra 3d)
	     (extra math)
	     (extra slayer)
	     (scum physics))

#|
(set! %load-path (append '("." "./guile-modules" ".." "../guile-modules") 
			 %load-path))

(use-modules (oop goops)(extra common)(extra ref) (extra math))

(rotation-quaternion #;from #f32(0 0 1) #;to #f32(1 0 0))
|#

(define (distance pose-1 pose-2)
  (match-let ((('pose . params-1) pose-1)
	      (('pose . params-2) pose-2))
    (fold + 0.0
	  (map (lambda((id . value))
		 (let ((value-2 (assoc-ref params-2 id)))
		   (if (or (not value) (not value-2))
		       0
		       (abs (- value value-2)))))
	       params-1))))

(define (arrangement rig)
  (let ((joints (rig-joints rig)))
    `(pose (,(joint-name joints) . ,(joint-property joints 'angle)) ...)))

(define (set-pose! #;of rig #;to pose)
  ;; musimy ustalić wszystkie wartości kątów poszczególnych zawiasów tak,
  ;; żeby były zgodne z pozą
  (match-let ((('pose . pose) pose))
    (set! #[rig-poses rig]
	  (replace-alist-bindings (or #[rig-poses rig] '()) pose))))

(define (null-pose #;for rig)
  (let ((joints (rig-joints rig)))
    `(pose (,(joint-name joints) . 0.0) ...)))

(define rig-behaviors #[])

(define rig-poses #[])

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

;; sekwencer póz
(define (initiate-sequence! sequence rig)
  (letrec ((next-pose! (lambda _
			 (display "next-pose!\n")
			 (match* sequence
			   ((current . rest)
			    (unregister-behavior! rig current next-pose!)
			    (set! sequence rest)
			    (match* rest
			      ((next . _)
			       (register-behavior! rig next next-pose!)
			       (set-pose! #;of rig #;to next))))))))
    (register-behavior! rig (first #;in sequence) next-pose!)
    (set-pose! #;of rig #;to (first #;in sequence))))

(define (freeze! rig)
  (let ((pose (arrangement #;of rig)))
    (reset-behaviors! #;of rig)
    (set-pose! #;of rig #;to pose)))



(rotation-quaternion #f32(0 0 1) (normalized #f32(-0.2 0 0.8)))


(define ((pd-drive kp kd) joint desired-value)
  (let ((the (lambda(name)(joint-property joint name))))
    (let ((axis (the 'axis))
	  (angle (the 'angle))
	  (rate (the 'angle-rate))
	  (body-1 (the 'body-1))
	  (body-2 (the 'body-2)))
      (let ((error (- desired-value angle)))
	(torque! body-1 (* (- (* 0.5 kp error)
			      (* 0.5 kd rate)) axis))
	(torque! body-2 (* (+ (* -0.5 kp error) 
			      (* 0.5 kd rate)) axis))
	))))

(define drive! (pd-drive 50.0 20.0))

(define (control!)
  (for (rig => behaviors) in rig-behaviors
    (for (pose . reaction!) in behaviors
      (let ((d (distance pose (arrangement #;of-the rig))))
	(when (< d 0.1)
	  (reaction! rig pose)))))

  (for (rig => pose) in rig-poses
    (for (joint-name . value) in pose
      (if value
	  (drive! (joint-named joint-name #;from rig) #;to value))))
  )

(define the-simulation (primitive-make-simulation))
(define-rig-for the-simulation
  'legs (with-input-from-file "art/rigs/legs.rig" read))

(define-rig-for the-simulation 'ground 
  (with-input-from-file "art/rigs/ground.rig" read))
(define the-ground (make-rig the-simulation 'ground #:position #f32(0 0 0.05)))


(define the-legs (make-rig #;in the-simulation #;instantiating 'legs))

(set-simulation-property! the-simulation 'gravity #f32(0 0 -0.098))

(define (combine-poses a b)
  (match-let ((('pose . pose-a) a)
	      (('pose . pose-b) b))
    `(pose ,@(replace-alist-bindings pose-a pose-b))))

(define stand (null-pose the-legs))
(define left-up 
  (combine-poses stand `(pose (left-knee . -0.5)
			      (left-pelvis . 0.5)
			      (right-ankle . #f)
			      (balance . 0.5)
			      )))
(define left-front 
  (combine-poses left-up `(pose (left-knee . -0.2)
				(left-ankle . #f)
				(right-knee . #f)
				(balance . 0.8)
				)))
(define left-straight
  (combine-poses left-front `(pose (left-knee . 0)
				   (left-ankle . 0)
				   (left-pelvis . 0)
				   (balance . 0.5)
				   (right-pelvis . 0.5))))


(define right-up
  (combine-poses stand `(pose (right-knee . 0.5)
			      (right-pelvis . -0.5)
			      (left-ankle . #f)
			      (balance . -0.5)
			      )))
(define right-front 
  (combine-poses right-up `(pose (right-knee . 0.2)
				(right-ankle . #f)
				(left-knee . #f)
			      (balance . -0.8)
				)))
(define right-straight
  (combine-poses right-front `(pose (right-knee . 0)
				   (right-ankle . 0)
				   (right-pelvis . 0)
				   (balance . -0.5)
				   (left-pelvis . -0.5))))

(define duck
  `(pose (left-pelvis . 2.2)
	 (left-knee . -2.2)
	 (left-ankle . 0.9)
	 (right-pelvis . -2.2)
	 (right-knee . 2.2)
	 (right-ankle . -0.9)))


(set! #[rig-poses the-legs] (tail stand))


#|
(keydn 1 (lambda()(set-pose! #;of the-legs #;to stand)))
(keydn 2 (lambda()(set-pose! #;of the-legs #;to duck)))
(keydn 3 (lambda()(set-pose! #;of the-legs #;to left-up)))
(keydn 4 (lambda()(set-pose! #;of the-legs #;to left-front)))
(keydn 5 (lambda()(set-pose! #;of the-legs #;to left-straight)))
(keydn 6 (lambda()(set-pose! #;of the-legs #;to right-up)))
(keydn 7 (lambda()(set-pose! #;of the-legs #;to right-front)))
(keydn 8 (lambda()(set-pose! #;of the-legs #;to right-straight)))
|#


(keydn 0 (lambda () (set-pose! #;of the-legs #;to stand)))
(keydn 1 (lambda () (set-pose! #;of the-legs #;to left-up)))
(keydn 2 (lambda () (set-pose! #;of the-legs #;to left-front)))
(keydn 3 (lambda () (set-pose! #;of the-legs #;to left-straight)))
(keydn 4 (lambda () (set-pose! #;of the-legs #;to right-up)))
(keydn 5 (lambda () (set-pose! #;of the-legs #;to right-front)))
(keydn 6 (lambda () (set-pose! #;of the-legs #;to right-straight)))
(keydn 9 (lambda () (set-pose! #;of the-legs #;to duck)))

(define stage (make <physics-stage> #:simulation the-simulation))

(define *view*
  (make <3d-editor> #:x 10 #:y 10 
	#:w (- (screen-width) 10)
	#:h (- (screen-height) 10)
	#:stage stage))

(add-child! *view* #;to *stage*)

(key 'q (lambda () (relative-twist! #[*view* 'camera] #f32(0 0 0.02))))
(key 'e (lambda () (relative-twist! #[*view* 'camera] #f32(0 0 -0.02))))
(key 'w (lambda () (relative-move! #[*view* 'camera] #f32(0 0 -0.07))))
(key 's (lambda () (relative-move! #[*view* 'camera] #f32(0 0 0.07))))
(key 'a (lambda () (relative-move! #[*view* 'camera] #f32(-0.07 0 0))))
(key 'd (lambda () (relative-move! #[*view* 'camera] #f32(0.07 0 0))))
(key 'r (lambda () (relative-move! #[*view* 'camera] #f32(0 0.07 0))))
(key 'f (lambda () (relative-move! #[*view* 'camera] #f32(0 -0.07 0))))
(key 'up (lambda () (relative-turn! #[*view* 'camera] 0 2)))
(key 'down (lambda () (relative-turn! #[*view* 'camera] 0 -2)))

(key 'left (lambda () (relative-turn! #[*view* 'camera] 2 0)))
(key 'right (lambda () (relative-turn! #[*view* 'camera] -2 0)))
(set! #[*view* 'drag] (lambda (x y dx dy)
			(relative-turn! #[*view* 'camera] (- dx) (- dy))))

(keydn 'return (lambda ()
		 (reset-behaviors! #;of the-legs)
		 (initiate-sequence!
		  (circular-list stand left-up left-front left-straight
				 stand right-up right-front right-straight)
		  the-legs)))




(define pause #f)
(keydn 'p (lambda () (set! pause (not pause))))

(add-timer! 
 25 
 (lambda()
   (unless pause
     (make-simulation-step! the-simulation)
     (control!)
   )))
