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
	     (scum physics)
	     (editor poses))

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

(include "temporary-poses.scm")

(set! #[rig-poses the-legs] (tail stand))

(define stage (make <physics-stage> #:simulation the-simulation))

(define view
  (make <3d-editor> #:x 10 #:y 10 
	#:w (- (screen-width) 10)
	#:h (- (screen-height) 10)
	#:stage stage))

(add-child! view #;to *stage*)

(include "config.scm")

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
