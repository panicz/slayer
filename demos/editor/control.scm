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
      (set! #[muscles joint] (create-muscle joint)))
    (set! #[rig-muscles rig] muscles)))

(define (attach-pid-muscles-to-all-joints! #;in rig #;with-parameters kp ki kd)
  (attach-muscles-to-rig! 
   rig
   (lambda (joint)
     (let ((the (lambda (property) (joint-property joint property)))
	   (error-integral 0.0))
       (lambda (desired-position)
	 (let ((axis (the 'axis))
	       (angle (the 'angle))
	       (rate (the 'angle-rate))
	       (body-1 (the 'body-1))
	       (body-2 (the 'body-2)))
	   (let ((error (- desired-position angle)))
	     (increase! error-integral error)
	     (torque! body-1 (* (+ (* 0.5 kp error)
				   (* 0.5 ki error-integral)
				   (* 0.5 kd rate)) axis))
	     (torque! body-2 (* (+ (* -0.5 kp error)
				   (* -0.5 ki error-integral)
				   (* -0.5 kd rate)) axis))
	     )))))))

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
    (specify-pose! #;of rig #;to (first #;in sequence))))

(define (freeze! rig)
  (let ((pose (pose #;of rig)))
    (reset-behaviors! #;of rig)
    (specify-pose! #;of rig #;to pose)))

(define ((pd-drive kp kd) joint desired-value)
  (let ((the (lambda (property) (joint-property joint property))))
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

(define ((pd-drive kp kd) joint desired-value)
  (let ((the (lambda (property) (joint-property joint property))))
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

(define (control!)
  (for (rig => behaviors) in rig-behaviors
    (for (trigger-pose . reaction!) in behaviors
      (let ((d (pose-distance trigger-pose (pose #;of rig))))
	(when (< d 0.2)
	  (reaction! rig pose)))))

  (for (rig => rig-pose) in rig-poses
    (let ((muscles #[rig-muscles rig]))
      (<< (pose-distance `(pose ,@rig-pose) (pose #;of rig)))
      (for (joint-name . value) in rig-pose
	(let* ((joint (joint-named joint-name #;from rig))
	       (muscle-joint! #[muscles joint]))
	  (when value
	    (muscle-joint! #;to value))))
      )))
