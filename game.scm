(use-modules (extra math) (extra 3d) (extra goose))

(define-class <player> (<goose> <3d-mesh>) 
  (client-slots #:init-value '(mesh))
  (health #:init-value 100.0)
  (reload-time-left #:init-value 0)
  (velocity #:init-thunk 
	    (\ random-array #:type 'f32 #:range 0.1 3))
  (angular-velocity #:init-value #f32(0 0 0)))

(define-method (jump! (p <player>))
  (increase! #[p 'position] #(0 1 0)))

(define-method (shoot! (p <player>))
  (set! #[p 'reload-time-left] 20))

(define-method (turn! (p <player>) degs)
  (increase! #[p 'position] #(0 1 0)))

(define-method (crouch! (p <player>))
  (set! #[p 'reload-time-left] 20))

(define-method (walk! (p <player>))
  (set! #[p 'reload-time-left] 20))

(define-method (update! (p <player>))
  (if (> #[p 'reload-time-left] 0)
      (decrease! #[p 'reload-time-left]))
  (increase! #[p 'position] #[p 'velocity]))

(define-method (spawn-object! world player)
  (hash-set! world #[player 'id] player))
