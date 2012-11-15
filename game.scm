(define-class <goose> ()
  (owners #:init-value #f #:init-keyword #:owners)
  (private-slots #:init-value '())
  (id #:init-thunk gensym))

(define-class <player> (<goose> <3d>) 
  (health #:init-value 100.0)
  (reload-time-left #:init-value 0)
  (velocity #:init-thunk 
	    (lambda()(random-array #:type 'f32 #:range 0.001 3)))
  (angular-velocity #:init-value #f32(0 0 0)))

(define-method (jump! (p <player>))
  (increase! #[p 'position] #(0 1 0)))

(define-method (shoot! (p <player>))
  (set! #[p 'reload-time-left] 20))

(define-method (turn! (p <player>))
  (increase! #[p 'position] #(0 1 0)))

(define-method (crouch! (p <player>))
  (set! #[p 'reload-time-left] 20))

(define-method (walk! (p <player>))
  (set! #[p 'reload-time-left] 20))

(define-method (update! (p <player>))
  (increase! #[p 'position] #[p 'velocity]))

(define-method (spawn-object! world player)
  (hash-set! world #[player 'id] player))
