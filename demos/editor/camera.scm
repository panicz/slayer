(define-module (editor camera)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (scum physics)
  #:export (look ahead back up down left right))

(define down `(,(normalized '(1.0 . #f32(0 0 0))) . #f32(0 0 7)))
(define up `(,(normalized '(0.0 . #f32(0 -1 0))) . #f32(0 0 -7)))
(define left `(,(normalized '(1.0 . #f32(1 1 1))) . #f32(7 0 0)))
(define right `(,(normalized '(-1.0 . #f32(-1 1 1))) . #f32(-7 0 0)))
(define ahead `(,(normalized '(1.0 . #f32(1 0 0))) . #f32(0 -7 0)))
(define back `(,(normalized '(0.0 . #f32(0 1 1))) . #f32(0 7 0)))

(define ((look #;towards direction #;relatve-to rig #;using camera) . _)
  (let (((rotation . displacement) direction)
	(center (rig-mass-center rig)))
    (set! #[camera 'orientation] rotation)
    (set! #[camera 'position] (+ center displacement))))
