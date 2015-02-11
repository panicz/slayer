
(let ((camera #[view 'camera]))
  (set! #[camera 'position] #f32(0 -6 -0.7))
  (set! #[camera 'orientation] (normalized '(1.0 . #f32(1 0 0)))))

(key 'q (lambda () (relative-twist! #[view 'camera] #f32(0 0 0.02))))
(key 'e (lambda () (relative-twist! #[view 'camera] #f32(0 0 -0.02))))
(key 'w (lambda () (relative-move! #[view 'camera] #f32(0 0 -0.07))))
(key 's (lambda () (relative-move! #[view 'camera] #f32(0 0 0.07))))
(key 'a (lambda () (relative-move! #[view 'camera] #f32(-0.07 0 0))))
(key 'd (lambda () (relative-move! #[view 'camera] #f32(0.07 0 0))))
(key 'r (lambda () (relative-move! #[view 'camera] #f32(0 0.07 0))))
(key 'f (lambda () (relative-move! #[view 'camera] #f32(0 -0.07 0))))
(key 'up (lambda () (relative-turn! #[view 'camera] 0 2)))
(key 'down (lambda () (relative-turn! #[view 'camera] 0 -2)))
(key 'left (lambda () (relative-turn! #[view 'camera] 2 0)))
(key 'right (lambda () (relative-turn! #[view 'camera] -2 0)))

(set! #[view 'drag] (lambda (x y dx dy)
		      (relative-turn! #[view 'camera] (- dx) (- dy))))

(let ((down '(1.0 . #f32(0 0 0)))
      (up '(0.0 . #f32(0 -1 0)))
      (left (normalized '(1.0 . #f32(1 1 1))))
      (right (normalized '(-1.0 . #f32(-1 1 1))))
      (ahead (normalized '(1.0 . #f32(1 0 0))))
      (back (normalized '(0.0 . #f32(0 1 1))))
      (camera #[view 'camera]))
  (define (look #;towards direction #;from position)
    (set! #[camera 'position] position)
    (set! #[camera 'orientation] direction))
  (keydn 1
    (lambda _  
      (let ((center (rig-mass-center the-rig)))
	(if (modifier-pressed? 'shift) 
	    (look back #;from (+ center #f32(0 7 0)))
	    (look ahead #;from (- center #f32(0 7 0)))))))
  (keydn 2
    (lambda _
      (let ((center (rig-mass-center the-rig)))
	(if (modifier-pressed? 'shift) 
	    (look right #;from (- center #f32(7 0 0)))
	    (look left #;from (+ center #f32(7 0 0)))))))
  (keydn 3
    (lambda _ 
      (let ((center (rig-mass-center the-rig)))
	(if (modifier-pressed? 'shift) 
	    (look up #;from (- center #f32(0 0 7)))
	    (look down #;from (+ center #f32(0 0 7))))))))

(keydn 0 (lambda _ (<< #[view : 'camera : 'orientation])))
(keydn 9 (lambda _ (<< #[view : 'camera : 'position])))
