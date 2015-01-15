
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
      (right '(0.5 . #f32(0.5 0.5 0.5)))
      (left '(-0.5 . #f32(-0.5 0.5 0.5)))
      (ahead '(0.707 . #f32(0.707 0 0)))
      (back '(0.707 . #f32(-0.707 0 0)))
      (camera #[view 'camera]))
  (let-syntax ((look (syntax-rules ()
		       ((_ direction)
			(begin 
			  (format #t "looking ~s\n" 'direction) 
			  (set! #[camera 'orientation] direction))))))
    (keydn 1
      (lambda _  (if (modifier-pressed? 'shift) 
		(look back)
		(look ahead))))
    (keydn 2
      (lambda _ (if (modifier-pressed? 'shift) 
	       (look left)
	       (look right))))
    (keydn 3
      (lambda _ (if (modifier-pressed? 'shift) 
	       (look down)
	       (look up))))))

(keydn 0 (lambda _ (<< #[view : 'camera : 'orientation])))
(keydn 9 (lambda _ (<< #[view : 'camera : 'position])))
