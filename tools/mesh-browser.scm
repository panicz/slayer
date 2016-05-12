#!../src/slayer -e3d
!#
(use-modules 
 (slayer) 
 (slayer 3d)
 (oop goops)
 (extra common)
 (extra slayer)
 (extra 3d)
 (extra ref)
 (widgets base)
 (widgets 3d))

(keydn 'esc quit)

(define *directory* (if (defined? '$1) $1 "./"))

(define *meshes* 
  (map (lambda (name)
	 (string-append *directory* name))
       (filter (string-matches "\\.3d$") (list-directory *directory*))))

(define *mesh-index* 0)

(if (<= (length *meshes*) 0)
    (die "no meshes found\n"))

(define object (make <3d-object> #:mesh '(mesh)))

(define world (make <3d-stage>))

(add-object! object #;to world)

(define view 
  (let (((w h) (screen-size)))
    (make <3d-editor> #:x 0 #:y 0 #:w w #:h h #:stage world)))

(add-child! view #;to *stage*)

(define (show-mesh! i)
  (set! *mesh-index* (modulo i (length *meshes*)))
  (and-let* ((mesh-name (list-ref *meshes* *mesh-index*))
	     (mesh (with-input-from-file mesh-name read)))
    (set-window-title! mesh-name)
    (set! #[object 'mesh] mesh)))

(show-mesh! 0)

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


(keydn "[" (lambda () (show-mesh! (+ *mesh-index* 1))))
(keydn "]" (lambda () (show-mesh! (- *mesh-index* 1))))
