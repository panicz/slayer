#!../src/slayer -e3d
exit
!#
(set! %load-path `("." "./guile-modules" ".." "../guile-modules"
		   "/home/panicz/.guile.d" ,@%load-path))

(use-modules 
 (slayer) 
 (slayer 3d)
 (oop goops)
 (ice-9 nice-9)
 (extra common)
 (extra slayer)
 (extra 3d)
 (extra ref)
 (extra math)
 (widgets base)
 (widgets 3d))

(set-window-title! "Sweeping triangulization")

(keydn 'esc quit)

(define object (make <3d-object> #:mesh '(mesh)))

(define world (make <3d-stage>))

(add-object! object #;to world)

(define view 
  (let (((w h) (screen-size)))
    (make <3d-editor> #:x 0 #:y 0 #:w w #:h h #:stage world)))

(add-child! view #;to *stage*)

(let ((camera #[view 'camera]))
  (set! #[camera 'position] #f32(5.2 0.7 6.5)))

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

(define polygon
  '((0 0) (2 3) (5 -1) (6 0) (7 4) (8 2) (9 3) (10 0)
    (9.5 -3.5) (5.5 -2) (4 -1) (3 -2) (1 -2)))

(define (<* a b)
  ;;(assert (and (list? a) (list? b)))
  (and (not (null? b))
       (or (null? a)
	   (let (((a . a*) a)
		 ((b . b*) b))
	     (or (< a b)
		 (and (= a b)
		      (<* a* b*)))))))

(publish 
 (define (triangulize points)
   (let* (((high-chain low-chain) (chains points))
	  (low-chain* (map (lambda (vertex)`(lo . ,vertex)) low-chain))
	  (high-chain* (map (lambda (vertex) `(hi . ,vertex)) high-chain))
	  ((v1 v2 . polygon*) (merge low-chain* high-chain*
				     (lambda ((tag1 . coords1) (tag2 . coords2))
				       (<* coords1 coords2)))))
     (map (lambda (triangle)
	    (map (lambda ((tag . coords)) coords) triangle))
	  (sweep polygon* `(,v2 ,v1)))))
 where
 (define (euclid-norm v)
   (sqrt (apply + (map * v v))))
 
 (define (tagged-line #;from (_ . a) #;to (tag . b))
   (let* ((b-a (map - b a))
	  (|b-a| (euclid-norm b-a))
	  (direction (map (lambda (o) (/ o |b-a|)) b-a))
	  (acosa (dot a direction))
	  (displacement (map - a (map (lambda (o) (* acosa o)) direction))))
     `(,tag ,direction ,displacement)))

 (define ((above? tagged-line) (tag* . point))
   (let* (((tag direction displacement) tagged-line)
	  (inside? (match tag
		     ('lo positive?)
		     ('hi negative?))))
     (inside? (cross direction (map - point displacement)))))

 (define (dot a b)
   (apply + (map * a b)))

 (define (cross (ax ay) (bx by))
   (- (* ax by) (* bx ay)))

 (define (stack-prefix tag vertex vertices stack)
   (match stack
     ((first second . rest)
      (if ((above? (tagged-line #;from vertex #;to second)) first)
	  (values vertices stack)
	  (stack-prefix tag vertex `(,first . ,vertices) `(,second . ,rest))))
     ((last)
      (values `(,last . ,vertices) '()))
     (_
      (values vertices stack))))

 (define (triangles vertex stack)
   (match stack
     ((first second . rest)
      `((,vertex ,first ,second) . ,(triangles vertex `(,second . ,rest))))
     (_
      '())))

 (define (chains polygon)
   (let* ((leftmost rightmost (apply argmin+argmax (lambda ((x y)) x) polygon)))
     (if (< (list-index (lambda (x) (equal? x leftmost)) polygon)
	    (list-index (lambda (x) (equal? x rightmost)) polygon))
	 ;; ... L ... R ...
	 (let* ((prefix lower-prefix (break (lambda (x)
					      (equal? x rightmost))
					    polygon))
		(lower-suffix upper (break (lambda (x)
					     (equal? x leftmost))
					   prefix)))
	   `(,upper ,(reverse `(,@lower-prefix ,@lower-suffix))))
	 ;; ... R ... L ...
	 (let* ((prefix upper-prefix (break (lambda (x)
					      (equal? x leftmost))
					    polygon))
		(upper-suffix lower (break (lambda (x)
					     (equal? x rightmost))
					   prefix)))
	   `((,@upper-prefix ,@upper-suffix) ,(reverse lower))))))
 
 (define (sweep polygon stack)
   (match polygon
     ((last)
      (triangles last stack))
     ((first . rest)
      (let (((tag . coords) first)
	    (((tag* . _) . _) stack))
	(if (eq? tag tag*)
	    (let ((friends stack* (stack-prefix tag* first '() stack)))
	      `(,@(triangles first friends)
		,@(sweep rest `(,first ,@(match friends
					   (() '())
					   ((h . t) `(,h)))
				       ,@stack*))))
	    (let (((top . _) stack))
	      `(,@(triangles first stack)
		,@(sweep rest `(,first ,top)))))))))
 )

(define mode #f)

(define forbidden #[])

(for i in (iota 10)
  (keydn i (lambda ()
	     (set! #[forbidden i] (not #[forbidden i])))))

(keydn 'return
       (lambda ()
	 (match mode
	   ('wireframe
	    (let* ((triangles (triangulize polygon))
		   (N (length triangles))
		   (grey (lambda (i)
			   (let ((shade (exact->inexact (/ (+ i 1) (+ N 2)))))
			     (list->typed-array 'f32 1 (make-list 3 shade)))))
		   (mesh `(mesh . ,(append-map
				    (lambda (triangle index)
				      (if #[forbidden index]
					  '()
					  `((vertices ,(list->typed-array
							'f32 2 triangle))
					    (color ,(grey index))
					    (faces (triangles #u8(0 1 2))))))
				    triangles (iota N)))))
	      (set! mode 'triangles)
	      (set! #[object 'mesh] mesh)))
	   (_
	    (let ((shape `(mesh (vertices ,(list->typed-array 'f32 2 polygon))
				(color #f32(1 0 0))
				(faces (line-strip
					,(list->uniform-array
					  (iota (length polygon))))))))
	      (set! mode 'wireframe)
	      (set! #[object 'mesh] shape))))))

