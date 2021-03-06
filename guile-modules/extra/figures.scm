(define-module (extra figures)
  :use-module (extra common)
  :use-module (extra ref)
  :use-module (extra math)
  :export (
	   generate-circle
	   generate-wheel
	   generate-hemisphere
	   generate-capsule
	   generate-sphere
	   generate-box
	   generate-mesh
	   square-mesh
	   rectangle-grid
	   square-grid
	   hemisphere
	   generate-tube
	   )
  )

(define* (generate-circle #:key (radius 1.0) (points 20))
  (let ((slice (/ 2pi points)))
    (let loop ((vertices '())
	       (i 0))
      (if (< i points)
	  (loop (cons (make-polar radius (* slice i)) vertices)
		(1+ i))
	  (let ((vertices (map (lambda(c) (list (real-part c)
						(imag-part c)))
			       vertices))
		(faces (iota points)))
	    `(mesh
	      (vertices ,(list->uniform-array vertices))
	      (faces (line-loop 
		      ,(list->uniform-array faces)))))))))

(define* (generate-wheel #:key (radius 1.0) (points 20))
  (match (generate-circle #:radius radius #:points points)
    (('mesh
      ('vertices vertices)
      ('faces ('line-loop faces)))
     (let ((vertices (array->list vertices))
	   (faces (array->list faces)))
       `(mesh
	 (vertices ,(list->uniform-array (cons '(0 0) vertices)))
	 (faces (triangle-fan ,(list->uniform-array 
				(append faces (list points 1))))))))))

(define* (rectangle-mesh #:key (width-start -1.0) (width-end 1.0)
			 (width-points 10) (height-start -1.0) 
			 (height-end 1.0) (height-points 10))
  (let ((width (- width-end width-start))
	(height (- height-end height-start))
	(points (* width-points height-points)))
    (let ((mesh 
	   (cart 
	    (iota width-points width-start 
		  (/ width (1- width-points)))
	    (iota height-points height-start 
		  (/ height (1- height-points)))
	    '(0.0)))
	  (faces
	   (map (match-lambda ((x y)
			       (let* ((w width-points)
				      (n (+ x (* y w))))
				 (list n (+ n w) (+ n w 1) (+ n 1)))))
		(cart (iota (1- width-points)) 
		      (iota (1- height-points))))))
      `(mesh
	(vertices
	 ,(list->typed-array 'f32 2 mesh))
	(color #f32(0 1 0))
	(faces
	 (points ,(list->uniform-array faces)))))))

(define* (rectangle-grid #:key (width-start -1.0) (width-end 1.0)
			 (width-points 10) (height-start -1.0) 
			 (height-end 1.0) (height-points 10))
  (let ((w (- width-end width-start))
	(h (- height-end height-start)))
    (let ((ver (iota width-points width-start (/ w (1- width-points))))
	  (hor (iota height-points height-start (/ h (1- height-points)))))
      `(mesh
	(vertices
	 ,(list->uniform-array
	   (append
	    (append-map (lambda(x)`((,x ,height-start 0)(,x ,height-end 0))) ver)
	    (append-map (lambda(y)`((,width-start ,y 0)(,width-end ,y 0))) hor)
	    )))
	(color #f32(0.3 0.3 0.3))
	(faces (lines ,(list->uniform-array 
			(iota (* 2 (+ width-points height-points))))))))))

(define* (square-grid #:key (size 1.0) (density 10))
  (let ((start (exact->inexact(/ size -2)))
	(end (exact->inexact (/ size 2))))
    (rectangle-grid #:width-start start #:width-end end 
		    #:width-points density
		    #:height-start start #:height-end end 
		    #:height-points density)))

(define* (square-mesh #:key (size 1.0) (density 10))
  (let ((start (exact->inexact (/ size -2)))
	(end (exact->inexact (/ size 2))))
    (rectangle-mesh #:width-start start #:width-end end 
		    #:width-points density
		    #:height-start start #:height-end end 
		    #:height-points density)))

(define* (hemisphere #:key (r 1.0) (density 12))
  (let ((domain (square-mesh #:size (* 2 r) #:density density))
	(r^2 (* r r)))
    (match domain
      (('mesh ('vertices vertices)('faces ('quads faces)))
       (let* ((v (list->vector
		  (map (match-lambda
			   ((x y z) 
			    (let ((x^2 (* x x))
				  (y^2 (* y y)))
			      (list x y (sqrt (- r^2 x^2 y^2))))))
		       (array->list vertices))))
	      (remove-list (filter (lambda(i)
				     (not (real? (third #[v i]))))
				   (iota (vector-length v))))
	      (new-faces (filter (lambda(indices)
				   (every 
				    (?not (\ in? _ remove-list)) 
				    indices))
				 (array->list faces))))
	 (array-index-map! v (lambda (i)
			       (match-let (((x y z) #[v i]))
				 (if (in? i remove-list)
				     (list x y 0.0)
				     (list x y z)))))
	 
	 `(mesh
	   (vertices ,(list->typed-array 'f32 2 (vector->list v)))
	   (color #f32(1 1 1 0.5))
	   (faces (quads ,(list->uniform-array new-faces)))))))))

(define* (generate-hemisphere #:key(radius 1.0)(slices 20)(stacks 8))
  (match-let ((('mesh ('vertices circle-vertices) _ ...)
	       (generate-circle #;(:radius radius) #:points slices)))
    (let* ((stack-height (/ 1.0 stacks))
	   (width-factor (lambda(h)
			  (sqrt (- 1.0 (* h h)))))
	   (circle-vertices (array->list circle-vertices)))
      (let loop ((stack 1)
		 (vertices (map (\ append _ '(0.0)) circle-vertices))
		 (faces '()))
	(cond ((< stack stacks)
	       (let ((new-vertices 
		      (let* ((z (* stack stack-height))
			     (scale (width-factor z))
			     (scale-circle
			      (lambda (v2)
				(append (map (\ * scale _) v2) 
					`(,z)))))
			(map scale-circle circle-vertices)))
		     (new-faces 
		      (append-map (lambda(n)
				    (let ((next (modulo (+ n 1) 
							slices))
					  (shift 
					   (* (1- stack) slices)))
				      (map (\ + shift _)
					   (list n 
						 (+ n slices)))))
				  (iota slices))))
		 (loop (1+ stack) (append vertices new-vertices)
		       (append faces 
			       new-faces 
			       (take new-faces 2)))))
	       (else ; (= stack stacks)
		`(mesh
		  (vertices ,(list->typed-array 
			      'f32 2 
			      (map (\ map (\ * radius _) _)
				   (append
				    vertices 
				    '((0.0 0.0 1.0))))))
		  (normals ,(list->typed-array
			     'f32 2 (append
				     vertices 
				     '((0.0 0.0 1.0)))))
		  (color #f32(1 1 1 0.5))
		  (faces
		   (quad-strip ,(list->uniform-array faces))
		   (triangle-fan
		    ,(list->uniform-array
		      (iota (+ slices 3)
			    (* slices stacks) 
			    -1)))))))))))

(define* (generate-box #:key (x 1.0) (y 1.0) (z 1.0))
  (match-let* (((x y z) (map (\ * _ 0.5) (list x y z)))
	       ((-x -y -z) (map - (list x y z))))
    `(mesh
      (vertices
       ,(list->typed-array
	 'f32 2
	 `(( ,x  ,y  ,z)
	   ( ,x  ,y ,-z)
	   ( ,x ,-y  ,z)
	   ( ,x ,-y ,-z)
	   (,-x  ,y  ,z)
	   (,-x  ,y ,-z)
	   (,-x ,-y  ,z)
	   (,-x ,-y ,-z))))
      (color #f32(1 1 1 0.5))
      (faces
       (quads 
	#2u8((0 1 3 2)
	     (0 2 6 4)
	     (0 1 5 4)
	     (7 6 4 5)
	     (7 5 1 3)
	     (7 6 2 3)))))))

(define* (generate-capsule #:key (points 20) (stacks 10)
			   (height 1.0) (radius 0.2))
  (match-let ((('mesh ('vertices vertices) 
		      ('normals normals)
		      ('color color) 
		      ('faces ('quad-strip quad-strip)
			      ('triangle-fan triangle-fan)))
	      (generate-hemisphere #:radius radius #:slices points
				   #:stacks stacks)))
    (let* ((top (map (match-lambda((x y z)
				   (list x y (+ (/ height 2) z)))) 
		     (array->list vertices)))
	   (bottom 
	    (map (match-lambda((x y z)
			       (list x y (- 0 (/ height 2) z))))
		 top))
	   (new-vertices (list->typed-array
			  'f32 2
			  (append top bottom)))
	   (top-size (apply max (array->list triangle-fan)))
	   (bottom-strip (map (\ + 1 top-size _)
			      (array->list quad-strip)))
	   (bottom-fan (map (\ + 1 top-size _)
			    (array->list triangle-fan))))
      `(mesh (vertices ,new-vertices)
	     (normals ,(list->typed-array 
			'f32 2 
			(map normalized (array->list new-vertices))))
	     (color #f32(1 1 1 0.5))
	     (faces (quad-strip ,quad-strip)
		    (triangle-fan ,triangle-fan)
		    (quad-strip 
		     ,(list->uniform-array bottom-strip))
		    (quads
		     ,(list->uniform-array
		       (map (lambda(n)
			      (let ((next (modulo (1+ n) points)))
				(list n (+ n top-size 1)
				      (+ next top-size 1) next)))
				(iota points))))
		    (triangle-fan 
		     ,(list->uniform-array bottom-fan)))))))

(define* (generate-sphere #:key (radius 1.0) (stacks 20) (points 20))
  (generate-capsule #:points points #:stacks stacks #:radius radius
		    #:height 0.0))

(define* (generate-tube #:key (radius 0.5) (height 1.0)
				 (base-points 20))
  (match-let ((('mesh ('vertices circle-vertices) _ ...)
	       (generate-circle #:radius radius 
				#:points base-points)))
    (let ((circle (array->list circle-vertices)))
      (let ((bottom (map (\ append _ (list 0.0)) circle))
	    (top (map (\ append _ (list height)) circle)))
	(let ((faces (map (lambda(n)
			    (let ((next (modulo (+ n 1) base-points)))
			      (list 
			       n (+ n base-points)
			       (+ next base-points) next)))
			    (iota base-points))))
	  `(mesh
	    (vertices ,(list->typed-array 
			'f32 2 (append bottom top)))
	    (color #f32(1 1 1 0.5))
	    (faces (quads ,(list->uniform-array faces)))))))))
