(define-module (extra 3d)
  :use-module (extra common)
  :use-module (extra ref)
  :use-module (extra oop)
  :use-module (extra math)
  :use-module (extra shape)
  ;;:duplicates (warn merge-generics); replace warn-override-core warn last)
  :export (
	   <3d> 
	   <3d-cam> 
	   <3d-shape>
	   <3d-mesh>
	   generate-circle
	   generate-hemisphere
	   generate-capsule
	   hemisphere
	   generate-open-cylinder
	   )
  :re-export (distance)
  )

(define-syntax define-symmetric-method
  (syntax-rules ()
    ((_ (name arg1 arg2) body ...)
     (begin
       (define-method (name arg1 arg2) body ...)
       (define-method (name arg2 arg1) body ...)))))

(define-class <3d> ()
  (position #:init-value #f32(0 0 0))
  (orientation #:init-value '(0 . #f32(1 0 0))))

(define-method (distance (a <3d>) (b <3d>))
  (distance #[a 'position] #[b 'position]))

(define-class <3d-cam> (<3d>)
  (fovy #:init-value 70.0))

(define-class <3d-shape> (<3d>)
  (shape #:init-value (make <sphere>)))

(define-method (distance (a <3d-shape>) (b <3d-shape>))
  (distance (translated (rotated #[a 'shape] #[a 'orientation]) #[a 'position])
	    (translated (rotated #[b 'shape] #[b 'orientation]) #[b 'position])))

(define-symmetric-method (distance (a <3d>) (b <3d-shape>))
  (distance #[a 'position] (translated (rotated #[b 'shape] #[b 'orientation])
				       #[b 'position])))

(define-class <3d-mesh> (<3d-shape>)
  (mesh #:init-value '() 
	#;(\ with-input-from-file "3d/cube.3d" read)))

(define* (generate-circle #:key (radius 1.0) (points 20))
  (let ((slice (/ 2π points)))
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
	      (vertices ,(list->typed-array 'f32 2 vertices))
	      (faces (line-loop 
		      ,(list->typed-array 'u8 1 faces)))))))))

(define (quad-strip->quads lst)
  (throw 'not-implemented))

(define (quads->quad-strip lst)
  (throw 'not-implemented))

(define* (rectangle-mesh 
	  #:key (width-start -1.0) (width-end 1.0) (width-points 10)
	  (height-start -1.0) (height-end 1.0) (height-points 10))
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
	(faces
	 (quads ,(list->uniform-array faces)))))))

(define* (square-mesh #:key (size 1.0) (density 10))
  (let ((start (/ size -2))
	(end (/ size 2)))
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
	   (colors ,(list->typed-array 'f32 2 (vector->list v)))
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
				    `((0.0 0.0 ,1.0))))))
		  (colors ,(list->typed-array 
			      'f32 2 (append
				      vertices 
				      `((0.0 0.0 ,1.0)))))
		  (faces
		   (quad-strip ,(list->typed-array
				 'u8 1 faces))
		   (triangle-fan
		    ,(list->typed-array
		      'u8 1
		      (iota (+ slices 3) 
			    (* slices stacks) 
			    -1)))))))))))

(define* (generate-sphere #:key (radius 1.0) (stacks 20) (points 20))
  (throw 'not-implemented))

(define* (generate-capsule #:key (points 20) (stacks 10)
			   (height 1.0) (radius 0.2))
  (match-let ((('mesh ('vertices vertices) ('colors colors) 
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
	     (colors ,(array-append colors colors))
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

(define* (generate-open-cylinder #:key (radius 0.5) (height 1.0)
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
	    (colors ,(list->typed-array 
		      'f32 2 (append 
			      (make-list base-points '(1 0 0))
			      (make-list base-points '(0 0 1)))))
	    (faces (quads ,(list->typed-array 'u8 2 faces)))))))))
