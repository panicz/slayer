(define-module (extra 3d)
  :use-module (extra common)
  :use-module (extra ref)
  :use-module (oop goops)
  :use-module (extra math)
  ;;:use-module (extra shape)
  ;;:duplicates (warn merge-generics); replace warn-override-core warn last)
  :export (
	   <3d> 
	   <3d-cam> 
	   <3d-shape>
	   <3d-mesh>
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
	   transform-mesh-vertices
	   )
  ;;:re-export (distance)
  )

;(use-modules (extra common) (extra math))


;; #;(describe (normals/triangles array? list?) array?)

;; #;(expect 
;;  (equal? (normals/triangles #2f32((0 0 1) (0 1 0) (0 -1 0)) '(0 1 2))
;; 	 #2f32((-1.0 -0.0 -0.0)(-1.0 -0.0 -0.0)(-1.0 -0.0 -0.0))))

;; (define (normals/triangles vertices triangle-indices/list)
;;   (let* ((3d-list (match-lambda ((x y)       (list x y .0))
;; 		                ((x y z . _) (list x y z))))
;; 	 (vertices (case (array-rank vertices)
;; 		     ((1)  (map-n 3 list (array->list vertices)))
;; 		     ((2)  (map 3d-list (array->list vertices)))
;; 		     (else (throw 'unsupported-array-rank vertices))))
;; 	 (normals (make-vector (length vertices) '())))
;;     (for indices in (map-n 3 list triangle-indices/list)
;; 	 (match-let (((v1 v2 v3) 
;; 		      (map (lambda(i)(list-ref vertices i)) indices)))
;; 	   (let ((normal (normalized (^ (- v2 v1) (- v3 v2)))))
;; 	     (for i in indices
;; 		  (vector-set! normals i 
;; 			       (cons normal 
;; 				     (vector-ref normals i)))))))
;;     (for i in 0 .. (last-index normals)
;; 	 (let ((vertex-normals (vector-ref normals i)))
;; 	       (vector-set! normals i 
;; 			    (if (null? vertex-normals)
;; 				'(.0 .0 .0)
;; 				(normalized (apply + vertex-normals))))))
;;     (list->typed-array 'f32 2 (vector->list normals))))

;; #;(transform (v0 v1 v2 v3 v4 ...) ((v0 v1 v2)(v2 v1 v3)(v2 v3 v4) ...))
;; #;(transform (v0 v1 v2 v3 v4 v5 v6 v7 ...)
;;              ((v0 v1 v2)(v1 v2 v3)(v4 v5 v6)(v5 v6 v7)))

;; #;(transform (v0 v1 v2 v3 v4 ...)
;; 	     ((v0 v1 v2)(v0 v2 v3)(v0 v3 v4) ...))

;; (define (indices->list indices) 
;;   (flatten (array->list indices)))

;; (define* (triangle-strip->triangles indices/flat-list #:optional (swap #f))
;;   (match indices/flat-list
;;     ((v0 v1 v2 . _)
;;      (cons (if swap (list v1 v0 v2) (list v0 v1 v2)) 
;; 	   (triangle-strip->triangles (drop indices/flat-list 1) (not swap))))
;;     (else '())))

;; (define (triangle-fan->triangles indices/flat-list)
;;   (match-let (((pivot . edges) indices/flat-list))
;;     (map (lambda (second third) (list pivot second third))
;; 	 (drop-right edges 1) (drop edges 1))))

;; (define (quads->triangles indices/flat-list)
;;   (match indices/flat-list
;;     ((a b c d . rest)
;;      (cons (list a b c) (cons (list c a d) (quads->triangles rest))))
;;     (else
;;      '())))

;; (define (quad-strip->quads indices/flat-list)
;;   (match indices/flat-list
;;     ((a b c d . _)
;;      (cons (list a b c d) (quad-strip->quads (drop indices/flat-list 2))))
;;     (else
;;      '())))

;; (define (quad-strip->triangles indices/flat-list)
;;   (quads->triangles (flatten (quad-strip->quads indices/flat-list))))

;; (define (triangle-indices type indices/list)
;;   (case type
;;     ((triangles) (flatten indices/list))
;;     ((triangle-strip) (triangle-strip->triangles (flatten indices/list)))
;;     ((triangle-fan polygon) (triangle-fan->triangles (flatten indices/list)))
;;     ((quads) (quads->triangles (flatten indices/list)))
;;     ((quad-strip) (quad-strip->triangles (flatten indices/list)))))

;; (define (split-into-display-units mesh-definition)
;;   "Each display unit contains one ``vertices'' section and typically 
;;  at least one ``faces'' section (besides some other stuff)"
;;   (let loop ((definition mesh-definition)
;; 	     (current-display-unit '())
;; 	     (finished-display-units '())
;; 	     (already-had-faces #f))
;;     (match definition
;;       (()
;;        (reverse (cons `(,(reverse current-display-unit))
;; 		      finished-display-units)))
;;       ((((? symbol? symbol) args ...) . rest)
;;        (if (and (eq? symbol 'vertices) already-had-faces)
;; 	   (loop rest `((,symbol ,@args))
;; 		 (cons `(,(reverse current-display-unit))
;; 		       finished-display-units)
;; 		 #t)
;; 	   (loop rest (cons `(,symbol ,@args) current-display-unit)
;; 		 finished-display-units 
;; 		 (or already-had-faces (eq? symbol 'faces))))))))

;; (define (insert-normals display-unit)
;;   (and-let* ((vertices (filter-map (match-lambda 
;; 				     (('vertices . vertices)
;; 				      vertices)
;; 				     (else #f))
;; 				   display-unit))
;; 	     ((= (length vertices) 1))
;; 	     (vertices (first vertices))
;; 	     ((<< "vertices: "vertices))
;; 	     (faces (append-map (match-lambda ((type indices)
;; 					       (triangle-indices 
;; 						type 
;; 						(indices->list indices))))
;; 				(filter-map (match-lambda (('faces . faces) faces) 
;; 					      (else #f))
;; 					    display-unit)))
;; 	     ((<<"faces: " faces))
;; 	     (normals (list->uniform-array (normals/triangles vertices faces))))
;;     (append-map (lambda (entry) (match entry
;; 				  (('vertices . _)
;; 				   (list entry `(normals ,normals)))
;; 				  (else
;; 				   (list entry))))
;; 		display-unit)))

;; (define (mesh-with-normals mesh)
;;   (match mesh
;;     (('mesh . definition)
;;      (if (any (matches? ('normals . _)) definition)
;; 	 mesh ; already has normals, so we don't want to interfere
;; 	 `(mesh ,@(let ((display-units (split-into-display-units definition)))
;; 		    (apply append
;; 			   (append-map insert-normals
;; 				       display-units))))))))


;; (mesh-with-normals (generate-hemisphere #:slices 2 #:stacks 2))


;; (mesh 
;;  (vertices #2f32((-1.0 1.22460635382238e-16 0.0) 
;; 		 (1.0 0.0 0.0) 
;; 		 (-0.866025388240814 1.06054024092951e-16 0.5) 
;; 		 (0.866025388240814 0.0 0.5) 
;; 		 (0.0 0.0 1.0))) 
;;  (colors #2f32((-1.0 1.22460635382238e-16 0.0) 
;; 	       (1.0 0.0 0.0) 
;; 	       (-0.866025388240814 1.06054024092951e-16 0.5) 
;; 	       (0.866025388240814 0.0 0.5) 
;; 	       (0.0 0.0 1.0))) 
;;  (faces (quad-strip #u8(0 2 1 3 0 2)) (triangle-fan #u8(4 3 2 1 0))))

;; ((((vertices #2f32((-1.0 1.22460635382238e-16 0.0) 
;; 		   (1.0 0.0 0.0) 
;; 		   (-0.866025388240814 1.06054024092951e-16 0.5) 
;; 		   (0.866025388240814 0.0 0.5) (0.0 0.0 1.0))) 
;;    (colors #2f32((-1.0 1.22460635382238e-16 0.0) (1.0 0.0 0.0)
;; 		 (-0.866025388240814 1.06054024092951e-16 0.5)
;; 		 (0.866025388240814 0.0 0.5) (0.0 0.0 1.0))) 
;;    (faces (quad-strip #u8(0 2 1 3 0 2)) (triangle-fan #u8(4 3 2 1 0))))))


;; (mesh 
;;  (vertices #2f32((-1.0 1.22460635382238e-16 0.0) 
;; 		 (1.0 0.0 0.0) 
;; 		 (-0.866025388240814 1.06054024092951e-16 0.5) 
;; 		 (0.866025388240814 0.0 0.5) 
;; 		 (0.0 0.0 1.0))) 
;;  (colors #2f32((-1.0 1.22460635382238e-16 0.0) 
;; 	       (1.0 0.0 0.0) 
;; 	       (-0.866025388240814 1.06054024092951e-16 0.5) 
;; 	       (0.866025388240814 0.0 0.5) 
;; 	       (0.0 0.0 1.0))) 
;;  (faces 
;;   (quad-strip #u8(0 2 1 3 0 2)) 
;;   (triangle-fan #u8(4 3 2 1 0))))

(define-syntax define-symmetric-method
  (syntax-rules ()
    ((_ (name arg1 arg2) body ...)
     (begin
       (define-method (name arg1 arg2) body ...)
       (define-method (name arg2 arg1) body ...)))))

(define (transform-mesh-vertices proc mesh)
  (match mesh
    (('mesh . definition)
     `(mesh
       ,@(map (match-lambda
		  (('vertices vertices)
		   `(vertices ,(proc vertices)))
		(else
		 else))
	      definition)))))

(define-class <3d> ()
  (position #:init-value #f32(0 0 0) #:init-keyword #:position)
  (orientation #:init-value '(0.0 . #f32(1 0 0)) #:init-keyword #:orientation))

(define-method (distance (a <3d>) (b <3d>))
  (distance #[a 'position] #[b 'position]))

(define-class <3d-cam> (<3d>)
  (fovy #:init-value 70.0))

(define-class <3d-shape> (<3d>)
  (shape #:init-value '()))

(define-class <3d-mesh> (<3d-shape>)
  (mesh #:init-value '()
	#;(\ with-input-from-file "3d/cube.3d" read)))

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
	(color #f32(0 0.5 0))
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
				    '((0.0 0.0 1.0))))))
		  (normals ,(list->typed-array
			     'f32 2 (append
				     vertices 
				     '((0.0 0.0 1.0)))))
		  (colors ,(list->typed-array 
			      'f32 2 (append
				      vertices 
				      '((0.0 0.0 1.0)))))
		  (faces
		   (quad-strip ,(list->uniform-array faces))
		   (triangle-fan
		    ,(list->uniform-array
		      (iota (+ slices 3)
			    (* slices stacks) 
			    -1)))))))))))

(generate-hemisphere #:radius 1.0 #:slices 2 #:stacks 2)

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
      (colors
       #2f32((0.5 0.7 0.2 1.0)
	     (0.7 0.5 0.2 0.8)
	     (0.2 0.7 0.5 0.7)
	     (0.7 0.2 0.5 0.6)
	     (0.5 0.2 0.7 0.5)
	     (0.2 0.5 0.7 0.3)
	     (0.7 0.7 0.2 0.2)
	     (1.0 1.0 1.0 0.0)))
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
		      ('colors colors) 
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
	     (colors ,(array-append colors colors))
	     (light #:position #f #:direction #f32(0 0 1)
		    #:ambient #f32(1 1 1 1))
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

#;(define (projection-matrix fovy aspect near far)
  (and-let* (( (not (= aspect 0)))
	     (radians (* fovy 0.5 pi 1/180))
	     (sine (sin radians))
	     ( (not (= sine 0)))
	     (depth (- far near))
	     ( (not (= depth 0)))
	     (cotangent (/ (cos radians) sine)))
    (list->uniform-array
     (apply 
      map list ;i.e. transpose
      `((,(/ cotangent aspect) 0          0                             0)
	(0                     ,cotangent 0                             0)
	(0                     0          ,(- (/ (+ near far) depth))  -1)
	(0                     0          ,(* -2 near far (/ 1 depth))  0))))))  

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
	    (colors ,(list->typed-array 
		      'f32 2 (append 
			      (make-list base-points '(1 0 0))
			      (make-list base-points '(0 0 1)))))
	    (faces (quads ,(list->uniform-array faces)))))))))
