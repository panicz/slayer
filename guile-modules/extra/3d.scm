(define-module (extra 3d)
  :use-module (extra common)
  :use-module (extra ref)
  :use-module (oop goops)
  :use-module (extra math)
  :use-module (slayer)
  :use-module (slayer 3d)
  ;;:use-module (extra shape)
  ;;:duplicates (warn merge-generics); replace warn-override-core warn last)
  :export (
	   <3d> 
	   <3d-cam> 
	   <3d-model>
	   make-light
	   transform-matrix!
	   draw-mesh!
	   draw-model!
	   draw-contour!
	   setup-lights!
	   extract-lights
	   skip-lights
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
  (fovy #:init-value 70.0)
  (light #:init-thunk (lambda()(make-light #:ambient #f32(0.3 0.3 0.3 0.3)))))

(define-class <3d-model> (<3d>)
  (%mesh #:init-value '(mesh))
  (%lights #:init-value '(mesh))
  (mesh #:allocation #:virtual
	#:slot-ref (lambda (this)
		     (append #[this '%lights] #[this '%mesh]))
	#:slot-set! (lambda (this mesh)
		      (let ((lights (extract-lights mesh))
			    (mesh (skip-lights mesh)))
			(set! #[this '%mesh] mesh)
			(set! #[this '%lights] lights)))
	#;(\ with-input-from-file "3d/cube.3d" read)))

(define-method (initialize (this <3d-model>) args)
  (next-method)
  (let-keywords args #t
		((mesh '(mesh)))
    (set! #[this 'mesh] mesh)))

(define (make-light . properties)
  (let ((light (make-light-)))
    (for (property value) in (map-n 2 list properties)
	 (set-light-property! light (keyword->symbol property) value))
    light))

(let-syntax ((mesh-extractor
	      (syntax-rules ()
		((_ extractor-name arg pattern+actions ...)
		 (match arg
		   (('mesh . definition)
		    `(mesh ,@(filter-map extractor-name definition)))
		   (('with-transforms transforms . actions)
		    (let ((subdefinition (filter-map extractor-name actions)))
		      (if (null? subdefinition)
			  #f
			  `(with-transforms ,transforms
					    ,@subdefinition))))
		   pattern+actions ...
		   (else
		    #f))))))
  
  (define-method (extract-lights (mesh <list>))
    (mesh-extractor extract-lights mesh
      (('light . properties) 
       `(light ,@properties))))

  (define-method (skip-lights (mesh <list>))
    (mesh-extractor skip-lights mesh
      (((? (lambda(x)(not (equal? x 'light))) command) . properties)
       `(,command ,@properties))))
  )

(define (transform-matrix! transform-spec)
  (for-each (match-lambda
	     (('load-identity!)
	      (load-identity!))
	     (('translate-view! (? array? vector))
	      (translate-view! vector))
	     (('rotate-view! quaternion)
	      (rotate-view! quaternion))
	     (else
	      (<< "unrecognised transformation: "else)))
	    transform-spec))

(let-syntax ((process-mesh
	      (syntax-rules ()
		((_ mesh-processor arg pattern+actions ...)
		 (match arg
		   (('mesh . definition)
		    (for-each (match-lambda
			       (('with-transforms transforms . actions)
				(push-matrix!)
				(transform-matrix! transforms)
				(mesh-processor `(mesh ,actions))
				(pop-matrix!))
			       pattern+actions ...
			       (else
				(display `(mesh-processor 
					   match-failure: ,else)))
			       ) definition)
		    (for-each forget-array! 
			      '(vertex-array 
				color-array 
				normal-array 
				texture-coord-array)))
		   (else
		    (display `(mesh-processor invalid-mesh: ,else))))))))

  (define-method (draw-mesh! (mesh <list>))
    (process-mesh draw-mesh! mesh
		  (('vertices (? array? array))
		   (set-vertex-array! array))
		  (('color color)
		   (set-color! color))
		  (('colors (? array? array))
		   (set-color-array! array))
		  (('normals (? array? array))
		   (set-normal-array! array))
		  (('faces . faces)
		   (for-each (match-lambda ((type array)
					    (draw-faces! type array)))
			     faces))))

  (define-method (setup-lights! (lights <list>))
    (process-mesh setup-lights! lights
		  (('light . properties)
		   (let ((light (apply make-light properties)))
		     ;; the lights created here
		     ;; should be removed by the caller
		     (demand 'remove-light light #;after-rendering)))))
  )



(define-method (draw-mesh! (mesh <3d-model>))
  (draw-mesh! #[mesh '%mesh]))

(define-method (setup-lights! (model <3d-model>))
  (push-matrix!)
  (translate-view! #[model 'position])
  (rotate-view! #[model 'orientation])
  (setup-lights! #[model '%lights])
  (pop-matrix!))

(define-syntax-rule (replace/mesh m (pattern replacement) ...)
  (match m
    (('mesh . primitives)
     `(mesh ,@(map (lambda (primitive)
		     (match primitive
		       (pattern replacement)
		       ...
		       (else primitive)))
		   primitives)))))

(define-method (draw-model! (model <3d-model>))
  (push-matrix!)
  (translate-view! #[model 'position])
  (rotate-view! #[model 'orientation])
  (draw-mesh! #[model '%mesh])
  (pop-matrix!))

(define-method (draw-contour! (model <3d-model>))
  (push-matrix!)
  (translate-view! #[model 'position])
  (rotate-view! #[model 'orientation])
  (scale-view! 1.1)
  (draw-mesh! (replace/mesh #[model '%mesh]
			    (((or 'color 'colors) . _)
			     `(color #f32(0 0.3 0.7 0.5)))))
  (pop-matrix!))

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
