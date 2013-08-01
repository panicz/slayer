(define-module (widgets 3d-view)
  #:use-module (oop goops)
  #:use-module (widgets base)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra ref)
  #:use-module (extra 3d)
  #:use-module (slayer 3d)
  #:export (<3d-view> 
	    add-object! 
	    draw-mesh! 
	    draw-objects
	    relative-turn!
	    relative-twist!
	    relative-move!
	    mouse->3d
	    X-SENSITIVITY
	    Y-SENSITIVITY))

(define-class <3d-view> (<extended-widget>)
  (camera #:init-thunk (lambda()(make <3d-cam>)))
  (objects #:init-value '()))

(define-method (draw-objects (view <3d-view>))
  (let ((lights '()))
    (assure
     (#;to ((remove-light l #;after-rendering)
	    #;by-doing (push! lights l)))
     (for model in #[view 'objects]
	  (setup-lights! #[model '%lights]))
     ;; lights need to be set up before the scene is rendered,
     ;; but they can be positioned in the same place as the vertices,
     ;; so we need to extract the lights (along with any matrix
     ;; transformations) from the model definition first
     (for-each draw-model! #[view 'objects])
     (for-each remove-light! lights))))

(define-method (draw (view <3d-view>))
  (let ((original-viewport (current-viewport))
	(camera #[view 'camera]))
    (apply set-viewport! (area view))
    (set-perspective-projection! #[camera 'fovy])
    (push-matrix!)
    (translate-view! #f32(0 0 -0.1))
    (rotate-view! (~ #[camera 'orientation]))
    (translate-view! (- #[camera 'position]))
    (set-light-property! #[camera 'light] 'position #[camera 'position])
    (draw-objects view)
    (pop-matrix!)
    (apply set-viewport! original-viewport)))

(define-method (add-object! (view <3d-view>) (object <3d>))
  (set! #[view 'objects] (cons object #[view 'objects])))

(define X-SENSITIVITY (make-fluid 0.01))
(define Y-SENSITIVITY (make-fluid 0.01))

(define-method (relative-turn! (object <3d>) (x <number>) (y <number>))
  (set! #[object 'orientation]
	(normalized 
	 (+ #[object 'orientation] 
	    (* (quaternion 0.0 (* x #[X-SENSITIVITY]
				  (rotate #f32(0 1 0) #[object 'orientation])))
	       #[object 'orientation])
	    (normalized (+ #[object 'orientation]
			   (* (quaternion 0.0 
					  (* y #[Y-SENSITIVITY]
					     (rotate #f32(1 0 0)
						     #[object 'orientation])))
			      #[object 'orientation])))))))

(define-method (relative-twist! (object <3d>) axis)
  (set! #[object 'orientation]
	(normalized
	 (+ #[object 'orientation]
	    (* (quaternion 0.0 (rotate axis #[object 'orientation]))
	       #[object 'orientation])))))

(define-method (relative-move! (object <3d>) direction)
  (increase! #[object 'position]
	     (rotate direction #[object 'orientation])))

(define-method (mouse->3d (view <3d-view>) (x <integer>) (y <integer>))
  (let* ((camera #[view 'camera])
	 (matrix (*
		  (translation-matrix (- #[camera 'position]))
		  (quaternion->matrix (~ #[camera 'orientation]))

		 ))
	 (projection (perspective-projection
		      #[camera 'fovy]
		      (/ (* 1.0 #[view 'w]) #[view 'h]))))
    (unproject x y #f
	       matrix
	       projection
	       (area view))))
