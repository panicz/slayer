(define-module (widgets 3d)
  #:use-module (oop goops)
  #:use-module (widgets base)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra shape)
  #:use-module (extra ref)
  #:use-module (extra 3d)
  #:use-module (extra slayer)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (slayer 3d)
  #:export (<3d-view> 
	    <3d-stage>
	    <3d-editor> 
	    add-object! 
	    delete-object!
	    object-at-position
	    relative-turn!
	    relative-twist!
	    relative-move!
	    screen->3d
	    3d->screen
	    select-object!
	    unselect-object!
	    select-all!
	    unselect-all!
	    delete-selected-objects!
	    grab-mode
	    rotate-mode
	    X-SENSITIVITY
	    Y-SENSITIVITY))

(define-class <3d-stage> ()
  (objects #:init-value '()))

(define-method (add-object! (object <3d>) #;to (stage <3d-stage>))
  (push! #[stage 'objects] object))

(define-method (delete-object! (object <3d>) #;from (stage <3d-stage>))
  (set! #[stage 'objects] (delete object #[stage 'objects])))

(define-class <3d-view> (<extended-widget>)
  (%horizontal-frame #:init-value #f)
  (%vertical-frame #:init-value #f)
  (%resize #:init-value noop)
  (camera #:init-thunk (lambda()(make <3d-cam>)) #:init-keyword #:camera)
  (resize
   #:allocation #:virtual
   #:slot-ref (lambda(self)
		(lambda(w h)
		  (unless (= w #[self '%w])
		    (set! #[self '%horizontal-frame]
			  (rectangle w 1 #xff0000)))
		  (unless (= h #[self '%h])
		    (set! #[self '%vertical-frame]
			  (rectangle 1 h #xff0000)))
		  (#[self '%resize] w h)))
   #:slot-set! (lambda(self resize)
		 (set! #[self '%resize] resize)))
  (draw-objects!
   #:allocation #:virtual
   #:slot-ref (lambda (view)
		(for object in #[view : 'stage : 'objects]
		     (draw-object! object)))
   #:slot-set! noop)
  (lit-objects!
   #:allocation #:virtual
   #:slot-ref (lambda (view)
		(noop)
		#;(for object in #[view : 'stage : 'objects]
		     (setup-lights! #[object '%lights])))
   #:slot-set! noop)
 
  (stage #:init-form (make <3d-stage>) #:init-keyword #:stage))

(define-method (initialize (self <3d-view>) args)
  (next-method)
  (set! #[self 'mouse-move] 
    (lambda (x y dx dy)
      (if (and left-click-widget 
	       (eq? self left-click-widget)
	       (not (zero? (+ (abs dx) (abs dy)))))
	  (set! left-click-widget #f))))
  (set! #[self '%horizontal-frame] (rectangle #[self 'w] 1 #xff0000))
  (set! #[self '%vertical-frame] (rectangle 1 #[self 'h] #xff0000)))

(define-method (draw-scene (view <3d-view>))
  (let ((lights '()))
    (supply
	(((remove-light l #;after-rendering)
       #;by-doing (push! lights l)))
      #[view 'lit-objects!]
      ;; lights need to be set up before the scene is rendered,
      ;; but they can be positioned in the same place as the vertices,
      ;; so we need to extract the lights (along with any matrix
      ;; transformations) from the model definition first
      #[view 'draw-objects!]
      (for-each remove-light! lights))))

(define-method (draw-border! (view <3d-view>))
  (let (((x y w h) (area view)))
    (draw-image! #[view '%horizontal-frame] x y)
    (draw-image! #[view '%vertical-frame] x y)
    (draw-image! #[view '%horizontal-frame] x (+ y h))
    (draw-image! #[view '%vertical-frame] (+ x w -1) y)))

(define-method (draw (view <3d-view>))
  (let ((original-viewport (current-viewport))
	((x y w h) (area view))
	(camera #[view 'camera]))
    (draw-border! view)
    (set-viewport! (+ x 1) (+ y 1) (- w 2) (- h 2))
    (set-perspective-projection! #[camera 'fovy])
    (push-matrix!)
    (translate-view! #f32(0 0 -0.1))
    (rotate-view! (~ #[camera 'orientation]))
    (translate-view! (- #[camera 'position]))
    (set-light-property! #[camera 'light] 'position #[camera 'position])
    (draw-scene view)
    (pop-matrix!)
    (apply set-viewport! original-viewport)))

(define-fluid X-SENSITIVITY 0.01)
(define-fluid Y-SENSITIVITY 0.01)

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

(define* (screen->3d view x y #:optional (z #f))
  (assert (let ((result (screen->3d view position)))
	    (if result 
		(and (uniform-vector? result)
		     (eq? (array-type result) 'f64)))))
  (let* ((camera #[view 'camera])
	 (matrix (* (translation-matrix (- #[camera 'position]))
		    (quaternion->matrix (~ #[camera 'orientation]))))
	 (projection (perspective-projection
		      #[camera 'fovy]
		      (/ (* 1.0 #[view 'w]) #[view 'h]))))
    (screen->world/coordinates x y z matrix projection (area view))))

(define-method (3d->screen (view <3d-view>) (position <point>))
  (assert (let ((result (3d->screen view position)))
	    (if result
		(matches? ((? real?) (? real?) (? real?)) result))))
  (let* ((camera #[view 'camera])
	 (matrix (* (translation-matrix (- #[camera 'position]))
		    (quaternion->matrix (~ #[camera 'orientation]))))
	 (projection (perspective-projection
		      #[camera 'fovy]
		      (/ (* 1.0 #[view 'w]) #[view 'h]))))
    (world->screen/coordinates #[position 0] #[position 1] #[position 2]
			       matrix projection (area view))))

(define-class <3d-editor> (<3d-view>)
  (object-groups #:init-form (make-vector (1+ (max-display-index)) '()))
  (draw-objects!
   #:allocation #:virtual
   #:slot-ref 
   (lambda (view)
     (array-map! #[view 'object-groups] (lambda _ '()))
     (let ((index 0))
       (for object in #[view : 'stage : 'objects]
	    (set-display-index! index)
	    (draw-object! object)
	    (when (in? object #[view 'selected])
	      (draw-contour! object))
	    (push! #[view : 'object-groups : index] object)
	    (set! index (modulo (+ index 1) (max-display-index))))))
   #:slot-set! noop)
  (objects #:init-thunk make-hash-table)
  ;; hash whose keys are objects and values -- display indices
  (selected #:init-value '()))

(define-method (object-at-position x y #;from (editor <3d-editor>))
  (and-let* ((n (display-index x y))
	     (candidates #[editor : 'object-groups : n]))
    (match candidates
      (()
       #f)
      ((only)
       only)
      (else
       (format #t "ambiguous display-index candidates\n")))))

(define-method (select-object! (object <3d>) #;from (view <3d-editor>))
  (if (modifier-pressed? 'shift)
      (if (in? object #[view 'selected])
	  (unselect-object! view object)
	  (push! #[view 'selected] object))
      (set! #[view 'selected] `(,object))))

(define-method (select-all! #;in (view <3d-editor>))
  (set! #[view 'selected] #[view : 'stage : 'objects]))

(define-method (unselect-object! (view <3d-editor>) (object <3d>))
  (set! #[view 'selected] (delete object #[view 'selected])))

(define-method (unselect-all! (view <3d-editor>))
  (set! #[view 'selected] '()))

(define-method (delete-selected-objects! (view <3d-editor>))
  (for object in #[view 'selected]
       (delete-object! object #;from #[view 'stage]))
  (set! #[view 'selected] '()))

(define ((grab-mode view))
  (unless (null? #[view 'selected])
    (let* ((old-bindings (current-key-bindings))
	   (original-positions (map #[_ 'position] #[view 'selected]))
	   (axis-mapping #f32(1 1 1))
	   (lock (lambda (u #;on v)
		   (list->typed-array 'f32 1 `(,(* #[u 0] #[v 0])
					       ,(* #[u 1] #[v 1])
					       ,(* #[u 2] #[v 2])))))
	   (snap (lambda (v)
		   (let ((v (* v 10.0)))
		     (* 0.1
			(list->typed-array 
			 'f32 1
			 `(,(floor #[v 0]) ,(floor #[v 1]) ,(floor #[v 2])))))))
	   ((_ _ zs) (3d->screen view (first original-positions))))
      (set-key-bindings!
       (key-bindings
	(keydn 'x (lambda () (set! axis-mapping x-axis)))

	(keydn 'y (lambda () (set! axis-mapping y-axis)))

	(keydn 'z (lambda () (set! axis-mapping z-axis)))

	(keydn 'esc
	  (lambda () 
	    (for (object position) in (zip #[view 'selected]
					   original-positions)
	      (set! #[object 'position] position))
	    (set-key-bindings! old-bindings)))
	
	(keydn 'mouse-left
	  (lambda (x y)
	    (set-key-bindings! old-bindings)))
	    
	(mousemove 
	 (lambda (x y xrel yrel)
	   (let ((displacement (screen->3d view x y zs)))
	   (for (object position) in (zip #[view 'selected]
					  original-positions)
	     (set! #[object 'position] 
	       (lock ((if (modifier-pressed? 'ctrl)
			  snap
			  identity)
		      (+ (- position (first original-positions))
			 displacement ))
		     #;on axis-mapping))))))
	)))))

(define* ((rotate-mode view #:key 
		       (axis #f)
		       (always-rotate-around-center #f)
		       (rotate-around-center always-rotate-around-center)
		       (center (lambda(selected)
				 (apply mean (map #[_ 'position] selected))))
		       (rotation-direction 1)
		       (on-exit noop)))
  (unless (null? #[view 'selected])
    (let ((old-bindings (current-key-bindings))
	  (center (center #[view 'selected]))
	  (angle 0.0)
	  (snap (let ((mu (/ 180.0 2pi)))
		  (lambda (x) (/ (floor (* x mu)) mu))))
	  (original-positions (map #[_ 'position] #[view 'selected]))
	  (original-orientations (map #[_ 'orientation] #[view 'selected])))
      (let (((_ _ zs) (3d->screen view center))
	    ((xs ys) (mouse-position)))
	(set-key-bindings!
	 (key-bindings
	  (keydn 'esc
	    (lambda ()
	      (for (object orientation position) in (zip #[view 'selected]
							 original-orientations
							 original-positions)
		(set! #[object 'orientation] orientation)
		(set! #[object 'position] position))
	      (set-key-bindings! old-bindings)
	      (on-exit 0.0)))

	(keydn 'x (lambda () (set! axis x-axis)))
	(keydn 'y (lambda () (set! axis y-axis)))
	(keydn 'z (lambda () (set! axis z-axis)))

	  (keydn 'tab
	    (lambda ()
	      (if (and rotate-around-center
		       (not always-rotate-around-center))
		  (for (object position) in (zip #[view 'selected]
						 original-positions)
		    ;; restore original position
		    (set! #[object 'position] position)))
	      (unless always-rotate-around-center
		(set! rotate-around-center (not rotate-around-center)))))

	  (keydn 'mouse-left
	    (lambda (x y)
	      (set-key-bindings! old-bindings)
	      (on-exit angle)))

	  (mousemove 
	   (lambda (x y xrel yrel)
	     (let* ((screen-rotation (rotation-quaternion 
				      #;from (- (screen->3d view xs ys zs) 
						center)
					     #;to (- (screen->3d view x y zs)
						     center)))
		    (screen-angle (* (sgn (* rotation-direction 
					     #[(im screen-rotation) 2]))
				     ((if (modifier-pressed? 'ctrl)
					  snap
					  identity)
				      (quaternion-angle
				       #;of screen-rotation))))
		    (axis (or axis (* (sgn (* rotation-direction 
					      #[(im screen-rotation) 2]))
				      (quaternion-axis screen-rotation))))
		    (rotation (rotation-quaternion
			       #;around axis #;by screen-angle)))
	       (set! angle screen-angle)
	       (for (object orientation position) in (zip #[view 'selected] 
							  original-orientations
							  original-positions)
		 (set! #[object 'orientation]
		       (* rotation orientation))

		 (when rotate-around-center
		   (set! #[object 'position]
			 (+ center (rotate (- position center)
					   #;by rotation)))
		   )))))))))))
