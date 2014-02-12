(define-module (widgets 3d)
  #:use-module (oop goops)
  #:use-module (widgets base)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra shape)
  #:use-module (extra ref)
  #:use-module (extra 3d)
  #:use-module (slayer)
  #:use-module (slayer 3d)
  #:export (<3d-view> 
	    <3d-stage>
	    <3d-editor> 
	    add-object! 
	    object-at-position
	    relative-turn!
	    relative-twist!
	    relative-move!
	    screen->3d
	    3d->screen
	    select-object!
	    unselect-object!
	    unselect-all!
	    X-SENSITIVITY
	    Y-SENSITIVITY))

(define-class <3d-stage> ()
  (objects #:init-value '()))

(define-method (add-object! (object <3d>) #;to (stage <3d-stage>))
  (push! #[stage 'objects] object))

(define-class <3d-view> (<extended-widget>)
  (camera #:init-thunk (lambda()(make <3d-cam>)))
  (draw-objects!
   #:allocation #:virtual
   #:slot-ref (lambda (view)
		(for object in #[view : 'stage : 'objects]
		     (draw-model! object)))
   #:slot-set! noop)
  (lit-objects!
   #:allocation #:virtual
   #:slot-ref (lambda (view)
		(for object in #[view : 'stage : 'objects]
		     (setup-lights! #[object '%lights])))
   #:slot-set! noop)
  (stage #:init-form (make <3d-stage>) #:init-keyword #:stage))

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
  (let* ((camera #[view 'camera])
	 (matrix (* (translation-matrix (- #[camera 'position]))
		    (quaternion->matrix (~ #[camera 'orientation]))))
	 (projection (perspective-projection
		      #[camera 'fovy]
		      (/ (* 1.0 #[view 'w]) #[view 'h]))))
    (screen->world/coordinates x y z matrix projection (area view))))

(define-method (3d->screen (view <3d-view>) (position <point>))
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
	    (draw-model! object)
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
    (case (length candidates)
      ((0)
       #f)
      ((1)
       (first candidates))
      (else
       (format #t "ambiguous display-index candidates\n")))))

(define-method (select-object! (view <3d-editor>) (object <3d>))
  (if (not (in? object #[view 'selected]))
      (push! #[view 'selected] object)))

(define-method (unselect-object! (view <3d-editor>) (object <3d>))
  (set! #[view 'selected] (delete object #[view 'selected])))

(define-method (unselect-all! (view <3d-editor>))
  (set! #[view 'selected] '()))
