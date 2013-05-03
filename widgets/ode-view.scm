(define-module (widgets ode-view)
  #:use-module (libs physics)
  #:use-module (widgets base)
  #:use-module (widgets 3d-view)
  #:use-module (oop goops)
  #:use-module (extra 3d)
  #:use-module (extra math)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (slayer)
  #:use-module (slayer 3d)
  #:export (<ode-view>))


(use-modules (extra ref) (oop goops))

(define-class <ode-view> (<3d-view>)
  (meshes #:init-value #[] #:allocation #:class)
  (simulation #:init-value #f #:init-keyword #:simulation))

(define-method (update! (ov <ode-view>))
  (simulation-step! #[ov 'simulation]))

(define-method (initialize (ov <ode-view>) args)
  (next-method)
  (for rig in (simulation-rigs #[ov 'simulation])
       (for body in (rig-bodies rig)
	    (and-let* ((mesh
			(match (body-type body)
			  ('box
			   (let ((dims (body-property body 'dimensions)))
			     (generate-box #:x #[dims 0] 
					   #:y #[dims 1] #:z #[dims 2])))
			  ('sphere
			   (let ((radius (body-property body 'radius)))
			     (generate-sphere #:radius radius)))
			  ('cylinder
			   (let ((radius (body-property body 'radius))
				 (height (body-property body 'height)))
			     (generate-tube #:radius radius
					    #:height height)))
			  ('plane
			   (let ((grid (transform-mesh-vertices
					(match-lambda ((x y) (list x 0.0 y)))
					(square-grid #:size 10.0 #:density 50))))
			     grid))
			  (else #f))))
	      (hash-set! #[ov 'meshes] (body-id body) mesh)))))

(define-method (draw-objects (ov <ode-view>))
  (for rig in (simulation-rigs #[ov 'simulation])
       (for body in (rig-bodies rig)
	    (let ((position (body-property body 'position))
		  (rotation (body-property body 'quaternion)))
	      ;; i jeszcze shape jakos trzeba wydobitch
	      ;;(display position)(newline)
	      (push-matrix!)
	      (translate-view! position)
	      (rotate-view! rotation)
	      (draw-mesh #[ov : 'meshes : (body-id body)])
	      (pop-matrix!)))))
