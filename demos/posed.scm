#!../src/slayer -e3d
exit
!#

(set! %load-path (append '("." "./guile-modules" ".." "../guile-modules"
			   "./scum")
			 %load-path))

(use-modules 
 (slayer)
 (slayer image)
 (slayer font)
 (slayer 3d)
 (extra common)
 (extra math)
 (extra slayer)
 (extra ref)
 (extra 3d)
 (oop goops)
 (widgets base)
 (widgets physics)
 (widgets 3d)
 (editor relations)
 (editor modes)
 (editor poses)
 (editor posed widgets)
 (editor posed undo)
 (editor control)
 (editor limbs)
 (editor movesets)
 (extra scmutils)
 (scum physics))

(set-window-title! "POSED: The POSE Editor")

(define the-simulation (primitive-make-simulation))

(define physical-objects (make <physics-stage> #:simulation the-simulation))

(define-rig rob (with-input-from-file "art/rigs/rob.rig" read))

(define-rig ground (with-input-from-file "art/rigs/ground.rig" read))

(define view (make <3d-editor>
	       #:x  0 #:y  0 
	       #:w 640 #:h 480
	       #:stage physical-objects))

(add-child! view #;to *stage*)

(define the-rig (make-rig #;in the-simulation 'rob))

(define the-ground (make-rig #;in the-simulation 'ground))

(define restricted-rigs `(,the-ground))

(define (selectable? object)
  (and (is-a? object <physical-object>)
       (not (in? #[object 'rig] restricted-rigs))))

(set-pose! the-rig (pose #;of the-rig))

(define max-force 10.0)

(define max-velocity 3.0)

(keydn '=
  (lambda ()
    (if (shift?)
	(set! max-velocity (+ max-velocity 0.1))
	(set! max-force (+ max-force 10.0)))))

(keydn '-
  (lambda ()
    (if (shift?)
	(set! max-velocity (max 0.0 (- max-velocity 0.1)))
	(set! max-force (max 0.0 (- max-force 10.0))))))


(attach-velocity-muscles-to-all-joints! #;in the-rig
					     #;with-parameters 
					     (lambda () max-force)
					     (lambda () max-velocity))

(set! #[view 'left-click]
  (lambda (x y)
    (and-let* ((object (object-at-position x y #;in view))
	       ((selectable? object)))
      (unless (modifier-pressed? 'shift)
	(unselect-all! #;in view))
      (if (in? object #[view 'selected])
	  (unselect-object! view object)
	  (select-object! object #;from view)))))

(keydn '(ctrl a)
  (lambda ()
    (unselect-all! #;in view)
    (for object in #[view : 'stage : 'objects]
      (if (selectable? object)
	  (select-object! object #;from view)))))

(keydn 'esc (lambda () (unselect-all! view)))

(keydn '(ctrl z)
  (lambda () (restore-previous-rig-state! the-rig)))

(keydn 'c
  (lambda _
    (with-context-for-joint/body-relation
     (save-rig-state! the-rig)
     (for body in (map #[_ 'body] #[view 'selected])
       (for joint in (joints-attached-to body)
	 (set-pose! #;of the-rig #;to `(pose (,(joint-name joint) . 0.0))
			 #:keeping (first (two-bodies-attached-by joint))))))))

(keydn '(ctrl r)
  (lambda () 
    (save-rig-state! #;of the-rig)
    (reset-rig! the-rig)))


;; making these available for evaluations (cf "posed/evaluation.ss")

(define erp 'error-reduction-parameter)
(define cfm 'constraint-force-mixing)

(define (sim-set! property value)
  (set-simulation-property! the-simulation property value)
  (simulation-property the-simulation property))

(define editor (make <pose-editor-widget> #:rig the-rig
		     #:evaluations-file "posed/evaluation.ss"
		     #:pivotal-body
		     (lambda ()
		       (match (map #[_ 'body] #[view 'selected])
			 ((body . _)
			  body)
			 (_
			  (body-named 'chest #;from the-rig))))))

(add-child! editor #;to *stage*)

(keydn '(ctrl p)
  (lambda ()
    (save-rig-state! #;of the-rig)
    (apply-stops! #;to the-rig #:keeping (#[editor 'pivotal-body]))))

(keydn 'p 
  (lambda () 
    (if #[editor 'pause]
	(save-rig-state! #;of the-rig))
    (set! #[editor 'pause] (not #[editor 'pause]))
    (format #t "pause ~a\n" (if #[editor 'pause] 'on 'off))))

(keydn 'h (rotate-around-joint-mode view))

(add-timer! 
 10 
 (lambda()
   (unless #[editor 'pause]
     (make-simulation-step! the-simulation)
     (control!))))

(let ((camera #[view 'camera]))
  (set! #[camera 'position] #f32(0 -6 -0.7))
  (set! #[camera 'orientation] (normalized '(1.0 . #f32(1 0 0)))))

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

(let ((down '(1.0 . #f32(0 0 0)))
      (up '(0.0 . #f32(0 -1 0)))
      (left (normalized '(1.0 . #f32(1 1 1))))
      (right (normalized '(-1.0 . #f32(-1 1 1))))
      (ahead (normalized '(1.0 . #f32(1 0 0))))
      (back (normalized '(0.0 . #f32(0 1 1))))
      (camera #[view 'camera]))
  (define (look #;towards direction #;from position)
    (set! #[camera 'position] position)
    (set! #[camera 'orientation] direction))
  (keydn 1
    (lambda _  
      (let ((center (rig-mass-center the-rig)))
	(if (modifier-pressed? 'shift) 
	    (look back #;from (+ center #f32(0 7 0)))
	    (look ahead #;from (- center #f32(0 7 0)))))))
  (keydn 2
    (lambda _
      (let ((center (rig-mass-center the-rig)))
	(if (modifier-pressed? 'shift) 
	    (look right #;from (- center #f32(7 0 0)))
	    (look left #;from (+ center #f32(7 0 0)))))))
  (keydn 3
    (lambda _ 
      (let ((center (rig-mass-center the-rig)))
	(if (modifier-pressed? 'shift) 
	    (look up #;from (- center #f32(0 0 7)))
	    (look down #;from (+ center #f32(0 0 7))))))))

(keydn 0 (lambda _ (<< #[view : 'camera : 'orientation])))
(keydn 9 (lambda _ (<< #[view : 'camera : 'position])))


(let ((turn-camera! (lambda (x y dx dy)
		      (relative-turn! #[view 'camera] (- dx) (- dy))))
      (walls (filter-map (lambda (rig) (and-let* (((wall) (rig-bodies rig))
					     ('plane (body-type wall)))
				    wall))
			 (simulation-rigs the-simulation)))
      (dragged-bodies '())
      (original-positions '())
      (tips '())
      (original-position #f)
      (dragging-corpus #f)
      (dragged-limb #f)
      (z 0.0))

  (set! #[view 'left-mouse-down]
    (lambda (x y)
      (and-let* ((object (object-at-position x y #;in view))
		 ((in? object #[view 'selected]))
		 (body #[object 'body])) 
	(with-context-for-joint/body-relation
	 (save-rig-state! the-rig)
	 (cond ((or (> (length #[view 'selected]) 1)
		    (part-of-corpus? body)
		    (any part-of-corpus? (bodies-attached-to body)))
		(let* ((rig #[object 'rig])
		       (bodies (rig-bodies rig))
		       (positions (map (lambda (body)
					 (body-property body 'position))
				       bodies))
		       (position (body-property body 'position))
		       ((_ _ z/screen) (3d->screen view position)))
		  (set! tips (filter tip? bodies))
		  (set! z z/screen)
		  (set! original-position position)
		  (set! dragged-bodies bodies)
		  (set! original-positions positions)
		  (set! dragging-corpus #t)))
	       (else
		(let (((_ _ z/screen) 
		       (3d->screen view (body-property body 'position))))
		  (set! z z/screen)
		  (set! dragged-limb body))))))))

  (set! #[view 'drag]
    (lambda (x y dx dy)
      (unless (= dx dy 0)
	(with-context-for-joint/body-relation
	 (cond (dragging-corpus
		(let ((displacement (screen->3d view x y z)))
		  (for (body position) in (zip dragged-bodies 
					       original-positions)
		    (set-body-property! body 'position
					(+ (- position original-position)
					   displacement))))
		(for wall in walls
		  (for tip in tips
		    (and-let* ((distance (body-distance wall tip))
			       (normal (body-property wall 'normal))
			       ((negative? distance))
			       ((parent) (bodies-attached-to tip))
			       (position (body-property parent 'position))
			       (displacement (* (- distance) normal))
			       (desired-position (+ position displacement)))
		      (for body in dragged-bodies
			(set-body-property! body 'position
					    (+ (body-property body 'position)
					       displacement)))))))
	       (dragged-limb
		(let ((current-position (body-property dragged-limb 'position))
		      (desired-position (screen->3d view x y z)))
		  (apply-inverse-kinematics! 
		   #;of dragged-limb #;to desired-position #;at hub?)))
	       (else
		(turn-camera! x y dx dy)))))))

  (set! #[view 'left-mouse-up]
    (lambda (x y)
      (set! original-position #f)
      (set! dragged-bodies '())
      (set! original-positions '())
      (set! dragging-corpus #f)
      (set! dragged-limb #f))))

(keydn 'l
  (lambda ()
    (save-rig-state! the-rig)
    (for body in (map #[_ 'body] #[view 'selected])
      (and-let* ((name (body-name body))
		 ((side) (symbol-match "^(.*)-foot$" name))
		 (rig (body-rig body))
		 (q (body-property body 'quaternion))
		 (y' (rotate y-axis #;by q))
		 (beta (asin (* y' z-axis)))
		 (/side/-ankle-bend (symbol-append side '-ankle-bend))
		 (joint (joint-named /side/-ankle-bend #;from rig))
		 (tigh (body-named (symbol-append side '-tigh) #;from rig))
		 (angle (joint-property joint 'angle)))
	(set-pose! the-rig `(pose (,/side/-ankle-bend . ,(- angle beta)))
		   #:keeping tigh)
	(let* ((q' (body-property body 'quaternion))
	       (x'' (rotate x-axis #;by q'))
	       (alpha (asin ((if (eq? side 'right) - +) (* x'' z-axis))))
	       (/side/-ankle-twist (symbol-append side '-ankle-twist))
	       (joint (joint-named /side/-ankle-twist #;from rig))
	       (angle (joint-property joint 'angle)))
	  (set-pose! the-rig `(pose (,/side/-ankle-twist . ,(- angle alpha)))
		     #:keeping tigh))))))

(define-class <mass-center> (<3d-object>)
  (target #:init-keyword #:of)
  (mesh #:allocation #:class
	 #:init-value
	 '(mesh (vertices #2f32((0 0 -1000) (0 0 1000)))
		(color #f32(1 0 0))
		(faces (lines #u8(0 1)))))
  (position 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self) (rig-mass-center #[self 'target]))
   #:slot-set! noop))

(let ((center (make <mass-center> #:of the-rig)))
  (keydn 'm
    (lambda ()
      (if (in? center #[physical-objects '%permanent-objects])
	  (set! #[physical-objects '%permanent-objects]
	    (delete center #[physical-objects '%permanent-objects]))
	  (push! #[physical-objects '%permanent-objects] center)))))

(define moveset-file (if (defined? '$1) $1 "posed/default.moves"))

(when (file-exists? moveset-file)
  (set! #[editor 'moveset] (with-input-from-file moveset-file read)))

(set! #[*stage* 'on-exit]
  (lambda (self)
    (let ((previous-moveset (with-input-from-file moveset-file read))
	  (current-moveset #[editor 'moveset]))
      (unless (same-movesets? previous-moveset current-moveset)
	(let ((backup (next-available-file-name moveset-file)))
	  (when (file-exists? moveset-file)
	    (copy-file moveset-file backup))
	  (with-output-file moveset-file 
	    (pretty-print current-moveset)))))))
