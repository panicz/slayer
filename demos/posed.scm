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

(set-simulation-property! the-simulation 'gravity #f32(0 0 -0.3))
(set-simulation-property! the-simulation 'error-reduction-parameter 0.8)
(set-simulation-property! the-simulation 'constraint-force-mixing 0.001)
(set-simulation-property! the-simulation 'auto-disable #t)

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

(attach-pid-muscles-to-all-joints! #;in the-rig
					#;with-parameters 20.0 0.3 -10.0)

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

(define editor (make <pose-editor-widget> #:rig the-rig
		     #:pivotal-body
		     (lambda ()
		       (match (map #[_ 'body] #[view 'selected])
			 ((body . _)
			  body)
			 (_
			  (body-named 'chest #;from the-rig))))))

(add-child! editor #;to *stage*)

(keydn '(lctrl o)
  (lambda ()
    (<< "adjust rotation of the tip of the limb to the origin")))

(keydn '(lctrl p)
  (lambda ()
    (save-rig-state! #;of the-rig)
    (apply-stops! #;to the-rig #:keeping (#[editor 'pivotal-body]))))

(keydn 'p 
  (lambda () 
    (set! #[editor 'pause] (not #[editor 'pause]))
    (format #t "pause ~a\n" (if #[editor 'pause] 'on 'off))))

(keydn 'h (rotate-around-joint-mode view))

(add-timer! 
 25 
 (lambda()
   (unless #[editor 'pause]
     (make-simulation-step! the-simulation)
     (control!))))

(load "config.scm")

;; override the default config (this may get confusing one day)

(let ((turn-camera! #[view 'drag])
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
		    (part-of-corpus? body))
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

(keydn '=
  (lambda ()
    (with-context-for-joint/body-relation
     (let ((displacement #f32(0 0 0.03)))
       (for body in (rig-bodies the-rig)
	 (set-body-property! body 'position
			     ((if (shift?) + -) (body-property body 'position)
			      displacement)))
       (for ankle in '(left-ankle right-ankle)
	 (let* ((limb (body-named ankle #;from the-rig))
		(its-position (body-property limb 'position))
		(new-position ((if (shift?) - +) its-position displacement)))
	   (save-rig-state! #;of the-rig)
	   (apply-inverse-kinematics! #;of limb 
					   #;to new-position #;at hub?)))))))

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
   (lambda (self) (mass-center #[self 'target]))
   #:slot-set! noop))

(let ((center (make <mass-center> #:of the-rig)))
  (keydn 'm
    (lambda ()
      (if (in? center #[physical-objects '%permanent-objects])
	  (set! #[physical-objects '%permanent-objects]
	    (delete center #[physical-objects '%permanent-objects]))
	  (push! #[physical-objects '%permanent-objects] center)))))

(define moveset-file (if (defined? '$1) $1 "default.moves"))

(when (file-exists? moveset-file)
  (set! #[editor 'moveset] (with-input-from-file moveset-file read)))

(set-exit-procedure!
 (lambda (outfile)
   (let ((previous-moveset (with-input-from-file moveset-file read))
	 (current-moveset #[editor 'moveset]))
     (unless (same-movesets? previous-moveset current-moveset)
       (let ((backup (next-available-file-name moveset-file)))
	 (when (file-exists? moveset-file)
	   (copy-file moveset-file backup))
	 (with-output-file moveset-file 
	   (pretty-print current-moveset)))))))
