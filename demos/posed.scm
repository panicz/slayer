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
 (widgets image-clipper)
 (widgets text-area)
 (widgets sortable)
 (widgets tab)
 (widgets sprite)
 (editor relations)
 (editor modes)
 (editor poses)
 (editor control)
 (editor limbs)
 (editor movesets)
 (extra scmutils)
 (scum physics))

(set-window-title! "POSED: The POSE Editor")

(define the-simulation (primitive-make-simulation))

(set-simulation-property! the-simulation 'gravity #f32(0 0 -0.3))
(set-simulation-property! the-simulation 'error-reduction-parameter 0.8)
(set-simulation-property! the-simulation 'constriant-force-mixing 0.9)
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

(set! #[view 'left-click]
  (lambda (x y)
    (and-let* ((object (object-at-position x y #;in view))
	       ((selectable? object)))
      (select-object! object #;from view))))

(keydn 'esc (lambda () (unselect-all! view)))

(keydn 'c
  (lambda _
    (with-context-for-joint/body-relation
     (for body in (map #[_ 'body] #[view 'selected])
       (for joint in (joints-attached-to body)
	 (set-pose! #;of the-rig #;to `(pose (,(joint-name joint) . 0.0))
			 #:keeping (first (two-bodies-attached-by joint))))))))

(keydn 'k (ik-mode view the-rig))

(keydn 'g (grab-mode view))

(define pause #t)

(keydn 'p 
  (lambda () 
    (set! pause (not pause))
    (format #t "pause ~a\n" (if pause 'on 'off))))

(keydn 'h (rotate-around-joint-mode view))

(add-timer! 
 25 
 (lambda()
   (unless pause
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
       (apply-inverse-kinematics! #;of limb #;to new-position #;at hub?)))))))

(load "editor/posed/widgets.scm")

(define moveset-file (if (defined? '$1) $1 "default.moves"))

(when (file-exists? moveset-file)
  (load-moveset! (with-input-from-file moveset-file read)))

(set-exit-procedure!
 (lambda (outfile)
   (let ((previous-moveset (with-input-from-file moveset-file read))
	 (current-moveset (current-moveset)))
     (unless (same-movesets? previous-moveset current-moveset)
       (let ((backup (next-available-file-name moveset-file)))
	 (when (file-exists? moveset-file)
	   (copy-file moveset-file backup))
	 (with-output-file moveset-file 
	   (pretty-print current-moveset)))))))
