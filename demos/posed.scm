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
 (extra scmutils)
 (scum physics))

(set-window-title! "POSED: The POSE Editor")

(define the-simulation (primitive-make-simulation))

(set-simulation-property! the-simulation 'gravity #f32(0 0 -0.2))

(define physical-objects (make <physics-stage> #:simulation the-simulation))

(define-rig-for the-simulation 
  'rob (with-input-from-file "art/rigs/rob.rig" read))

(define-rig-for the-simulation
  'ground (with-input-from-file "art/rigs/ground.rig" read))

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
    (let ((object (object-at-position x y view)))
      (when (and object (selectable? object))
	(select-object! object #;from view)))))

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

;; override the default config
(let ((turn-camera! #[view 'drag]))
  (set! #[view 'drag]
    (lambda (x y dx dy)
      (turn-camera! x y dx dy))))

(load "editor/posed/widgets.scm")

(define moveset-file (if (defined? '$1) $1 "default.moves"))

(when (file-exists? moveset-file)
  (load-moveset! (with-input-from-file moveset-file read)))

(define (same-movesets? moveset-a moveset-b)
  (let ((('moveset ('poses . poses-a)
		   ('sequences . sequences-a)) moveset-a)
	(('moveset ('poses . poses-b)
		   ('sequences . sequences-b)) moveset-b))
    (and (equivalent-set? (lambda ((name-a . configuration-a)
			      (name-b . configuration-b))
			    (and (eq? name-a name-b)
				 (same-set? configuration-a
					    configuration-b)))
			  poses-a poses-b)
	 (same-set? sequences-a sequences-b))))

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
