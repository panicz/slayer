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

;;(load "posed-trash.scm")

(define the-rig (make-rig #;in the-simulation 'rob #:position #f32(0 0 0)
			       #:orientation '(0.707 . #f32(-0.707 0 0))))

(define the-ground (make-rig #;in the-simulation 'ground))

(define ((rig-joints-name&properties-getter . properties) rig)
  "the properties are obtained directly from ODE simulator"
  (map (lambda (joint) 
	 `(,(joint-name joint)
	   ,@(map (lambda (property) 
		    (joint-property joint property))
		  properties)))
       (rig-joints rig)))

(define joint-angles (rig-joints-name&properties-getter 'angle))

(define joint-axes (rig-joints-name&properties-getter 'axis))

(define joint-anchors (rig-joints-name&properties-getter 'anchor))

(set-pose! the-rig (pose the-rig))

(set! #[view 'left-click]
      (lambda (x y)
	(let ((object (object-at-position x y view)))
	  (when object
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

(load "editor/posed/widgets.scm")

(keydn '/ (lambda () (with-output-file "default.moves" 
		  (pretty-print (current-moveset)))))

(and-let* ((moveset (if (defined? '$1) $1 "default.moves"))
	   ((file-exists? moveset)))
  (load-moveset! (with-input-from-file moveset read)))

(set-exit-procedure!
 (lambda (outfile)
   (if (file-exists? "default.moves")
       (copy-file "default.moves" (next-available-file-name "default.moves")))
   (with-output-file "default.moves" 
     (pretty-print (current-moveset)))))
