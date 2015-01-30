#!../src/slayer -e3d
!#
(use-modules (slayer)
	     (slayer 3d)
	     (widgets base)
	     (widgets physics)
	     (oop goops)
	     (extra ref)
	     (extra common)
	     (extra 3d)
	     (extra math)
	     (extra slayer)
	     (scum physics)
	     (editor poses)
	     (editor control))

(define the-simulation (primitive-make-simulation))

(define-rig legs (with-input-from-file "art/rigs/legs.rig" read))

(define-rig ground (with-input-from-file "art/rigs/ground.rig" read))

(define the-ground (make-rig the-simulation 'ground))

(define the-legs (make-rig #;in the-simulation #;instantiating 'legs))

(set-simulation-property! the-simulation 'gravity #f32(0 0 -0.098))

(define stage (make <physics-stage> #:simulation the-simulation))

(define view
  (make <3d-editor> #:x 10 #:y 10 
	#:w (- (screen-width) 10)
	#:h (- (screen-height) 10)
	#:stage stage))

(add-child! view #;to *stage*)

(include "config.scm")

(include "temporary-poses.scm")

(keydn 'return (lambda ()
		 (reset-behaviors! #;of the-legs)
		 (initiate-sequence!
		  (circular-list stand left-up left-front left-straight
				 stand right-up right-front right-straight)
		  the-legs)))

(define pause #f)
(keydn 'p (lambda () (set! pause (not pause))))

(add-timer! 
 25 
 (lambda()
   (unless pause
     (make-simulation-step! the-simulation)
     (control!)
   )))
