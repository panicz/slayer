#!../src/slayer -e3d
exit
!#

(set! %load-path (append '("." "./guile-modules" ".." "../guile-modules"
			   "./scum")
			 %load-path))

(use-modules (extra common)
	     (extra math)
	     (extra slayer)
	     (extra ref)
	     (oop goops)
	     (widgets base)
	     (widgets physics)
	     (widgets 3d)
	     (editor relations)
	     (scum physics))

(set-window-title! "POSED: The POSE Editor")

(define the-simulation (primitive-make-simulation))

(set-simulation-property! the-simulation 'gravity #f32(0 0 0))

(define physical-objects (make <physics-stage> #:simulation the-simulation))

(define-rig-for the-simulation 
  'legs (with-input-from-file "art/rigs/legs.rig" read))

;; achtung! obsługujemy tylko hinge (a niebawem będziemy musieli wprowadzić
;; również hinge-2, ale inne -- dopiero później)

(define view (make <3d-editor>
	       #:x  10 #:y  10 
	       #:w 620 #:h 460
	       #:stage physical-objects))

(add-child! view #;to *stage*)

(define-syntax-rule (with-context-for-joint/body-relation action . *)
  (specify ((joint-property-getter joint-property)
	    (body-rig-getter body-rig)
	    (rig-joints-getter rig-joints))
    action . *))

(define the-legs (make-rig the-simulation 'legs #:position #f32(0 0 0)
			  #:orientation '(0.707 . #f32(-0.707 0 0))))

(define (rotate-body! body #;by angle #;around axis #;at pivot)
  (let ((position (body-property body 'position))
	(orientation (body-property body 'quaternion)))
    (set-body-property! body 'position
			(rotate position #;by angle #;around axis
				#;at pivot))
    (set-body-property! body 'quaternion
			(* (rotation-quaternion #;around axis
							 #;by angle)
			   orientation))))

(define (rotate-joint! joint #;by angle #;around axis #;at pivot)
  (assert (eq? (joint-type joint) 'hinge))
  (let ((anchor (joint-property joint 'anchor))
	(axis (joint-property joint 'axis)))
    (set-joint-property! joint 'anchor (rotate anchor #;by angle 
					       #;around axis #;at pivot))
    (set-joint-property! joint 'axis (rotate axis #;by angle #;around axis))))

(define (set-pose! #;of rig #;to pose)
  (assert (pose? pose))
  (with-context-for-joint/body-relation
   (match pose
     (('pose . parameters)
      (for (joint-name . angle) in parameters
	(when angle
	  (let*-values (((joint) (joint-named joint-name #;from rig))
			((mobile-bodies immobile-bodies) 
			 (split-bodies-at joint))
			((mobile-joints) 
			 (unique (fold union '()
				       (map joints-attached-to
					    mobile-bodies))))
			((pivot axis) 
			 (begin
			   (assert (eq? (joint-type joint) 'hinge))
			   (values (joint-property joint 'anchor)
				   (joint-property joint 'axis)))))
	    (for body in mobile-bodies
	      (rotate-body! body #;by angle #;around axis #;at pivot))
	    #;(for joint in mobile-joints
	      (rotate-joint! joint #;by angle #;around axis 
			     #;at pivot)))))))))

(include "temporary-poses.scm")

;; selekta!

;; warunek dokonywania obrotów:
;; 1. na początku muszą być zaznaczone dokładnie dwa ciała połączone
;; więzem
;; 2. wybieramy jedno z ciał do obracania wokół. a drugie jako nieruchome.
;; pozycja więzu staje się punktem obrotu
;; 3. obracania dokonujemy względem rzutu osi prostopadłej do ekranu na
;; oś więzu

;; konieczność napisania funkcji ładującej pozę i ustawiającą ją
;; dla danego ciała

(set! #[view 'left-click]
      (lambda (x y)
	(let ((object (object-at-position x y view)))
	  (when object
	    (with-context-for-joint/body-relation
	     (let ((joints (joints-attached-to #[object 'body])))
	       (pretty-print (map joint-name joints))))
	    (select-object! object #;from view)))))

(keydn 'esc (lambda () (unselect-all! view)))

(keydn 'g (grab-mode view))

(keydn 'h (rotate-mode view #:always-rotate-around-center #t))

(define pause #f)
(keydn 'p (lambda () (set! pause (not pause))))

(add-timer! 
 25 
 (lambda()
   (unless pause
     (make-simulation-step! the-simulation)
   )))

(load "config.scm")
