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
	     (scum physics))

(define *sim* (primitive-make-simulation))

(set-simulation-property! *sim* 'gravity #f32(0 0 -0.1))
;(set-simulation-property! *sim* 'erp 0.8)
;(set-simulation-property! *sim* 'cfm 1.0)

(define-rig ground (with-input-from-file "art/rigs/ground.rig" read))
(make-rig *sim* 'ground)

(when (and (defined? '$1) (file-exists? $1))
  (define-rig object (with-input-from-file $1 read))
  (make-rig *sim* 'object))

(define *sim-stage* (make <physics-stage> #:simulation *sim*))

;; co my chcemy osiągnąć?
;; - jeżeli siła nie jest przyłączona do ciała, to wszystkich transformacji
;; dokonujemy w globalnym układzie współrzędnych
;; - w przeciwnym razie musimy uważać: chcemy, żeby siła przyłączona do ciała
;; obracała się razem z nim. Dlatego położenie i orientacja w globalnym układzie
;; współrzędnych to lokalne położenie i orientacja przetransformowane położeniem
;; i orientacją rodzica. Jak dokładnie wygląda ta transformacja?
;; (obrócony (przesunięty punkt-oryginalny o (- położenie-lokalnego-układu))
;;  o orientację-lokalnego-układu)

(define-class <force-vector> (<3d-object>)
  (parent #:init-value #f)
  (strength #:init-value 1.0)
  (vector 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (* #[self 'strength] (rotate #f32(0 0 1) #;by #[self 'orientation])))
   #:slot-set! noop)
  (%mesh #:init-value
	 `(mesh
	   (colors #2f32((1 0 0) (1 0 1)))
	   (vertices #2f32((0 0 0) (0 0 1)))
	   (faces (lines #u8(0 1)))))
  (mesh 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (match #[self '%mesh]
       (('mesh . data)
	(match-let (((vertices) #[data 'vertices]))
	  (set! #[vertices 1 2] #[self 'strength]))))
     #[self '%mesh])
   #:slot-set! noop))

(define-method (detach! (force <force-vector>))
  (set! #[force 'parent] #f))

(define-method (attach! (force <force-vector>) #;to (body <physical-object>))
  (set! #[force 'parent] body))

(define-method (draw-object! (model <force-vector>))
  (next-method))

(define the-force (make <force-vector>))

(push! #[*sim-stage* '%permanent-objects] the-force)

(keydn 'space 
  (lambda () 
    (match #[view 'selected]
      ((the-body . _)
       (attach! the-force #;to the-body))
      (else
       (display 
	"An object needs to be selected in order to attach the force to it\n"))
      )))

#|
(keydn 'return
  (lambda ()
    (match #[view 'selected]
      ((object . _)
       (if (is-a? object <physical-object>)
	   (let ((body #[oobject 'body]))
	     (if (modifier-pressed? 'shift)
		 (torque! body #[the-force 'vector])
		 (force! body #[the-force 'vector] 
			 #:at #[the-force 'position])))))
      (else
       (display "An object needs to be selected \
in order to apply force to it\n")))))
|#

(key 'i (lambda () (relative-move! the-force #f32(0 0 -0.07))))
(key 'k (lambda () (relative-move! the-force #f32(0 0 0.07))))
(key 'j (lambda () (relative-move! the-force #f32(-0.07 0 0))))
(key 'l (lambda () (relative-move! the-force #f32(0.07 0 0))))
(key 'p (lambda () (relative-move! the-force #f32(0 0.07 0))))
(key ";" (lambda () (relative-move! the-force #f32(0 -0.07 0))))
(key 'home (lambda () (relative-turn! the-force 0 2)))
(key 'end (lambda () (relative-turn! the-force 0 -2)))
(key 'delete (lambda () (relative-turn! the-force 2 0)))
(key 'page-down (lambda () (relative-turn! the-force -2 0)))

(keydn '/
  (lambda () 
    (let ((j (first (simulation-joints *sim*))))
      (format #t "~s (~s, ~s deg) connects ~s and ~s\n" 
	      (joint-name j) (joint-type j) 
	      (rad->deg (joint-property j 'angle))
	      (body-name (joint-property j 'body-1)) 
	      (body-name (joint-property j 'body-2))))))
      

(let ((set-axis! (lambda(axis)
		   (lambda ()
		     (set! #[the-force 'orientation]
			   (rotation-quaternion
			    #;from #f32(0 0 1)
				   #;to (* (if (modifier-pressed? 'shift)
					       -1.0 1.0)
					   axis)))))))
  (keydn 'x (set-axis! #f32(1 0 0)))
  (keydn 'y (set-axis! #f32(0 1 0)))
  (keydn 'z (set-axis! #f32(0 0 1))))

(define view 
  (make <3d-editor> #:x 10 #:y 10 
	#:w (- (screen-width) 10)
	#:h (- (screen-height) 10)
	#:stage *sim-stage*))

(add-child! view #;to *stage*)

;(set! #[view : 'camera : 'position] #f32(0 0 -5))

(define pause #t)

(keydn 'p 
  (lambda () 
    (set! pause (not pause))
    (format #t "pause ~a\n" (if pause 'on 'off))))

(add-timer! 
 25 
 (lambda()
   (unless pause
     (make-simulation-step! *sim*)
     )))

(keydn 'b (lambda ()
	    (display (map rig-bodies (simulation-rigs *sim*))) (newline)))

(publish 
 (define (select-next-body!)
   (let ((bodies (append-map rig-bodies (simulation-rigs *sim*))))
     (select-next-body-from-list! bodies)))
 (define (select-previous-body!)
   (let ((bodies (reverse (append-map rig-bodies (simulation-rigs *sim*)))))
     (select-next-body-from-list! bodies)))
 where
 (define (select-next-body-from-list! bodies)
   (if (not (null? bodies))
       (and-let* ((body (if (null? #[view 'selected])
			    (first bodies)
			    (match (find-tail
				    (equals? 
				     #[(first #[view 'selected]) 'body])
				    bodies)
			      ((this next . rest)
			       next)
			      (else
			       (first bodies)))))
		  (object #[*sim-stage* : '%body=>object : body]))
	 (unselect-all! #;in view)
	 (select-object! object #;from view)))))

(keydn "," select-previous-body!)
(keydn "." select-next-body!)
(keydn 'esc (lambda()(unselect-all! #;in view)))

(set! #[view : 'camera : 'position] 
      #f32(0 1.5 0.2))

(set! #[view : 'camera : 'orientation] 
      (quaternion 0.0 #f32(0.0 0.707106781186548 0.707106781186548)))

(load "config.scm")
