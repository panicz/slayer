#!../src/slayer -e3d
exit
!#

(use-modules (slayer) 
	     (slayer 3d) 
	     (extra common)
	     (extra ref)
	     (extra 3d)
	     (extra slayer)
	     (extra math)
	     (extra figures)
	     (scum physics)
	     (oop goops)
	     (widgets base)
	     (widgets text-area)
	     (widgets sprite)
	     (widgets 3d))

(keydn 'esc quit)

(set-window-title! "RED")

;; TODO LIST:
;; - zapis rigów do pliku, odczyt rigów z pliku
;; - dodawanie ciał, usuwanie ciał, przesuwanie i obracanie
;; - zmiana kształtów ciał, w tym
;;   - zmiana typu (kula, walec, prostopadłościan)
;;   - zmiana parametrów (wymiary, promień, wysokość) -- osobne okienko
;; - łączenie dwóch ciał więzami; odpowiednia zmiana pozycji
;;   więzów
;; - zmiana parametrów więzów

(define-class <physical-body> (<3d-object>)
  (mesh-cache #:allocation #:class #:init-value #[])
  (default-dimensions #:allocation #:class
    #:init-value '((sphere (radius . 0.5))
		   (box (x . 0.5) (y . 0.5) (z . 0.5))
		   (cylinder (radius . 0.5) (height . 1.0))))
  (generators 
   #:allocation #:class
   #:init-value 
   `((sphere . ,generate-sphere)
     (cylinder . ,generate-tube)
     (box . ,generate-box)))
  (shapes
   #:allocation #:virtual
   #:slot-ref (lambda(self)(map first #[self 'generators]))
   #:slot-set! noop)
  (%shape #:init-value 'box #:init-keyword #:shape)
  (dimensions #:init-value #f #:init-keyword #:dimensions)
  (shape 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     #[self '%shape])
   #:slot-set!
   (lambda (self shape)
     (set! #[self '%shape] shape)
     (set! #[self 'dimensions] #[self : 'default-dimensions : shape])))
  (mass #:init-value 1.0 #:init-keyword #:mass)
  (center-of-mass #:init-value #f32(0 0 0) #:init-keyword #:center-of-mass)
  (inertia-tensor #:init-value #2f32((1 0 0)
				     (0 1 0)
				     (0 0 1)) #:init-keyword #:inertia-tensor)
  (mesh 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (let ((shape #[self '%shape])
	   (dimensions #[self 'dimensions]))
       (or #[self : 'mesh-cache : `(,shape ,@dimensions)]
	   (let ((mesh (apply #[self : 'generators : shape] 
			      (append-map (lambda ((name . value))
					    `(,(symbol->keyword name) ,value))
				   dimensions))))
	     (set! #[self : 'mesh-cache : `(,shape ,@dimensions)] mesh)
	     mesh))))
   #:slot-set! noop))


#;(parameter-editor  '((shape #:one-of ())
		     (dimensions #:dynamic (shape))
		     mass
		     center-of-mass
		     inertia-tensor))



#|
|#


(define-method (initialize (self <physical-body>) args)
  (next-method)
  (if (not #[self 'dimensions])
      (set! #[self 'dimensions]
	    #[self : 'default-dimensions : #[self 'shape]])))

(define-method (set-body-shape! (body <physical-body>) (shape <symbol>) . args)
  (set! #[body 'shape] shape)
  (set! #[body 'dimensions] 
	(replace-alist-bindings #;in #[body 'dimensions] 
				     #;with (keyword-args->alist args))))

;;(define-method (set-previous-body-shape! (body <physical-body>))

#;(define-class <joint> (<3d-object>)
  ...)

(define the-rig (make <3d-stage>))

(add-object! (make <physical-body>) #;to the-rig)

(define view (make <3d-editor> 
	       #:x 10 #:y 10 
	       #:w (- (screen-width) 20) 
	       #:h (- (screen-height) 20)
	       #:stage the-rig))

(set-screen-size! 800 480)

(add-child! view #;to *stage*)

#|
(add-child! 
 (layout:
  )   
 #;to *stage*)
|#

(set! #[view 'left-click]
      (lambda (x y)
	(unselect-all! view)
	(let ((object (object-at-position x y view)))
	  (if object
	      (select-object! view object)
	      (add-object! 
	       (make <physical-body> #:position (screen->3d view x y 0.95))
	       #;to the-rig)))))

(set! #[view 'drag]
      (lambda(x y dx dy)
	(relative-turn! #[view 'camera] (- dx) (- dy))))

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

(keydn "1" (lambda () 
	     (set! #[view : 'camera : 'orientation] 
		   `(0.0 . #f32(1 0 0)))))

(keydn 'g (grab-mode view))
(keydn 'h (rotate-mode view))
(keydn 'delete (lambda () (delete-selected-objects! #;in view)))

(define-method (shift-body-shape! (body <physical-body>) #;by (n <integer>))
  (let* ((shapes #[body 'shapes])
	 (l (length shapes))
	 (new-shape (list-ref shapes
			      (modulo (+ (list-index (equals? #[body 'shape])
						     shapes)
					 n)
				      l))))
    (set-body-shape! body new-shape)))

(keydn ","
  (lambda ()
    (match #[view 'selected]
      ((body)
       (if (is-a? body <physical-body>)
	   (shift-body-shape! body -1)))
      (else
       (display "Exactly one object needs to be selected in order to change \
its type")))))

(keydn "."
  (lambda ()
    (match #[view 'selected]
      ((body)
       (if (is-a? body <physical-body>)
	   (shift-body-shape! body +1)))
      (else
       (display "Exactly one object needs to be selected in order to change \
its type")))))
