#!../src/slayer -w 900
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

(define-class <editable-object> (<3d-object>))

(define-class <physical-body> (<editable-object>)
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
  (dimension-editors 
   #:allocation #:class
   #:init-value '()) ;; initalized elsewhere
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
     (set! #[self 'dimensions] (copy-tree #[self : 'default-dimensions : shape]))))
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
	     (format (current-error-port)"new mesh: ~a\n"`(,shape ,@dimensions))
	     (set! #[self : 'mesh-cache : (copy-tree `(,shape ,@dimensions))] mesh)
	     mesh))))
   #:slot-set! noop))

(define-method (initialize (self <physical-body>) args)
  (next-method)
  (if (not #[self 'dimensions])
      (set! #[self 'dimensions]
	    (copy-tree #[self : 'default-dimensions : #[self 'shape]]))))

(define-method (set-body-shape! #;of (body <physical-body>) 
				     #;to (shape <symbol>) . args)
  (set! #[body 'shape] shape)
  (set! #[body 'dimensions]
	(replace-alist-bindings #;in #[body 'dimensions]
				     #;with (keyword-args->alist args)))
  (set! #[the-dimension-editor 'content]
	#[body : 'dimension-editors : shape])
  (set-target! #;of the-dimension-editor #;as body))

(define the-rig (make <3d-stage>))

(publish
 ;; tu jest -- w istocie, zgodzię się -- za dużo pisaniny!
 ;; edytor powinien budować się sam na podstawie opisu obiektu
 (define body-editor
   ((layout #:x 640 #:y 20)
    position-editor 
    orientation-editor
    mass-editor
    inertia-tensor-1
    inertia-tensor-2
    inertia-tensor-3
    dimension-editor))
 (define the-dimension-editor dimension-editor)
 where
 (define body (make <physical-body>))
 (define position-editor
   (parameter-editor 
    body
    (" x: " #[body : 'position : 0])
    (" y: " #[body : 'position : 1])
    (" z: " #[body : 'position : 2])))
 (define orientation-editor
   (parameter-editor
    body
    (" s: " (head #[body 'orientation]))
    (" i: " #[(tail #[body 'orientation]) 0])
    (" j: " #[(tail #[body 'orientation]) 1])
    (" k: " #[(tail #[body 'orientation]) 2])))
 (define mass-editor
   (parameter-editor
    body
    ("mass: " #[body 'mass])
    (" x: " #[body : 'center-of-mass : 0])
    (" y: " #[body : 'center-of-mass : 1])
    (" z: " #[body : 'center-of-mass : 2])))
 (define inertia-tensor-1 
   (parameter-editor
    body 
    ("t" #[#[body 'inertia-tensor] 0 0])
    ("e" #[#[body 'inertia-tensor] 0 1])
    ("n" #[#[body 'inertia-tensor] 0 2])))
 (define inertia-tensor-2 
   (parameter-editor
    body 
    ("s" #[#[body 'inertia-tensor] 1 0])
    ("o" #[#[body 'inertia-tensor] 1 1])
    ("r" #[#[body 'inertia-tensor] 1 2])))
 (define inertia-tensor-3
   (parameter-editor
    body 
    ("i" #[#[body 'inertia-tensor] 2 0])
    ("n" #[#[body 'inertia-tensor] 2 1])
    ("e" #[#[body 'inertia-tensor] 2 2])))
 (define dimension-editor
   (make <container-widget> #:min-h 12))
 (add-object! body #;to the-rig)
 (keydn 'v (lambda ()
	     (<< #[body 'dimensions])))
 (set! #[body 'dimension-editors]
       (map (lambda ((name . parameters))
	      `(,name
		.
		,(apply 
		  (layout #:lay-out lay-out-horizontally)
		  (map (lambda ((param . _))
			 (make <numeric-input> #:w 80 #:h 12
			       #:label 
			       (string-append (symbol->string param) ": ")
			       #:accessor
			       (accessor x #[x : 'dimensions : param])))
		       parameters))))
	    #[body 'default-dimensions])))

(add-child! body-editor #;to *stage*)


(define-method (select-object! (o <editable-object>) #;from (v <3d-editor>))
  (next-method)
  (set! #[the-dimension-editor 'content]
	#[o : 'dimension-editors : #[o 'shape]])
  (set-target! #;of body-editor #;as o))

(define view (make <3d-editor> 
	       #:x 10 #:y 10 
	       #:w 620
	       #:h 460
	       #:stage the-rig))

(add-child! view #;to *stage*)

;;(<< #[*stage* 'x] ", " #[*stage* 'y] ", " #[*stage* 'w] ", " #[*stage* 'h])

(set! #[view 'left-click]
      (lambda (x y)
	(unselect-all! view)
	(let ((object (object-at-position x y view)))
	  (if object
	      (select-object! object #;from view)
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
