#!../src/slayer -w 900
exit
!#

;; this code is horrible and don't look at it!

(set! %load-path (append '("." "./guile-modules" ".." "../guile-modules") 
			 %load-path))

;; (use-modules (oop goops)(extra common)(extra ref))


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
	     (widgets 3d)
	     (red body)
	     (red joint)
	     )

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
(define-method (add-joint! #;between (body-1 <physical-body>)
				     #;and (body-2 <physical-body>)
					   #;to (rig <3d-stage>))
  (let ((joint (make <physical-joint> #:body-1 body-1 #:body-2 body-2)))
    (add-object! joint #;to rig)))



(define the-rig (make <3d-stage>))

(define-method (describe-rig (stage <3d-stage>))
  (let-values (((bodies joints) (partition (λ(x)(is-a? x <physical-body>))
					   #[stage 'objects])))
    `(rig
      (bodies
       ,@(map describe-body bodies))
      (joints
       ,@(map describe-joint joints)))))

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
    name-editor
    dimension-editor))
 (define the-dimension-editor dimension-editor)
 where
 (define body (make <physical-body>))
 (define name-editor
   (property-editor
    body
    ("name: " #[body 'name])))
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
    ("mass:" #[body 'mass])
    (" x: " #[body : 'center-of-mass : 0])
    (" y: " #[body : 'center-of-mass : 1])
    (" z: " #[body : 'center-of-mass : 2])))
 (define inertia-tensor-1 
   (parameter-editor
    body 
    (" te " #[#[body 'inertia-tensor] 0 0])
    (" ns " #[#[body 'inertia-tensor] 0 1])
    (" or " #[#[body 'inertia-tensor] 0 2])))
 (define inertia-tensor-2 
   (parameter-editor
    body 
    ("    " #[#[body 'inertia-tensor] 1 0])
    (" of " #[#[body 'inertia-tensor] 1 1])
    ("  i " #[#[body 'inertia-tensor] 1 2])))
 (define inertia-tensor-3
   (parameter-editor
    body 
    (" ne " #[#[body 'inertia-tensor] 2 0])
    (" rt " #[#[body 'inertia-tensor] 2 1])
    (" ia " #[#[body 'inertia-tensor] 2 2])))
 (define dimension-editor
   (make <container-widget> #:min-h 12))
 (add-object! body #;to the-rig)
 (keydn 'v (lambda ()
	     (<< #[body 'dimensions])))
 (set! #[body 'dimension-editors]
       (map (lambda ((name . parameters))
	      `(,name
		. ,(apply 
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

(publish
 (define joint-editor
   (apply (layout #:x 640 #:y (+ #[body-editor 'y] #[body-editor 'h] 100))
	  (map (lambda ((name . default-value))
		 (make <numeric-input> #:w 120 #:h 12 
		       #:label (symbol->string name)
		       #:taget joint
		       #:accessor (accessor joint name)))
	       #[joint 'default-common-parameters])))
 where
 (define joint (make <physical-joint>)))

(define-method (select-object! (o <physical-body>) #;from (v <3d-editor>))
  (next-method)
  (set! #[the-dimension-editor 'content]
	#[o : 'dimension-editors : #[o 'shape]])
  (set-target! #;of body-editor #;as o))

(define-method (select-object! (o <physical-joint>) #;from (v <3d-editor>))
  (next-method)
  (format #t "selecting a joint ~a\n" o))

(define view (make <3d-editor>
	       #:x 10 #:y 10 
	       #:w 620
	       #:h 460
	       #:stage the-rig))

(add-child! view #;to *stage*)

;;(<< #[*stage* 'x] ", " #[*stage* 'y] ", " #[*stage* 'w] ", " #[*stage* 'h])

(set! #[view 'left-click]
      (lambda (x y)
	(if (or (>= (length #[view 'selected]) 2)
		(any (λ(x)(is-a? x <physical-joint>)) #[view 'selected]))
	    (unselect-all! view))
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

(keydn 'esc (lambda () (unselect-all! view)))

(keydn 'j (lambda ()
	    (and-let* ((selected #[view 'selected])
		       ((= (length selected) 2)))
	      (add-joint! #;between (first selected) #;and (second selected)
				    #;to the-rig))))

(keydn '/ (lambda () (pretty-print (describe-rig the-rig))))

(keydn "1" (lambda () 
	     (set! #[view : 'camera : 'orientation] 
		   `(0.0 . #f32(1 0 0)))))

(keydn 'g (grab-mode view))
(keydn 'h (rotate-mode view))
(keydn 'delete (lambda () (delete-selected-objects! #;in view)))

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
