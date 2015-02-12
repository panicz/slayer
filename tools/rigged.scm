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
	     (editor rigged object)
	     (editor rigged body)
	     (editor rigged joint)
	     (editor relations)
	     )

(set-window-title! "RIGGED: The RIG Editor")

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
  (let ((bodies joints (partition (λ(x)(is-a? x <physical-body>))
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
 (define body (make <physical-body> #:name '<<no-body-selected>>))
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
 ;;(add-object! body #;to the-rig)
 (set! #[body 'dimension-editors]
       (map (lambda ((name . parameters))
	      `(,name . ,(apply 
			  (layout #:lay-out lay-out-horizontally)
			  (map (lambda ((param . _))
				 (make <numeric-input> #:w 80 #:h 12
				       #:label 
				       (string-append
					(symbol->string param) ": ")
				       #:accessor
				       (accessor x #[x : 'dimensions : param])))
			 parameters))))
	    #[body 'default-dimensions])))

(add-child! body-editor #;to *stage*)

(publish
 (define joint-editor
   (apply (layout #:x 640 #:y (+ #[body-editor 'y] #[body-editor 'h] 20))
	  `(,name-editor
	    ,@(map (lambda ((name . default-value))
		     (make <numeric-input> #:w 180 #:h 12 
			   #:label (string-append (symbol->string name) ": ")
			   #:taget joint
			   #:accessor (accessor joint 
						#[joint : 'parameters : name])))
		   #[joint 'default-common-parameters])
	    ,position-editor
	    ,specific-parameter-editor
	    )))
 (define the-joint-specific-parameter-editor specific-parameter-editor)
 where
 (define joint (make <physical-joint>))
 (define name-editor
   (property-editor
    joint
    ("name: " #[joint 'name])))

 (define position-editor
   (parameter-editor 
    joint
    (" x: " #[joint : 'position : 0])
    (" y: " #[joint : 'position : 1])
    (" z: " #[joint : 'position : 2])))
 (define specific-parameter-editor
   (make <container-widget> #:min-h 12))
 (set! #[joint 'specific-parameter-editors]
       (map (lambda ((name . parameters))
	      `(,name 
		. ,(apply
		    (layout #:lay-out lay-out-vertically)
		    (map (lambda ((param . _))
			   (make <numeric-input> #:w 180 #:h 12
				 #:label
				 (string-append (symbol->string param) ": ")
				 #:accessor
				 (accessor x #[x : 'parameters : param])))
			 parameters))))
	    #[joint 'default-specific-parameters]))
 )

(add-child! joint-editor #;to *stage*)

(define-method (select-object! (o <physical-body>) #;from (v <3d-editor>))
  (next-method)
  (set! #[the-dimension-editor 'content]
	#[o : 'dimension-editors : #[o 'shape]])
  (set-target! #;of body-editor #;as o))

(define-method (select-object! (o <physical-joint>) #;from (v <3d-editor>))
  (next-method)
  (set! #[the-joint-specific-parameter-editor 'content]
	#[o : 'specific-parameter-editors : #[o 'type]])
  (set-target! #;of joint-editor #;as o)
  (format #t "selecting a joint ~a\n" o))

(define view (make <3d-editor>
	       #:x 10 #:y 10 
	       #:w 620
	       #:h 460
	       #:stage the-rig))

(add-child! view #;to *stage*)

;;(<< #[*stage* 'x] ", " #[*stage* 'y] ", " #[*stage* 'w] ", " #[*stage* 'h])

(define add-new? #f)

(keydn 'n (lambda () (set! add-new? #t)))

(define-method (set-body-shape! #;of (body <physical-body>) 
				     #;to (shape <symbol>) . args)
  (set! #[body 'shape] shape)
  (set! #[body 'dimensions]
	(replace-alist-bindings #;in #[body 'dimensions]
				     #;with (keyword-args->alist args)))
  (set! #[the-dimension-editor 'content]
	#[body : 'dimension-editors : shape])
  (set-target! #;of the-dimension-editor #;as body))

(define-method (shift-body-shape! (body <physical-body>) #;by (n <integer>))
  (let* ((shapes #[body 'shapes])
	 (l (length shapes))
	 (new-shape (list-ref shapes
			      (modulo (+ (list-index (equals? #[body 'shape])
						     shapes)
					 n)
				      l))))
    (set-body-shape! body new-shape)))

(define-method (set-joint-type! #;of (joint <physical-joint>)
				     #;to (type <symbol>) . args)
  (let ((old-position #[joint 'position]))
    (set! #[joint 'type] type)
    (set! #[joint 'parameters]
	  (replace-alist-bindings #;in #[joint 'parameters]
				       #;with (keyword-args->alist args)))
    (set! #[the-joint-specific-parameter-editor 'content]
	  #[joint : 'specific-parameter-editors  : type])
    (set-target! #;of the-joint-specific-parameter-editor #;as joint)
    (set! #[joint 'position] old-position)))

(define-method (shift-joint-type! (joint <physical-joint>) #;by (n <integer>))
  (let* ((types #[joint 'types])
	 (l (length types))
	 (new-type (list-ref types
			     (modulo (+ (list-index (equals? #[joint 'type])
						    types)
					n)
				     l))))
    (set-joint-type! joint new-type)))

(set! #[view 'left-click]
      (lambda (x y)
	(if (or (>= (length #[view 'selected]) 2)
		(any (λ(x)(is-a? x <physical-joint>)) #[view 'selected]))
	    (unselect-all! view))
	(let ((object (object-at-position x y view)))
	  (if object
	      (select-object! object #;from view)
	      (when add-new?
		(add-object! 
		 (make <physical-body> #:position (screen->3d view x y 0.95))
		 #;to the-rig)
		(set! add-new? #f)
		)))))

(set! #[view 'drag]
      (lambda(x y dx dy)
	(relative-turn! #[view 'camera] (- dx) (- dy))))

(keydn 'esc (lambda () (unselect-all! view)))

(keydn 'j (lambda ()
	    (and-let* ((selected #[view 'selected])
		       ((= (length selected) 2)))
	      (add-joint! #;between (first selected) #;and (second selected)
				    #;to the-rig))))

(keydn '/ 
  (lambda () 
    (let ((file (next-available-file-name "the-rig")))
      (with-output-file file
	(pretty-print (describe-rig the-rig)))
      (format #t "rig saved to ~s\n" file))))


(keydn '\
  (lambda ()
    (pretty-print (describe-rig the-rig))))

(keydn 'g (grab-mode view))

(define-syntax-rule (with-context-for-joint/body-relation action . *)
  (specify ((joint-property-getter 
	     (lambda (joint property)
	       (assert (in? property '(body-1 body-2)))
	       #[joint property]))
	    (body-rig-getter 
	     (lambda (body)
	       #[body 'rig]))
	    (rig-joints-getter 
	     (lambda (rig)
	       (filter (lambda(x)
			 (is-a? x <physical-joint>))
		       #[rig 'objects]))))
    action . *))

(keydn 'h 
  (rotate-mode
   view 
   #:center 
   (lambda(selected)
     (with-context-for-joint/body-relation
      (or (and-let* ((selected-bodies (filter (lambda(x)
						(is-a? x <physical-body>))
					      selected))
		     ((not (null? selected-bodies)))
		     (first-body (last selected-bodies))
		     (joints (joints-attached-to first-body))
		     (joints-attaching-unselected-bodies
		      (filter (lambda(joint)
				(not (in? (body-attached-by joint 
							    #;to first-body)
					  selected-bodies)))
			      joints))
		     ((not (null? joints-attaching-unselected-bodies))))
	    #[(first joints-attaching-unselected-bodies) 'position])
	  (apply mean (map #[_ 'position] selected)))))))

(keydn 't 
  (lambda()
    (with-context-for-joint/body-relation
     (match #[view 'selected]
       ((first second . _)
	(if (and (is-a? first <physical-body>)
		 (is-a? second <physical-body>)
		 (bodies-are-connected? first second))
	    (let*-values (((joint) (joint-connecting-bodies 
				    first second))
			  ((left right) (split-bodies-at joint))
			  ((move still) (cond ((in? first left)
					       (values left right))
					      ((in? first right)
					       (values right left))
					      (else
					       (error)))))
	      (unselect-all! #;from view)
	      (for object in move
		(select-object! object #;from view)))
	    #;else
	    (display "operation requires two bodies connected with\
 a joint to be selected\n")))
       (else
	(display "at least two objects need to be selected\n")
	)))))

(keydn 'delete (lambda () (delete-selected-objects! #;in view)))

(keydn ","
  (lambda ()
    (match #[view 'selected]
      ((object)
       (cond ((is-a? object <physical-body>)
	      (shift-body-shape! object -1))
	     ((is-a? object <physical-joint>)
	      (shift-joint-type! object -1))
	     ))
      (else
       (display "Exactly one object needs to be selected in order to change\
 its type")))))

(keydn "."
  (lambda ()
    (match #[view 'selected]
      ((object)
       (cond ((is-a? object <physical-body>)
	      (shift-body-shape! object +1))
	     ((is-a? object <physical-joint>)
	      (shift-joint-type! object +1))
	     ))
      (else
       (display "Exactly one object needs to be selected in order to change\
 its type")))))

(if (and (defined? '$1) (file-exists? $1))
    (match (with-input-from-file $1 read)
      (; STRUCTURE
       ('rig
	('bodies body-spec ...)
	('joints joint-defs ...))
       ;; ACTION
       (let ((bodies #[]))
	 (for (name (shape props ...)) in body-spec
	   (let ((body (apply make <physical-body> #:shape shape props)))
	     (set! #[bodies name] body)
	     (set! #[body 'name] name)
	     (add-object! body #;to the-rig)))
	 (for (name (type props ...)) in joint-defs
	   (let ((joint (apply 
			 make <physical-joint> 
			 (alist->keyword-args 
			  (map (lambda ((property . value))
				 (match value
				   ((? symbol? body-name)
				    `(,property . ,#[bodies body-name]))
				   (('? property-name body-name)
				    `(,property . ,#[#[bodies body-name] 
						     property-name]))
				   (else
				    `(,property . ,value))
				   ))
			       (keyword-args->alist props))))))
	     (set! #[joint 'name] name)
	     (add-object! joint #;to the-rig)))))))

(let ((camera #[view 'camera]))
  (set! #[camera 'position] #f32(0 -6 -0.7))
  (set! #[camera 'orientation] (normalized '(1.0 . #f32(1 0 0)))))

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

(set! #[view 'drag] (lambda (x y dx dy)
		      (relative-turn! #[view 'camera] (- dx) (- dy))))
