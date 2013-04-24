(define-module (libs physics)
  #:use-module (extra common)
  #:export-syntax (define-rigs-for-sim))

(load-extension "physics" "init")

(use-modules (extra common))

(define-syntax define-rigs-for
  (syntax-rules ()
    ((define-rigs-for sim (rig1-name rig1-def) (rig2-name rig2-def)...)
     (begin (define-rig-for sim (quote rig1-name) rig1-def)
	    (define-rig-for sim (quote rig2-name) rig2-def)
	    ...))))

(define (define-rig-for simulation rig-name rig-def)
  (set-simulation-rig-maker! 
   simulation rig-name
   (lambda (sim) 
     (match rig-def
       (; STRUCTURE
	('rig
	 ('bodies
	  body-spec ...)
	 ('joints
	  joint-defs ...))
	;; ACTION
	(let ((rig (primitive-make-rig sim)))
	  (for (name (type props ...)) in body-spec
	       (let ((body (make-body rig type name)))
		 (for (property value) in (map-n 2 list props)
		      (set-body-property! body 
					  (keyword->symbol property) 
					  value))))
	  (for (type props ...) in joint-defs
	       (let ((joint (make-joint rig type))
		     (literal
		      (lambda (x)
			(match x
			  ((? symbol? body-name)
			   (body-named body-name rig))
			  ((property-name body-name)
			   (body-property (body-named body-name rig)
					  property-name))
			  (else 
			   else)))))
		 (for (property value) in (map-n 2 list props)
		      (display `(set-joint-property! ,joint 
						     ,(keyword->symbol property)
						     ,(literal value))))))
	  rig))))))

(define (make-rig simulation name)
  (let ((rig-maker (simulation-rig-maker simulation name)))
    (if (procedure? rig-maker)
	(rig-maker simulation)
	(throw 'undefined-rig simulation name))))

(define sim (primitive-make-simulation))

(define-rigs-for sim
  ;;(ground (with-input-from-file "rigs/ground.scm" read))
  (buggy '(rig
	   (bodies
	    (chassiss (box #:dimensions #(0.7 0.5 0.2) #:mass 1.0 
			   #:position #(0 0 0.5)))
	    (front-wheel (cylinder #:height 0.2 #:radius 0.5  
				   #:position #(3.5 0 0.4)))
	    (left-rear-wheel (cylinder #:height 0.2 #:radius 0.5  
				       #:position #(3.5 0 0.4)))
	    (right-rear-wheel (cylinder #:height 0.2 #:radius 0.5  
					#:position #(3.5 0 0.4))))
	   (joints
	    (hinge-2 #:body-1 chassiss #:body-2 front-wheel 
		     #:anchor (position front-wheel) #:axis-1 #(0 0 1)
		     #:axis-2 #(0 1 0) #:suspension-erp 0.4
		     #:suspension-cfm 0.8)
	    (hinge-2 #:body-1 chassiss #:body-2 left-rear-wheel
		     #:anchor (position left-rear-wheel) #:axis-1 #(0 0 1)
		     #:axis-2 #(0 1 0) #:suspension-erp 0.4 
		     #:suspension-cfm 0.8 #:lo-stop 0 #:hi-stop 0)
	    (hinge-2 #:body-1 chassiss #:body-2 right-rear-wheel
		     #:anchor (position right-rear-wheel) 
		     #:axis-1 #(0 0 1) #:axis-2 #(0 1 0) 
		     #:suspension-erp 0.4 
		     #:suspension-cfm 0.8 #:lo-stop 0 #:hi-stop 0)))))

(make-rig sim 'buggy)

(define r (primitive-make-rig sim))

(make-body r 'cylinder 'one)

(make-body r 'cylinder 'two)

(define j (make-joint r 'hinge-2))

(set-joint-property! j 'body-2 (body-named 'one r))

(joint-property j 'body-2)


#|
(define (define-rig-for simulation rig-name rig-def)
  (set-simulation-rig-maker! 
   simulation rig-name
   (lambda (sim) 
     (match rig-def
       (; STRUCTURE
	('rig
	 ('bodies
	  body-spec ...)
	 ('joints
	  joint-defs ...))
	;; ACTION
	(let ((rig (primitive-make-rig sim)))
	  (for (name (type props ...)) in body-spec
	       (let ((body (make-body rig type name)))
		 (for (property value) in (map-n 2 list props)
		      (set-body-property! body 
					  (keyword->symbol property) 
					  value))))
	  (for (type props ...) in joint-defs
	       (let ((joint (make-joint rig type))
		     (literal-value (lambda (x)
				      (match x
					((? symbol? body-name)
					 (body-named body-name #:from rig))
					((property-name body-name)
					 (body-property (body-named body-name 
								    #:from rig)
							property-name))
					(else 
					 else)))))
		 (for (property value) in (map-n 2 list props)
		      (set-joint-property! joint (keyword->symbol property)
					   (literal-value value))))
	  rig))))))
|#

;;(with-input-from-port (open-pipe "echo '{S, {NP, {DET, the}, {N, cat}}, {VP {V caught} {NP, {DT, a}, {N, mouse}}}}' | tr '{},' '() '" OPEN_READ) read)
;;(use-modules (ice-9 popen))

;; primitive C functions (to be implemented!)
;; make-rig
;; make-body
;; set-body-property!
;; body-property
;; add-body!
;; make-joint
;; body-named
;; set-joint-property!
;; joint-property
;; add-rig!

#|
(use-modules (oop goops)
	     (ice-9 match)
	     (srfi srfi-1))

(define frequency (make-hash-table))

(define (word-count w)
  (or (hash-ref frequency w) 0))

(define (count-word! w)
  (hash-set! frequency w (1+ (word-count w))))


(define* (terminals syntax-tree #:optional (action list))
  (match syntax-tree
    ((N T)
     (list (action N T)))
    ((N A B)
     (append (terminals A action) (terminals B action)))))

(define* (transform syntax-tree #:optional (action list))
  (match syntax-tree
    ((N T)
     (action N T))
    ((N A B)
     (list N (transform A action) (transform B action)))))

(terminals '(S 
	     (NP (DET the) 
		 (N cat)) 
	     (VP (V caught) 
		 (NP (DT a) 
		     (N mouse))))
	   (lambda(d w)(count-word! w)(list w (word-count w))))

(transform '(S (NP (DET the) (N cat)) (VP (V caught) (NP (DT a) (N mouse))))
	   (lambda (a b) a #;(list a b)))
|#
