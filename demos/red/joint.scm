(define-module (red joint)
  #:use-module (oop goops)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (extra 3d)
  #:use-module (widgets base)
  #:use-module (red object)
  #:export (<physical-joint> describe-joint))

(publish
 (define (hinge . _)
   hinge-mesh)
 where
 (define hinge-mesh
   (match (with-input-from-file "art/3d/hinge.3d" read)
     (('mesh definition ...)
      `(mesh (color #f32(0.7 0.3 0.5 0.7))
	     (with-transforms ((scale-view! 0.1))
			      ,@definition))))))

(publish
 (define (hinge-2 . _)
   hinge-2-mesh)
 where
 (define hinge-2-mesh
   (match (with-input-from-file "art/3d/hinge-2.3d" read)
     (('mesh definition ...)
      `(mesh (color #f32(0.7 0.1 0.6 0.7))
	     (with-transforms ((scale-view! 0.1))
			      ,@definition))))))

(publish
 (define (ball-socket . _)
   ball-socket-mesh)
 where
 (define ball-socket-mesh
   (match (with-input-from-file "art/3d/ball-socket.3d" read)
     (('mesh definition ...)
      `(mesh (color #f32(0.7 0.7 0.0 0.7))
	     (with-transforms ((scale-view! 0.1))
			      ,@definition))))))

(publish
 (define (universal . _)
   universal-mesh)
 where
 (define universal-mesh
   (match (with-input-from-file "art/3d/universal.3d" read)
     (('mesh definition ...)
      `(mesh (color #f32(0.1 0.3 0.7 0.7))
	     (with-transforms ((scale-view! 0.1))
			      ,@definition))))))


(publish
 (define (slider . _)
   slider-mesh)
 where
 (define slider-mesh
   (match (with-input-from-file "art/3d/slider.3d" read)
     (('mesh definition ...)
      `(mesh (color #f32(0.7 0.3 0.1 0.7))
	     (with-transforms ((scale-view! 0.1))
			      ,@definition))))))

(define (no-mesh . _)
  '(mesh 
    (vertices #2f32((0 0 0)(1 0 0)(0 1 0)(0 0 1)))
    (colors #2f32((1 1 1)(1 0 0)(0 1 0)(0 0 1)))
    (faces
     (lines #2u8((0 1)(0 2)(0 3))))))

(define-class <physical-joint> (<editable-object>)
  (default-common-parameters #:allocation #:class
    #:init-value '((max-force . 1.0)
		   (lo-stop . -inf.0)
		   (hi-stop . +inf.0)
		   (velocity . 0.0)
		   (fudge-factor . 1.0)
		   (bounce . 0.3)
		   (cfm . 0.2)
		   (stop-erp . 0.2)
		   (stop-cfm . 0.2)))

  (default-specific-parameters #:allocation #:class
    #:init-value '((ball-socket (anchor . #f32(0 0 0)))
		   (hinge (anchor . #f32(0 0 0))
			  (axis . #f32(0 0 1)))
		   (slider (axis . #f32(1 0 0)))
		   (universal (anchor . #f32(0 0 0))
			      (axis-1 . #f32(1 0 0))
			      (axis-2 . #f32(0 1 0)))
		   (hinge-2 (anchor . #f32(0 0 0))
			    (axis-1 . #f32(1 0 0))
			    (axis-2 . #f32(0 1 0))
			    (angle-1 . 0.0)
			    (hi-stop-2 . 0.0)
			    (velocity-2 . 0.0)
			    (max-force-2 . 0.0)
			    (suspension-erp . 0.0)
			    (suspension-cfm . 0.0)
			    (lo-stop-2 . 0.0))))

  (generators #:allocation #:class
	      #:init-value 
	      `((ball-socket . ,ball-socket)
		(hinge . ,hinge)
		(hinge-2 . ,hinge-2)
		(universal . ,universal)
		(slider . ,slider)
		))

  (default-parameters #:allocation #:virtual
    #:slot-ref (lambda (self)
		 (map (lambda ((name . params))
			`(,name ,@(append params 
					  #[self 'default-common-parameters])))
		      #[self 'default-specific-parameters]))
    #:slot-set! noop)

  (types #:allocation #:virtual
	 #:slot-ref (lambda (self)
		      (map first #[self 'default-parameters]))
	 #:slot-set! noop)

  (%type #:init-value #f)
  (type #:allocation #:virtual
	#:slot-ref (lambda (self) #[self '%type])
	#:slot-set! 
	(lambda (self type)
	  (set! #[self '%type] type)
	  (format #t "choosing ~a from ~a\n" type #[self 'types])
	  (set! #[self 'parameters]
		(copy-tree #[self : 'default-parameters : type]))))
  (parameters #:init-value #f)
  (name #:init-thunk (lambda()(gensym "joint")) #:init-keyword #:name)
  (specific-parameter-editors #:allocation #:class 
			      #:init-value '()) ;; initialized elsewhere
  (body-1 #:init-value #f #:init-keyword #:body-1)
  (body-2 #:init-value #f #:init-keyword #:body-2)
  (mesh
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (let ((type #[self 'type])
	   (parameters #[self 'parameters])
	   (body-1 #[self 'body-1])
	   (body-2 #[self 'body-2]))
       (or #[self : 'mesh-cache : `(,type ,body-1 ,body-2 ,@parameters)]
	   (let ((mesh (apply #[self : 'generators : type] 
			      #:body-1 body-1
			      #:body-2 body-2
			      (alist->keyword-args parameters))))
	     (set! #[self : 'mesh-cache 
			  : (copy-tree `(,type ,body-1 ,body-2 ,@parameters))]
		   mesh)
	     mesh))))
   #:slot-set! noop)

  (position 
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (or #[self : 'parameters : 'anchor]
	 (and #[self 'body-1] #[self 'body-2]
	      (mean #[self : 'body-1 : 'position]
		    #[self : 'body-2 : 'position]))))
   #:slot-set! 
   (lambda (self value)
     (if #[self : 'parameters : 'anchor]
	 (set! #[self : 'parameters : 'anchor] value)))))

(define-method (initialize (self <physical-joint>) args)
  (next-method)
  (set! #[self 'type] (first #[self 'types]))
  (default-slot-values self args 
    (position 
     (or (and #[self 'body-1]
	      #[self 'body-2]
	      (mean #[self : 'body-1 : 'position]
		    #[self : 'body-2 : 'position]))
	 #f32(0 0 0))))
  (format #t "parameters: ~a\n" #[self 'parameters])
  )

(define-method (properties (joint <physical-joint>))
  `(#:body-1  
    ,#[joint : 'body-1 : 'name]
    #:body-2 
    ,#[joint : 'body-2 : 'name]
    ,@(alist->keyword-args #[joint 'parameters])))

(define-method (describe-joint (joint <physical-joint>))
  `(,#[joint 'name] (,#[joint 'type] ,@(properties #;of joint))))
