(define-module (red joint)
  #:use-module (oop goops)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (extra math)
  #:use-module (widgets base)
  #:use-module (red object)
  #:export (<physical-joint> describe-joint))

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
	      `((ball-socket . ,no-mesh)
		(hinge . ,no-mesh)
		(slider . ,no-mesh)
		(universal . ,no-mesh)
		(hinge-2 . ,no-mesh)))

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
	   (parameters #[self 'parameters]))
       (or #[self : 'mesh-cache : `(,type ,@parameters)]
	   (let ((mesh (apply #[self : 'generators : type] parameters)))
	     (set! #[self : 'mesh-cache : (copy-tree `(,type ,@parameters))]
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

(define-method (describe-joint (joint <physical-joint>))
  '())
