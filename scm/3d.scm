(display "loading 3d.scm\n")

(use-modules (extra math))

(define-syntax define-symmetric-method
  (syntax-rules ()
    ((_ (name arg1 arg2) (body ...))
     (begin
       (define-method (name arg1 arg2) (body ...))
       (define-method (name arg2 arg1) (body ...))))))

(define *tolerance* 0.0001)
;; this needs to be fixed in the core guile
(define <point> <top>) 
(define <generalized-vector> <top>)
(define <quaternion> <pair>)

(define (quaternion real imag)
  (cons real imag))

(define (quaternion-real q)
  (car q))

(define (quaternion-imag q)
  (cdr q))

(define-method (re (q <quaternion>))
  (quaternion-real q))

(define-method (im (q <quaternion>))
  (quaternion-imag q))

(define-method (re (c <complex>))
  (real-part c))

(define-method (im (c <complex>))
  (imag-part c))

(define (array-map proc first . rest)
  (let ((dest (apply make-typed-array (array-type first) *unspecified* 
		     (array-dimensions first))))
    (apply array-map! dest proc first rest)
    dest))

(define-method (+ (p <point>) . rest)
  (apply array-map + p rest))

(define-method (- (p <point>) . rest)
  (apply array-map - p rest))

; this has to be added to support unary minus, as guile probably 
; implements it like as (define (- (first <number>)) (- 0 first)) ...
(define-method (- (n <number>) (p <point>))
  (array-map (lambda(x)(- n x)) p)) 

(define (quaternion-multiply2 p q)
  (cons (- (* (re p) (re q))
	   (* (im p) (im q)))
	(+ (* (re p) (im q))
	   (* (re q) (im p))
	   (* (wedge3x3 (im p) (im q))))))

(define-method (* (p <quaternion>) . rest)
  (fold (lambda(q p)(quaternion-multiply2 p q)) p rest))

(define-generic ~)

(define-method (~ (c <complex>))
  (make-rectangular (real-part c) (- (imag-part c))))

(define-method (~ (q <quaternion>))
  (cons (car q) (- (cdr q))))

(define-symmetric-method (* (s <number>) (p <point>))
  (array-map (lambda(x)(* x s)) p))

(define-method (* (m <array>) (v <generalized-vector>))
  (matrix-vector-mul m v))

(define-method (* (u <generalized-vector>) (v <generalized-vector>))
  (dot u v))

(define-method (* (p <point>))
  p)

(define-method (* (m1 <array>) (m2 <array>) . rest)
  (apply matrix-mul m1 (cons m2 rest)))

(define-class <3d> ()
  (position #:init-value #f32(0 0 0))
  (orientation #:init-value '(0 . #f32(1 0 0))))

(define-class <3d-cam> (<3d>)
  (fovy #:init-value 70.0))

(define-class <3d-shape> (<3d>)
  (shape #:init-value '()))

(define-class <3d-mesh> (<3d-shape>)
  (mesh #:init-thunk 
	(lambda()(with-input-from-file "3d/cube.3d" read))))


(define (draw-mesh mesh)
  (match mesh
    (('mesh . definition)
     (for-each (match-lambda
		(('vertices (? array? array))
		 (set-vertices-array! array))
		(('colors (? array? array))
		 (set-colors-array! array))
		(('faces . faces)
		 (for-each (match-lambda 
			    ((type array)
			     (draw-faces! type array)))
			   faces)))
	       definition))
    (else
     (display `(no-match ,else)))))
		 
  
(define-method (draw (object <3d-mesh>))
  (push-matrix!)
  (translate-view! #[object 'position])
  (rotate-view! #[object 'orientation])
  (draw-mesh #[object 'mesh])
  (pop-matrix!))

(define-class <3d-view> (<widget>)
  (camera #:init-thunk (lambda()(make <3d-cam>)))
  (objects #:init-value '()))

(define-method (draw (view <3d-view>))
  (let ((original-viewport (current-viewport)))
    (set-viewport! #[view 'x] #[view 'y] #[view 'w] #[view 'h])
    (push-matrix!)
    (perspective-projection! #[view : 'camera : 'fovy])
    (translate-view! #[view : 'camera : 'position])
    (rotate-view! #[view : 'camera : 'orientation])
    (for object in #[view 'objects]
	 (draw object))
    (pop-matrix!)
    (apply set-viewport! original-viewport)))

(define-method (add-object! (view <3d-view>) (object <3d>))
  (set! #[view 'objects] (cons object #[view 'objects])))

(display "loaded 3d.scm\n")
