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

(define (array-map proc first . rest)
  (let ((dest (apply make-typed-array (array-type first) *unspecified* 
		     (array-dimensions first))))
    (apply array-map! dest proc first rest)
    dest))

(define-method (+ (p <point>) . rest)
  (apply array-map + p rest))

(define-method (- (p <point>) . rest)
  (apply array-map - p rest))

(define-method (- (n <number>) (p <point>)) ; this has to be added to support
  (array-map (lambda(x)(- n x)) p)) ; unary minus, as guile probably implements
;; it like that (define (- first . rest) (if (null? rest) (- 0 first) ...))

(define-symmetric-method (* (s <number>) (p <point>))
  (array-map (lambda(x)(* x s)) p))

(define-method (* (m <array>) (v <generalized-vector>))
  (matrix-vector-mul m v))

(define-method (* (u <generalized-vector>) (v <generalized-vector>))
  (dot u v))

(define-method (* (m1 <array>) (m2 <array>) . rest)
  (apply matrix-mul m1 (cons m2 rest)))

(define-class <3d> ()
  (position #:init-value #(0 0 0))
  (orientation #:init-value #(1 0 0 0))
  (shape #:init-value '())
  (mesh #:init-value '()))

(define-method (draw (object <3d>))
  (translate-view! #[object 'position])
  (rotate-view! #[object 'orientation]))

(define-class <3d-view> (<widget>)
  (camera #:init-thunk (lambda()(make <3d>)))
  (objects #:init-value '()))

(define-method (draw (view <3d-view>))
  (let ((original-viewport (current-viewport)))
    (set-viewport! #[view 'x] #[view 'y] #[view 'w] #[view 'h])
    (push-matrix!)
    (translate-view! #[#[view 'camera] 'position])
    (rotate-view! #[#[view 'camera] 'orientation])
    (for object in #[view 'objects]
	 (draw object))
    (pop-matrix!)
    (apply set-viewport! original-viewport)))

(display "loaded 3d.scm\n")
