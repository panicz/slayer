(define-module (extra math)
  :use-module (ice-9 optargs)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-17)
  :use-module (extra common)
  :use-module (extra ref)
  :use-module (oop goops)
  :duplicates (merge-generics);; replace warn-override-core warn first)
  :use-module ((rnrs) :version (6))  
  :export (eye transpose reciprocal
	   rows columns row column
	   matrix->vector vector->matrix
	   vectors->matrix
	   dot norm square project
	   normalize! normalized
	   det3x3 inv3x3 wedge3x3 crossm3x3
	   matrix-mul matrix-vector-mul
	   sgn
	   pi/4 pi/2 pi 2pi π/4 π/2 π 2π e
	   deg->rad rad->deg
	   multiply add subtract divide
	   <point>
	   <generalized-vector>
	   <quaternion> 
	   quaternion quaternion-real quaternion-imag re im ~
	   rotation-quaternion
	   TOLERANCE
	   ))

(define TOLERANCE (make-fluid 0.0001))

(define <point> <uvec>) 

(define <generalized-vector> <top>)

(define <quaternion> <pair>)

(define (quaternion real imag)
  (cons real imag))

(define (quaternion-real q)
  (car q))

(define (quaternion-imag q)
  (cdr q))

(define-generic re)

(define-generic im)

(define-method (re (q <quaternion>))
  (quaternion-real q))

(define-method (im (q <quaternion>))
  (quaternion-imag q))

(define-method (re (c <complex>))
  (real-part c))

(define-method (im (c <complex>))
  (imag-part c))

(define (quaternion-multiply2 p q)
  (cons (- (* (re p) (re q))
	   (* (im p) (im q)))
	(+ (* (re p) (im q))
	   (* (re q) (im p))
	   (* (wedge3x3 (im p) (im q))))))

(define (quaternion-add2 p q)
  (quaternion (+ (re p) (re q)) (+ (im p) (im q))))

(define pi/4 (atan 1))

(define π/4 pi/4)

(define pi/2 (acos 0))

(define π/2 pi/2)

(define pi (* 2 pi/2))

(define π pi)

(define 2pi (* 2 pi))

(define 2π 2pi)

(define e (exp 1))

(define (deg->rad x) (* x (/ pi 180)))

(define (rad->deg x) (* x (/ 180 pi)))

(define (sgn x) 
  (cond ((> x 0) 1)
	((< x 0) -1)
	(#t 0)))

(define* (eye size #:optional (type #t))
  (let ((m (make-typed-array type 0 size size)))
    (let loop ((i 0))
      (if (< i size)
	(begin 
	  (array-set! m 1 i i)
	  (loop (1+ i)))))
    m))

(define (transpose a)
  (transpose-array a 1 0))

(define (rows a) 
  (car (array-dimensions a)))

(define (columns a) 
  (cadr (array-dimensions a)))

(define (row a i) 
  (make-shared-array 
   a 
   (lambda (x) (list i x)) 
   (list 0 (1- (columns a)))))

(define (column a i) 
  (make-shared-array 
   a 
   (lambda (x) (list x i)) 
   (list 0 (1- (rows a)))))

(define (dot a b) 
  (let ((sum 0))
    (array-for-each 
     (lambda(x y) (set! sum (+ sum (* x y)))) a b)
    sum))

(define (square v)
  (dot v v))

(define-method (norm v)
  (sqrt (square v)))

(define-method (normalize! v)
  (let ((lv (norm v)))
    (array-map! v (lambda(x)(/ x lv)) v)))

(define-method (normalized v)
  (let ((lv (norm v))
	(u (apply make-typed-array (array-type v) (if #f #f) 
		  (array-dimensions v))))
    (array-map! u (lambda(x)(/ x lv)) v)
    u))

(define-method (normalized (q <quaternion>))
  (match-let* (((re . im) q)
	       (1/norm (/ 1 (sqrt (+ (* re re) (* im im))))))
    (quaternion (* 1/norm re) (* 1/norm im))))

(define* (project source #:optional destination 
		  #:key (onto destination)) ; project src onto dest
  (let ((s (/ (dot source onto) (square onto))))
    (array-map (lambda(x)(* x s)) onto)))

(define (matrix-mul2 a b)
  (assert (= (columns a) (rows b)))
  (let ((result (make-typed-array (array-type a) (if #f #f) 
				  (rows a) (columns b))))
    (array-index-map! 
     result 
     (lambda(i j)(dot (column b j) (row a i))))
    result))

(define (vector->matrix v)
  (let* ((vector-length 
	  (if (uniform-vector? v) 
	      uniform-vector-length 
	      vector-length))
	 (vl (vector-length v)))
    (make-shared-array v (lambda(i j)(list i)) vl 1)))

(define (matrix->vector V)
  (cond ((= (columns V) 1) (column V 0))
	((= (rows V) 1) (row V 0))
	(#t (error "unable to convert matrix to vector" V))))

(define (vectors->matrix . vectors)
  (transpose 
   (apply array-append 
	  (map transpose 
	       (map vector->matrix vectors)))))

(define (matrix-vector-mul M v)
  (matrix-mul2 M (vector->matrix v)))

(define (matrix-mul first . rest) 
  (fold (lambda(b a)(matrix-mul2 a b)) first rest))

(define (det3x3 M)
  (assert (and (array? M) (= (columns M) (rows M) 3)))
  (+ (* #[M 1 1] (- (* #[M 0 0] #[M 2 2]) (* #[M 2 0] #[M 0 2])))
     (* #[M 2 1] (- (* #[M 1 0] #[M 0 2]) (* #[M 0 0] #[M 1 2])))
     (* #[M 0 1] (- (* #[M 2 0] #[M 1 2]) (* #[M 1 0] #[M 2 2])))))

(define (wedge3x3 u v)
  (let ((vector-length 
	 (if (uniform-vector? v) 
	     uniform-vector-length 
	     vector-length)))
    #;(assert (and (or (and (vector? u) (vector? v))
		     (and (uniform-vector? u) (uniform-vector? v)))
		 (= (vector-length u) (vector-length v) 3)))
    (list->typed-array 
     (array-type u) 1 
     (list (- (* #[u 1] #[v 2])(* #[u 2] #[v 1]))
	   (- (* #[u 2] #[v 0])(* #[u 0] #[v 2]))
	   (- (* #[u 0] #[v 1])(* #[u 1] #[v 0]))))))

(define (crossm3x3 v)
  (list->typed-array (array-type v) 2
		     (list (list        0 (- #[v 2])   #[v 1])
			   (list    #[v 2]       0  (- #[v 0]))
			   (list (- #[v 1])  #[v 0]        0))))

(define (inv3x3 M)
  (assert (and (array? M) (= (columns M) (rows M) 3)))
  (let ((d (det3x3 M)))
    (if (not (= d 0))
	(let ((1/M (make-typed-array (array-type M) (if #f #f) 3 3))
	      (1/d (/ 1 d)))
	(set! #[1/M 0 0] (* 1/d (- (* #[M 1 1] #[M 2 2]) 
				   (* #[M 2 1] #[M 1 2]))))
	(set! #[1/M 0 1] (* 1/d (- (* #[M 2 0] #[M 1 2]) 
				   (* #[M 1 0] #[M 2 2]))))
	(set! #[1/M 0 2] (* 1/d (- (* #[M 1 0] #[M 2 1]) 
				   (* #[M 2 0] #[M 1 1]))))
	;;
	(set! #[1/M 1 0] (* 1/d (- (* #[M 2 1] #[M 0 2]) 
				   (* #[M 0 1] #[M 2 2]))))
	(set! #[1/M 1 1] (* 1/d (- (* #[M 0 0] #[M 2 2]) 
				   (* #[M 2 0] #[M 0 2]))))
	(set! #[1/M 1 2] (* 1/d (- (* #[M 2 0] #[M 0 1]) 
				   (* #[M 0 0] #[M 2 1]))))
	;;
	(set! #[1/M 2 0] (* 1/d (- (* #[M 0 1] #[M 1 2]) 
				   (* #[M 1 1] #[M 0 2]))))
	(set! #[1/M 2 1] (* 1/d (- (* #[M 1 0] #[M 0 2]) 
				   (* #[M 0 0] #[M 1 2]))))
	(set! #[1/M 2 2] (* 1/d (- (* #[M 0 0] #[M 1 1]) 
				   (* #[M 1 0] #[M 0 1]))))
	;;
	(transpose 1/M)))))

(define-syntax-rule (save-operation op #;as name #;for <type>)
  (begin
    (define name op)
    (define-generic op)
    (define-method (op (n <type>) (m <type>))
      (name n m))
    (define-method (op (n <type>) (m <type>) . rest)
      (apply op (op n m) rest))))

(save-operation + #;as add #;for <number>)

(save-operation * #;as multiply #;for <number>)

(save-operation / #;as divide #;for <number>)

(save-operation - #;as subtract #;for <number>)

(define-method (* (s <number>) (p <point>)) 
  (array-map (lambda (x) (* x s)) p))

(define-method (* (p <point>) (s <number>))
  (array-map (lambda (x) (* x s)) p))

(define-method (* (m <array>) (v <generalized-vector>))
  (matrix-vector-mul m v))

(define-method (* (u <generalized-vector>) (v <generalized-vector>))
  (dot u v))

(define-method (* (p <point>))
  p)

(define-method (* (m1 <array>) (m2 <array>) . rest)
  (apply matrix-mul m1 (cons m2 rest)))

(define-method (+ (p <point>) . rest)
  (apply array-map add p rest))

(define-method (- (p <point>) . rest)
  (apply array-map subtract p rest))

(define-method (/ (p <point>) (n <number>))
  (* p (/ 1.0 n)))

; this has to be added to support unary minus, as guile probably 
; implements it like as (define (- (first <number>)) (- 0 first)) ...
(define-method (- (n <number>) (p <point>))
  (array-map (lambda(x)(- n x)) p)) 

(define-method (* (p <quaternion>) . rest)
  (fold (lambda(q p)(quaternion-multiply2 p q)) p rest))

(define-method (+ (p <quaternion>) . rest)
  (fold quaternion-add2 p rest))

(define-generic ~)

(define-method (~ (c <complex>))
  (make-rectangular (real-part c) (- (imag-part c))))

(define-method (~ (q <quaternion>))
  (cons (car q) (- (cdr q))))

(define-generic reciprocal)

(define-method (reciprocal (v <point>))
  (- v))

(define-method (reciprocal (q <quaternion>))
  (~ q))

(define-method (reciprocal (M <array>))
  (inv3x3 M))

(define* (projection #:key of onto)
  (let ((u of)
	(v onto))
    (let ((v^2 (* v v)))
      (if (< v^2 #[TOLERANCE])
	  #f32(0 0 0)
	  (/ (* (* u v) v) v^2)))))

(define (random-vector size)
  (let loop ((v (random-array size)))
    (if (< (* v v) #[TOLERANCE])
	(random-array size)
	v)))

(define (rotation-quaternion u w); This method is borrowed from Game
  (let ((u (normalized u)); Programming Gems vol.1 chapter 2.10
	(w (normalized w))); written by Stan Melax
    (let ((v (wedge3x3 u w)) 
	  (f (sqrt (* 2.0 (+ (* u w) 1.0)))))
      (cond ((> f #[TOLERANCE])
	     (quaternion (* 0.5 f) (/ v f)))
	    ((> (* u v) 0)
	     (quaternion 1.0 #f32(0 0 0)))
	    (else
	     (let ((v (random-vector 3)))
	       (quaternion 
		0.0
		(normalized 
		 (- v (projection #:of v #:onto u))))))))))
