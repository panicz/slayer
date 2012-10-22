(define-module (goose matrix)
  :use-module (ice-9 optargs)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-17)
  :use-module (extra common)
  :use-module (extra ref)
  :use-module ((rnrs) :version (6))  
  :export (eye transpose 
	   rows columns row column
	   matrix->vector vector->matrix
	   dot norm square project
	   normalize! normalized
	   det3x3 inv3x3 wedge3x3 crossm3x3
	   matrix-mul matrix-vector-mul
	   sgn
	   pi/4 pi/2 pi e
	   deg->rad rad->deg))

(define pi/4 (atan 1))
(define pi/2 (acos 0))
(define pi (* 2 pi/2))
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
  (make-shared-array a (lambda (x) (list i x)) (list 0 (1- (columns a)))))

(define (column a i) 
  (make-shared-array a (lambda (x) (list x i)) (list 0 (1- (rows a)))))

(define (dot a b)
  (apply + (array->list (vector-map * a b))))

(define (square v)
  (dot v v))

(define (norm v)
  (sqrt (square v)))

(define (normalize! v)
  (let ((lv (norm v)))
    (array-map! v (lambda(x)(/ x lv)) v)))

(define (normalized v)
  (let ((lv (norm v))
	(u (apply make-typed-array (array-type v) *unspecified* (array-dimensions v))))
    (array-map! u (lambda(x)(/ x lv)) v)
    u))

(define* (project source #:optional destination #:key (onto destination)) ; project src onto dest
  (let ((s (/ (dot source onto) (square onto))))
    (array-map (lambda(x)(* x s)) onto)))

(define (matrix-mul2 a b)
  (assert (= (columns a) (rows b)))
  (let ((result (make-typed-array (array-type a) *unspecified* (rows a) (columns b))))
    (array-index-map! result (lambda(i j)(dot (column b j) (row a i))))
    result))

(define (vector->matrix v)
  (let ((vl (vector-length v)))
    (make-shared-array v (lambda(i j)(list i)) vl 1)))

(define (matrix->vector V)
  (cond ((= (columns V) 1) (column V 0))
	((= (rows V) 1) (row V 0))
	(#t (error "unable to convert matrix to vector" V))))

(define (matrix-vector-mul M v)
  (matrix-mul2 M (vector->matrix v)))

(define (matrix-mul . args) (reduce matrix-mul2 (if #f #f) args))

(define (det3x3 M)
  (assert (and (array? M) (= (columns M) (rows M) 3)))
  (+ (* #[M 1 1] (- (* #[M 0 0] #[M 2 2]) (* #[M 2 0] #[M 0 2])))
     (* #[M 2 1] (- (* #[M 1 0] #[M 0 2]) (* #[M 0 0] #[M 1 2])))
     (* #[M 0 1] (- (* #[M 2 0] #[M 1 2]) (* #[M 1 0] #[M 2 2])))))

(define (wedge3x3 u v)
  (assert (and (vector? u) (vector? v) 
	       (= (vector-length u) (vector-length v) 3)))
  (list->typed-array (array-type u) 1 
		     (list (- (* #[u 1] #[v 2]) (* #[u 2] #[v 1]))
			   (- (* #[u 2] #[v 0]) (* #[u 0] #[v 2]))
			   (- (* #[u 0] #[v 1]) (* #[u 1] #[v 0])))))

(define (crossm3x3 v)
  (assert (and (array? M) (= (columns M) (rows M) 3)))
  (list->typed-array (array-type v) 2
		     (list (list        0 (- #[v 2])   #[v 1])
			   (list    #[v 2]       0  (- #[v 0]))
			   (list (- #[v 1])  #[v 0]        0))))

(define (inv3x3 M)
  (assert (and (array? M) (= (columns M) (rows M) 3)))
  (let ((d (det3x3 M)))
    (if (not (= d 0))
	(let ((1/M (make-typed-array (array-type M) *unspecified* 3 3))
	      (1/d (/ 1 d)))
	(set! #[1/M 0 0] (* 1/d (- (* #[M 1 1] #[M 2 2]) (* #[M 2 1] #[M 1 2]))))
	(set! #[1/M 0 1] (* 1/d (- (* #[M 2 0] #[M 1 2]) (* #[M 1 0] #[M 2 2]))))
	(set! #[1/M 0 2] (* 1/d (- (* #[M 1 0] #[M 2 1]) (* #[M 2 0] #[M 1 1]))))

	(set! #[1/M 1 0] (* 1/d (- (* #[M 2 1] #[M 0 2]) (* #[M 0 1] #[M 2 2]))))
	(set! #[1/M 1 1] (* 1/d (- (* #[M 0 0] #[M 2 2]) (* #[M 2 0] #[M 0 2]))))
	(set! #[1/M 1 2] (* 1/d (- (* #[M 2 0] #[M 0 1]) (* #[M 0 0] #[M 2 1]))))

	(set! #[1/M 2 0] (* 1/d (- (* #[M 0 1] #[M 1 2]) (* #[M 1 1] #[M 0 2]))))
	(set! #[1/M 2 1] (* 1/d (- (* #[M 1 0] #[M 0 2]) (* #[M 0 0] #[M 1 2]))))
	(set! #[1/M 2 2] (* 1/d (- (* #[M 0 0] #[M 1 1]) (* #[M 1 0] #[M 0 1]))))
	
	(transpose 1/M)))))

