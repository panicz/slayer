(define-module (extra shape)
  :use-module (oop goops)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-2)
  :use-module (srfi srfi-11)
  :use-module (ice-9 optargs)
  :use-module (ice-9 match)
  :use-module (extra ref)
  :use-module (extra common)
  :use-module (extra math)
  :use-module (extra oop)
  :use-module ((rnrs) :version (6))
  :export 
  (<basic-shape>
   <point>
   <plane>
   <line>
   <sphere>
   <triangle>
   <segment>
   <capsule>
   <complex-shape>
   distance
   nearest-points))

(set! %load-path (append (list "." "..")  %load-path))

(use-modules 
 (oop goops)
   (srfi srfi-1)
   (srfi srfi-2)
   (srfi srfi-11)
   (ice-9 optargs)
   (ice-9 match)
   (extra ref)
   (extra common)
   (extra math)
   (extra oop)
   ((rnrs) :version (6)))


(define-syntax define-symmetric-method
  (syntax-rules ()
    ((_ (name arg1 arg2) body ...)
     (begin
       (define-method (name arg1 arg2) body ...)
       (define-method (name arg2 arg1) body ...)))))

(define tolerance (make-fluid 0.0001))

(define-class <basic-shape> ())

(define-class <plane> (<basic-shape>)
  (normal #:init-keyword #:normal) ; vector
  (displacement #:init-keyword #:displacement)) ; scalar

(define-class <sphere> (<basic-shape>)
  (position #:init-value #(0 0 0))
  (radius #:init-value 1))

(define-class <line> (<basic-shape>)
  direction ; vector
  displacement) ; vector

(define-class <triangle> (<basic-shape>)
  (vertices #:init-value #(#(1 0 0) #(0 1 0) #(0 0 1))))

(define-class <segment> (<basic-shape>)
  (a #:init-value #f32(1 -1 0))
  (b #:init-value #f32(-1 1 0)))

(define-class <capsule> (<segment>)
  (radius #:init-value 1.0))

(define-generic line)

(define-method (line (p1 <point>) (p2 <point>))
  (let* ((direction (normalized (- p2 p1))) ; from p1 to p2
	 (displacement (- p1 (* (* p1 direction) direction))))
    (make* <line> 
	   #:direction direction
	   #:displacement displacement)))

(define-method (line (p1 <plane>) (p2 <plane>))
  (let* ((direction 
	  (normalized (wedge3x3 #[p1 'normal] #[p2 'normal])))
	   (angle (* #[p1 'normal] #[p2 'normal]))
	   (normalizer (/ 1 (- 1 (expt angle 2))))
	   (displacement 
	    (+ (* normalizer 
		  (- #[p1 'displacement] 
		     (* #[p2 'displacement] angle))
		  #[p1 'normal])
	       (* normalizer
		  (- #[p2 'displacement] 
		     (* #[p1 'displacement] angle))
		  #[p2 'normal]))))
      (make* <line>
	     #:direction direction
	     #:displacement displacement)))

(define-generic plane)

(define-method (plane (p1 <point>) (p2 <point>) (p3 <point>))
  (let* ((normal (wedge3x3 (- p1 p2) (- p1 p3)))
	 (distance (* p1 normal))
	 (1/norm (/ 1 (norm normal))))
    (make* <plane>  #:normal (* normal 1/norm) 
	   #:displacement (* distance 1/norm))))

(define-method (plane (t <triangle>))
    (let ((v (t 'vertices)))
      (plane #[v 0] #[v 1] #[v 2])))

(define (project-onto-line l x)
  (+ (* x #[l 'direction]) #[l 'displacement]))

(define (unproject-from-line l v)
  (* #[l 'direction] (- v #[l 'displacement])))

(define-method (nearest-points (l1 <line>) (l2 <line>))
  (let* ((ldot (* #[l1 'direction] #[l2 'direction]))
	 (x (- 1 ldot)))
    (if (< x #[tolerance]);lines are parallel, so any points will do
	(cons x (unproject-from-line l2 (project-onto-line l1 x)))
	(let* ((v (- #[l1 'displacement] #[l2 'displacement]))
	       (lv (* #[l1 'direction] v))
	       (y (/ (- (* #[l2 'displacement] v) (* ldot lv)) x)))
	  (cons (- (* ldot y) lv) y)))))

(define-generic intersection)

(define-method (intersection (P <plane>) (Q <plane>) (R <plane>))
  (let ((type (array-type #[P 'normal]))
	(d (det3x3 (vectors->matrix
		    #[P 'normal]
		    #[Q 'normal] 
		    #[R 'normal]))))
    (if (> (abs d) #[tolerance])
	(let ((all (list P Q R)))
	  (let ((N (map #[_ 'normal] all))
		(V (list->uniform-vector 
		    type
		    (map #[_ 'displacement] all))))
	    (let ((X (list->uniform-vector type (map #[_ 0] N)))
		  (Y (list->uniform-vector type (map #[_ 1] N)))
		  (Z (list->uniform-vector type (map #[_ 2] N))))
	      (let ((dx (det3x3 (vectors->matrix V Y Z)))
		    (dy (det3x3 (vectors->matrix X V Z)))
		    (dz (det3x3 (vectors->matrix X Y V))))
		(* (/ 1.0 d)
		   (list->uniform-vector 
		    type
		    (list dx dy dz))))))))))

(define-generic distance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      1        2        3        4        5        6        7
;1<pt>|<pt><pt>|<pt><pl>|<pt><ln>|<pt><sp>|<pt><tr>|<pt><sg>|<pt><cp>|
;2<pl>         |<pl><pl>|<pl><ln>|<pl><sp>|<pl><tr>|<pl><sg>|<pl><cp>|
;3<ln>                  |<ln><ln>|<ln><sp>|<ln><tr>|<ln><sg>|<ln><cp>|
;4<sp>                           |<sp><sp>|<sp><tr>|<sp><sg>|<sp><cp>|
;5<tr>                                    |<tr><tr>|<tr><sg>|<tr><cp>|
;6<sg>                                             |<sg><sg>|<sg><cp>|
;7<cp> 28!                                                  |<cp><cp>|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For 7 primitives there are 28 symmetric methods (49 actual methods)
;; to check the distance between them: 
;;  +1. <pt><pt>     +8. <pl><sp>     15. <tr><tr>     22. <pt><cp>
;;  +2. <pt><pl>      9. <ln><sp>     16. <pt><sg>     23. <pl><cp>
;;   3. <pl><pl>    +10. <sp><sp>    +17. <pl><sg>     24. <ln><cp>
;;  +4. <pt><ln>     11. <pt><tr>     18. <ln><sg>     25. <sp><cp>
;;   5. <pl><ln>     12. <pl><tr>     19. <sp><sg>     26. <tr><cp>
;;   6. <ln><ln>     13. <ln><tr>     20. <tr><sg>     27. <sg><cp>
;;  +7. <pt><sp>     14. <sp><tr>    +21. <sg><sg>    +28. <cp><cp>

;;  1. <pt><pt>               
(define-method (distance (p1 <point>) (p2 <point>))
  (norm (- p1 p2)))

;;  2. <pt><pl>
(define-symmetric-method (distance (v <point>) (p <plane>))
  (- (* #[p 'normal] v) #[p 'displacement]))

;;  3. <pl><pl>
(define-method (distance (p1 <plane>) (p2 <plane>))
  (throw 'not-implemented))

;;  4. <pt><ln> 
(define-symmetric-method (distance (p <point>) (l <line>))
  (distance p (+ #[l 'displacement] 
		 (* (* p #[l 'direction] #[l 'direction])))))

;;  5. <pl><ln>
(define-symmetric-method (distance (p <plane>) (l <line>))
  (throw 'not-implemented))

;;  6. <ln><ln>
(define-method (distance (l1 <line>) (l2 <line>))
  (throw 'not-implemented))

;;  7. <pt><sp>
(define-symmetric-method (distance (p <point>) (s <sphere>))
  (- (distance p #[s 'position]) #[s 'radius]))

;;  8. <pl><sp> 
(define-symmetric-method (distance (p <plane>) (s <sphere>))
  (- (distance p #[s 'position]) #[s 'radius]))

;;  9. <ln><sp>
(define-symmetric-method (distance (l <line>) (s <sphere>))
  (throw 'not-implemented))

;; 10. <sp><sp>
(define-method (distance (s1 <sphere>) (s2 <sphere>))
    (- (distance #[s1 'position] #[s2 'position]) 
       #[s1 'radius] #[s2 'radius]))

;; 11. <pt><tr>
(define-symmetric-method (distance (p <point>) (t <triangle>))
  (throw 'not-implemented))

;; 12. <pl><tr>
(define-symmetric-method (distance (P <plane>) (t <triangle>))
  (throw 'not-implemented))

;; 13. <ln><tr>
(define-symmetric-method (distance (l <line>) (t <triangle>))
  (throw 'not-implemented))

;; 14. <sp><tr>
(define-symmetric-method (distance (s <sphere>) (t <triangle>))
  (throw 'not-implemented))

;; 15. <tr><tr>
(define-method (distance (t1 <triangle>) (t2 <triangle>))
  (throw 'not-implemented))

;; 16. <pt><sg>
(define-symmetric-method (distance (p <sphere>) (t <triangle>))
  (throw 'not-implemented))

;; 17. <pl><sg>
(define-symmetric-method (distance (p <plane>) (s <segment>))
  (min (distance p #[s 'a]) (distance p #[s 'b])))

;; 18. <ln><sg>
(define-symmetric-method (distance (l <line>) (s <segment>))
  (throw 'not-implemented))

;; 19. <sp><sg>
(define-symmetric-method (distance (s <sphere>) (g <segment>))
  (throw 'not-implemented))

;; 20. <tr><sg>
(define-symmetric-method (distance (t <triangle>) (s <segment>))
  (throw 'not-implemented))

;; 21. <sg><sg>
(define-method (distance (s1 <segment>) (s2 <segment>))
  (let ((l1 (line #[s1 'a] #[s1 'b]))
	(l2 (line #[s2 'a] #[s2 'b])))
    (match-let (((l1p . l2p) (nearest-points l1 l2))
		((l1a l1b l2a l2b) 
		 (map unproject-from-line
		      (list  l1     l1     l2     l2    )
		      (list(s1 'a)(s1 'b)(s2 'a)(s2 'b)))))
	(apply min (map (lambda(pair)
			  (distance (car pair) (cadr pair)))
			(cart (append (list (s1 'a) (s1 'b))
				      (if (< l1a l1p l1b) 
					  (list (project-onto-line 
						 l1 l1p))
					  '()))
			      (append (list (s2 'a) (s2 'b))
				      (if (< l2a l2p l2b)
					  (list (project-onto-line
						 l2 l2p))
					  '()))))))))

;; 22. <pt><cp>
(define-symmetric-method (distance (p <point>) (c <capsule>))
  (throw 'not-implemented))

;; 23. <pl><cp>
(define-symmetric-method (distance (p <plane>) (c <capsule>))
  (throw 'not-implemented))

;; 24. <ln><cp>
(define-symmetric-method (distance (l <line>) (c <capsule>))
  (throw 'not-implemented))

;; 25. <sp><cp>
(define-symmetric-method (distance (s <sphere>) (c <capsule>))
  (throw 'not-implemented))

;; 26. <tr><cp>
(define-symmetric-method (distance (t <triangle>) (c <capsule>))
  (throw 'not-implemented))

;; 27. <sg><cp>
(define-symmetric-method (distance (s <segment>) (c <capsule>))
  (throw 'not-implemented))

;; 28. <cp><cp>
(define-method (distance (c1 <capsule>) (c2 <capsule>))
  (- (next-method) #[c1 'radius] #[c2 'radius]))
