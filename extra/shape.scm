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
   <plane>
   <line>
   <sphere>
   <segment>
   <capsule>
   ;<complex-shape>
   distance
   TOLERANCE
   nearest-points))

#;(set! %load-path (append (list "." "..")  %load-path))

#;(use-modules 
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

(define TOLERANCE (make-fluid 0.0001))

(define-class <basic-shape> ())

(define-class <plane> (<basic-shape>)
  (normal #:init-keyword #:normal) ; vector
  (displacement #:init-keyword #:displacement)) ; scalar

(define-class <sphere> (<basic-shape>)
  (position #:init-value #f32(0 0 0) #:init-keyword #:position)
  (radius #:init-value 1 #:init-keyword #:radius))

(define-class <line> (<basic-shape>)
  (direction #:init-keyword #:direction); vector
  (displacement #:init-keyword #:displacement)) ; vector

(define-class <segment> (<basic-shape>)
  (a #:init-value #f32(1 -1 0) #:init-keyword #:a)
  (b #:init-value #f32(-1 1 0) #:init-keyword #:b))

(define-class <capsule> (<segment>)
  (radius #:init-value 1.0 #:init-keyword #:radius))

(define-generic line)

(define-method (line (p1 <point>) (p2 <point>))
  (let* ((direction (normalized (- p2 p1))) ; from p1 to p2
	 (displacement (- p1 (* (* p1 direction) direction))))
    (make <line> 
      #:direction direction
      #:displacement displacement)))

(define-method (line (s <segment>))
  (line #[s 'a] #[s 'b]))

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
    (make <line>
      #:direction direction
      #:displacement displacement)))

(define-generic plane)

(define-method (plane (p1 <point>) (p2 <point>) (p3 <point>))
  (let* ((normal (wedge3x3 (- p1 p2) (- p1 p3)))
	 (distance (* p1 normal))
	 (1/norm (/ 1 (norm normal))))
    (make <plane>  
      #:normal (* normal 1/norm) 
      #:displacement (* distance 1/norm))))

(define-method (project-onto (l <line>) (x <real>))
  (+ (* x #[l 'direction]) #[l 'displacement]))

(define-method (unproject-from (l <line>) (v <point>))
  (* #[l 'direction] (- v #[l 'displacement])))

(define-method (nearest-points (l1 <line>) (l2 <line>))
  (let* ((ldot (* #[l1 'direction] #[l2 'direction]))
	 (x (- 1 ldot)))
    (if (< x #[TOLERANCE]);lines are parallel, so any points will do
	(cons x (unproject-from l2 (project-onto l1 x)))
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
    (if (> (abs d) #[TOLERANCE])
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

;(distance: <shape> <shape> -> <real>)
(define-generic distance)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      1        2        3        4        5        6 
;1 <pt>|<pt><pt>|<pt><pl>|<pt><ln>|<pt><sp>|<pt><sg>|<pt><cp>|
;2 <pl>         |<pl><pl>|<pl><ln>|<pl><sp>|<pl><sg>|<pl><cp>|
;3 <ln>                  |<ln><ln>|<ln><sp>|<ln><sg>|<ln><cp>|
;4 <sp>                           |<sp><sp>|<sp><sg>|<sp><cp>|
;5 <sg>                                    |<sg><sg>|<sg><cp>|
;6 <cp>                                             |<cp><cp>|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For 6 primitives there are 21 symmetric methods (36 actual 
;; methods) to check the distance between them: 
;;  +1. <pt><pt>     +8. <pl><sp>    +15. <sg><sg>
;;  +2. <pt><pl>     +9. <ln><sp>    +16. <pt><cp>
;;  +3. <pl><pl>    +10. <sp><sp>    +17. <pl><cp>
;;  +4. <pt><ln>    +11. <pt><sg>    +18. <ln><cp>
;;  +5. <pl><ln>    +12. <pl><sg>    +19. <sp><cp>
;;  +6. <ln><ln>    +13. <ln><sg>    +20. <sg><cp>
;;  +7. <pt><sp>    +14. <sp><sg>    +21. <cp><cp>

;;  1. <pt><pt>       
(define-method (distance (p1 <point>) (p2 <point>))
  (norm (- p1 p2)))

;;  2. <pt><pl>
(define-symmetric-method (distance (v <point>) (p <plane>))
  (- (* #[p 'normal] v) #[p 'displacement]))

;;  3. <pl><pl>
(define-method (distance (p1 <plane>) (p2 <plane>))
  (if (< (norm (wedge3x3 #[p1 'normal] #[p2 'normal])) #[TOLERANCE])
      (abs (- #[p1 'displacement] #[p2 'displacement]))
      0.0))

;;  4. <pt><ln> 
(define-symmetric-method (distance (p <point>) (l <line>))
  (distance p (+ #[l 'displacement] 
		 (* (* p #[l 'direction] #[l 'direction])))))

;;  5. <pl><ln>
(define-symmetric-method (distance (p <plane>) (l <line>))
  (if (< (* #[p 'normal] #[l 'direction]) #[TOLERANCE])
      (norm (- #[l 'displacement]
	       (* #[p 'displacement] #[p 'normal])))
      0.0))

;;  6. <ln><ln>
(define-method (distance (l1 <line>) (l2 <line>)) 
  (match-let (((p1 . p2) (nearest-points l1 l2)))
    (distance (project-onto l1 p1) 
	      (project-onto l2 p2))))

;;  7. <pt><sp>
(define-symmetric-method (distance (p <point>) (s <sphere>))
  (- (distance p #[s 'position]) #[s 'radius]))

;;  8. <pl><sp> 
(define-symmetric-method (distance (p <plane>) (s <sphere>))
  (- (distance p #[s 'position]) #[s 'radius]))

;;  9. <ln><sp>
(define-symmetric-method (distance (l <line>) (s <sphere>))
  (- (distance #[s 'position] l) #[s 'radius]))

;; 10. <sp><sp>
(define-method (distance (s1 <sphere>) (s2 <sphere>))
  (- (distance #[s1 'position] #[s2 'position]) 
     #[s1 'radius] #[s2 'radius]))

;; 11. <pt><sg>
(define-symmetric-method (distance (p <point>) (s <segment>))
  (let ((a #[s 'a])
	(b #[s 'b]))
    (let ((l (line a b)))
      (let ((a* (unproject-from l a))
	    (b* (unproject-from l b))
	    (p* (unproject-from l p)))
	(if (<= a* p* b*)
	    (distance p (project-onto l p*))
	    (min (distance p a) (distance p b)))))))

;; 12. <pl><sg>
(define-symmetric-method (distance (p <plane>) (s <segment>))
  (let ((d (* #[plane 'displacement] #[plane 'normal]))
	(a #[s 'a])
	(b #[s 'b]))
    (if (< (* (- a d) (- b d)) 0)
	0.0
	(min (distance a p) (distance b p)))))

;; 13. <ln><sg>
(define-symmetric-method (distance (l <line>) (s <segment>))
  (let ((a #[s 'a])
	(b #[s 'b]))
    (let ((l* (line a b)))
      (let ((a* (unproject-from l* a))
	    (b* (unproject-from l* b)))
	(match-let (((p . p*)(nearest-points l l*)))
	  (if (< a* p* b*)
	      (distance (project-onto l* p*)
			(project-onto l p))
	      (min (distance a l)
		   (distance b l))))))))

;; 14. <sp><sg>
(define-symmetric-method (distance (s <sphere>) (g <segment>))
  (- (distance #[s 'position] g) #[s 'radius]))

;; 15. <sg><sg>
(define-method (distance (s1 <segment>) (s2 <segment>))
  (let ((l1 (line #[s1 'a] #[s1 'b]))
	(l2 (line #[s2 'a] #[s2 'b])))
    (match-let (((l1p . l2p) (nearest-points l1 l2))
		((l1a l1b l2a l2b) 
		 (map unproject-from
		      (list  l1     l1     l2     l2    )
		      (list(s1 'a)(s1 'b)(s2 'a)(s2 'b)))))
      (apply min (map (match-lambda((a b)
				    (distance a b)))
		      (cart (append (list (s1 'a) (s1 'b))
				    (if (< l1a l1p l1b) 
					(list (project-onto 
					       l1 l1p))
					'()))
			    (append (list (s2 'a) (s2 'b))
				    (if (< l2a l2p l2b)
					(list (project-onto
					       l2 l2p))
					'()))))))))

;; 16. <pt><cp>
(define-symmetric-method (distance (p <point>) (c <capsule>))
  (- (next-method) #[c 'radius]))

;; 17. <pl><cp>
(define-symmetric-method (distance (p <plane>) (c <capsule>))
  (- (next-method) #[c 'radius]))

;; 18. <ln><cp>
(define-symmetric-method (distance (l <line>) (c <capsule>))
  (- (next-method) #[c 'radius]))

;; 19. <sp><cp>
(define-symmetric-method (distance (s <sphere>) (c <capsule>))
  (- (distance #[s 'position] c) #[s 'radius] #[c 'radius]))

;; 20. <sg><cp>
(define-symmetric-method (distance (s <segment>) (c <capsule>))
  (- (next-method) #[c 'radius]))

;; 21. <cp><cp>
(define-method (distance (c1 <capsule>) (c2 <capsule>))
  (- (next-method) #[c1 'radius] #[c2 'radius]))
