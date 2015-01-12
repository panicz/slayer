(define-module (extra trimesh)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra ref)
  #:export (
	    3d->trimesh
	    ))

(define-syntax-rule (temporarily assertions ...)
  ;; the purpose of "temporarily" macro is to mark statements that were
  ;; asserted while the code was being written, but which should be rejected
  ;; in the final code in favor of more general code
  (if #f #f))

(define (triangles faces)
  (match faces

    (('triangles indices)
     (map-n 3 list (flatten (array->list indices))))

    (('triangle-strip indices)
     (let ((indices (flatten (array->list indices))))
       (let (((a ... _ _) indices)
	     ((_ b ... _) indices)
	     ((_ _ c ...) indices))
	 (map list a b c))))

    (('triangle-fan indices)
     (let (((middle contour ...) (flatten (array->list indices))))
       (let (((a ... _) contour)
	     ((_ b ...) contour))
	 (map (lambda (a b) `(,middle ,a ,b)) a b))))

    (('quads indices)
     (append-map (lambda ((a b c d))
		   `((,a ,b ,c) (,c ,d ,a)))
		 (map-n 4 list (flatten (array->list indices)))))

    ((type . _)
     (format #t "primitive ~s not supported\n" type)
     '())

    (else
     (error "malformed faces specification"))))

(define (3d->trimesh mesh)
  (let ((('mesh . details) mesh))
    (temporarily
     (assert (and (= (count (matches? ('vertices . _)) details)
		     (count (matches? ('faces . _)) details)
		     1)
		  (= (count (matches? ('with-transforms . _)) details)
		     0))))
    (let (((vertices) (assoc-ref mesh 'vertices))
	  (faces (assoc-ref mesh 'faces)))
      (unless (and (matches? (_ 3) (array-dimensions vertices))
		   (eq? (array-type vertices) 'f32))
	(error "the vertex array should have 3 columns of f32 elements"))
      `(,vertices
	. ,(list->typed-array 'u32 2 (append-map triangles faces))))))
