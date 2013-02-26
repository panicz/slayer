(define-module (extra subspace)
  #:use-module ((oop goops) #:hide (slot-ref slot-set!))
  :use-module (ice-9 optargs)
  :use-module (extra ref)
  :use-module (extra common)
  :use-module (extra hset)
  :use-module (extra oop)
  :use-module (extra network)
  :use-module (extra shape)
  :use-module (extra 3d)
  :use-module (extra math)
  :duplicates (merge-generics);; replace warn-override-core warn last)
  :use-module ((rnrs) :version (6) :hide (map for-each))
  :export (<subspace>
	   <portal>
	   <passage>
	   <proxy> <3d-proxy>

	   *modifications*

	   objects-visible-to
	   subspaces-visible-from
	   subspaces-visible-to
	   objects-visible-from

	   ;network-slot-value

	   colliding-objects
	   update!
	   add!
	   other-side)
  :re-export (distance remove!)
  )

(define-syntax define-symmetric-method
  (syntax-rules ()
    ((_ (name arg1 arg2) body ...)
     (begin
       (define-method (name arg1 arg2) body ...)
       (define-method (name arg2 arg1) body ...)))))

(define-class <subspace> (<registered-object>)
  (objects #:init-value '()))

(define-class <modification> (<write-and-display-signature>)
  (subject #:init-keyword #:of)
  (source #:init-value #f #:init-keyword #:from)
  (destination #:init-value #f #:init-keyword #:to))

(define-method (signature (modification <modification>))
  (list (class-name (class-of modification))
	#:of #[modification 'subject]
	#:from #[modification 'source]
	#:to #[modification 'destination]))

(define *modifications* '())

(define-generic add!)

(define-method (add! (object <network-object>) #;to (space <subspace>))
  (<< "ADDING "object" TO "space)
  (push! *modifications* 
	 (make <modification> #:of object #:to space))
  (if (not (in? object #[space 'objects]))
      (push! #[space 'objects] object))
  (set! #[object 'context] space))

(define-generic remove!)

(define-method (remove! (object <network-object>))
  (push! *modifications* 
	 (make <modification> #:of object #:from #[object 'context]))
  (and-let* ((context #[object 'context])
	     ((is-a? context <subspace>)))
    ((@ (srfi srfi-1) delete!) object #[context 'objects]))
  (next-method))

(define-generic move!)

(define-method (move! (object <network-object>) #;to (destination <subspace>))
  (push! *modifications*
	 (make <modification> #:of object #:from #[object 'context]
	       #:to destination))
  (and-let* ((context #[object 'context])
	     ((is-a? context <subspace>)))
    ((@ (srfi srfi-1) delete!) object #[context 'objects]))
  (add! object destination))

(define-class <passage> (<subspace>)
  (plane #:init-value (make <plane> #:normal #f32(1 0 0) #:displacement 0.0)
	 #:allocation #:class)
  (left-portal #:init-value #f #;type #;<portal>)
  (right-portal #:init-value #f #;type #;<portal>))

(define-method (signature (passage <passage>))
  (append (next-method) 
	  (map (lambda(slot)(list slot #[passage slot]))
 	       '(left-portal right-portal))))

(define-class <portal> (<network-object> <3d-shape>)
  (shape #:init-keyword #:shape #:init-value 
	 (make <plane> #:normal #f32(1 0 0) #:displacement 0.0))
  (passage #:init-value #f #:init-keyword #:to #;type #;<passage>))

(define-method (other-side (portal <portal>))
  (let ((passage #[portal 'passage]))
    (cond ((equal? portal #[passage 'left-portal])
	   #[passage 'right-portal])
	  ((equal? portal #[passage 'right-portal])
	   #[passage 'left-portal])
	  (else
	   (throw 'invalid-portal-structure)))))

(define-class <proxy> ()
  (original #:init-keyword #:original))

(define-method (slot-ref (proxy <proxy>) slot-name)
  (if (in? slot-name (class-slot-names (class-of proxy)))
      (next-method)
      (slot-ref #[proxy 'original] slot-name)))

(define-method (slot-set! (proxy <proxy>) slot-name value)
  (if (in? slot-name (class-slot-names (class-of proxy)))
      (next-method)
      (slot-set! #[proxy 'original] slot-name value)))

(define-class <3d-proxy> (<proxy>)
  (context #:init-value #f)
  (translation #:init-value #f32(0 0 0)) ; vector
  (rotation #:init-value '(1.0 . #f32(0 0 0)))
  (position #:allocation #:virtual
	    #:slot-ref (lambda(my)
			 (+ #[my : 'original : 'position] #[my 'translation]))
	    #:slot-set! (lambda(my vector)
			  (set! #[my : 'original : 'position]
				(rotated vector (~ #[my 'rotation])))))
  ;; the keywords the class is meant to be
  ;; initialized with:
  ;;   #:of <3d-proxy> (the original object that we refer to)
  ;;   #:through <portal>
  ;;   #:into <subspace> (the destination subspace)
  )

(define-symmetric-method (distance (a <3d-proxy>) (b <3d-shape>))
  (distance (translated (rotated #[a 'shape] #[a 'orientation]) #[a 'position])
	    (translated (rotated #[b 'shape] #[b 'orientation]) #[b 'position])))

(define-symmetric-method (distance (a <3d>) (b <3d-proxy>))
  (distance #[a 'position] (translated (rotated #[b 'shape] #[b 'orientation])
				       #[b 'position])))

(define-method (initialize (this <3d-proxy>) args)
  (next-method)
  (let-keywords args
      #t
      ((of #;<3d-shape> #f) 
       (into #;<subspace> #f) 
       (through #;<portal> #f))
    (if (not (and (is-a? of <3d-shape>) 
		  (is-a? into <subspace>) 
		  (is-a? through <portal>)))
	(throw 'invalid-argument #:of of #:into into #:through through))
    (set! #[this 'original] of)
    (let ((plane #[through 'shape]))
      (if (not (is-a? plane <plane>))
	  (throw 'invalid-portal through "#[portal 'shape] is not a <plane>"))
      (set! #[this 'rotation] 
	    (rotation-quaternion #[plane 'normal]
				 #[into : 'plane : 'normal]))
      (set! #[this 'translation]
	    (- (* #[plane 'normal] #[plane 'displacement]))))))

; (make <3d-proxy> #:of object #:into subspace #:through portal)

(define-generic colliding-objects)


(define-method (colliding-objects (subspace <subspace>))
  (<< "OBJECTS: "#[subspace 'objects])
  (filter (match-lambda ((x y)
			 (<= (distance x y) 0.0)))
	  (all-pairs #[subspace 'objects])))

(define-method (colliding-objects (passage <passage>))
  (let ((neighbours 
	 (append (map (\ make <3d-proxy> #:of _ #:into passage 
		       #:through #[passage 'left-portal])
		      #[passage : 'left-portal : 'context : 'objects])
		 (map (\ make <3d-proxy> #:of _ #:into passage 
		       #:through #[passage 'right-portal])
		      #[passage : 'right-portal : 'context : 'objects]))))
    (append (next-method)
	    (filter (match-lambda ((x y)
				   (<= (distance x y) 0.0)))
		    (cart (filter (\ not (is-a? _ <portal>))
				  #[passage 'objects]) 
			  neighbours)))))


(define-generic handle-collision!!)

(define-method (handle-collision!! a b)
  (<< "undefined collision handler "
      (class-name (class-of a))" "
      (class-name (class-of b))))

(define-symmetric-method (handle-collision!! (object <top>) (proxy <3d-proxy>))
  (handle-collision!! object #[proxy 'original]))

(define-method (handle-collision!! (a <portal>) (b <portal>))
  (noop))

(define-symmetric-method (handle-collision!! (object <network-object>)
					     (portal <portal>))
  (move! object #[portal 'passage]))

(define-generic update!)

(define-method (update! (object <network-object>))
  (<< "updating " object (hash-map->alist #[object '%%write-registry]))
  (reset-write-registry! object))

(define-method (update! (subspace <subspace>))
  (for-each update! #[subspace 'objects])
  (<<"COLLIDING OBJECTS: " (colliding-objects #;in subspace))
  (map (\ apply handle-collision!! _)  (colliding-objects #;in subspace))
  )

(define* (subspaces-visible-from space #:key (except #;portals '())(range +inf.0))
  (cond ((<= range 0)
	 '())
	((is-a? space <passage>)
	 (let ((left-portal #[space 'left-portal])
	       (right-portal #[space 'right-portal]))
	   (let ((exceptions (union except (list left-portal right-portal))))
	     (cons space 
		   (append 
		    (if (in? left-portal except)
			'()
			(subspaces-visible-from 
			 #[left-portal 'context] #:range (1- range)
			 #:except exceptions))
		    (if (in? right-portal except)
			'()
			(subspaces-visible-from
			 #[right-portal 'context] #:range (1- range)
			 #:except exceptions)))))))
	((is-a? space <subspace>)
	 (let* ((portals (difference
			  (filter (\ is-a? _ <portal>) #[space 'objects])
			  except))
		(exceptions (append except portals #;(map other-side portals))))
	   (cons space
		 (append-map
		  (lambda (portal)
		    (subspaces-visible-from #[portal 'passage] #:range (1- range)
					    #:except exceptions))
		  portals))))))

(define* (subspaces-visible-to <network-object> #:key (range +inf.0))
  (let ((context #[<network-object> 'context]))
    (if (is-a? context <subspace>)
	(subspaces-visible-from context #:range range)
	(throw 'invalid-context context 'of <network-object>))))

(define* (objects-visible-from space #:key (range +inf.0))
  (let ((objects (append-map 
		  #[_ 'objects] 
		  (subspaces-visible-from space #:range range))))
    (<< objects)
    objects))  

(define* (objects-visible-to <network-object> #:key (range +inf.0))
  (let ((context #[<network-object> 'context]))
    (if (is-a? context <subspace>)
	(objects-visible-from context #:range range)
	(list <network-object>))))

