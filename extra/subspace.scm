(define-module (extra subspace)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-2)
  :use-module (srfi srfi-11)
  :use-module (oop goops)
  :use-module (ice-9 match)
  :use-module (ice-9 optargs)
  :use-module (ice-9 regex)
  :use-module (extra ref)
  :use-module (extra common)
  :use-module (extra hset)
  :use-module (extra oop)
  :use-module (extra network)
  :use-module (extra 3d)
  :use-module (extra shape)
  :use-module ((rnrs) :version (6))
  :export (<subspace>
	   <portal>
	   <passage>
	   *modifications*

	   objects-visible-to
	   subspaces-visible-from
	   objects-visible-from
	   
	   update!
	   add!
	   other-side))

(define-class <subspace> (<registered-object>)
  (objects #:init-value '()))

(define-class <modification> ()
  (subject #:init-keyword #:of)
  (source #:init-value #f #:init-keyword #:from)
  (destination #:init-value #f #:init-keyword #:to))

(define *modifications* '())

(define-generic add!)

(define-method (add! (object <network-object>) (space <subspace>))
  (push! *modifications* 
	 (make <modification> #:of object #:to space))
  (push! #[space 'objects] object)
  (set! #[object 'context] space))

(define-generic remove!)

(define-method (remove! (object <network-object>))
  (push! *modifications* 
	 (make <modification> #:of object #:from #[object 'context]))
  (and-let* ((context #[object 'context])
	     ((is-a? context <subspace>)))
    (delete! object #[context 'objects]))
  (next-method))

(define-generic move!)

(define-method (move! (object <network-object>) (destination <subspace>))
  (push! *modifications*
	 (make <modification> #:of object #:from #[object 'context]
	       #:to destination))
  (and-let* ((context #[object 'context])
	     ((is-a? context <subspace>)))
    (delete! object #[context 'objects]))
  (add! object destination))

(define-generic update!)

(define-method (update! (object <network-object>))
  (noop))

(define-method (update! (subspace <subspace>))
  (for-each update! #[subspace 'objects]))

(define-class <passage> (<subspace>)
  (left-portal #:init-value #f)
  (right-portal #:init-value #f))

(define-method (signature (passage <passage>))
  (append (next-method) 
	  (map (lambda(slot)(list slot #[passage slot]))
	       (class-specific-slot-names (class-of passage)))))

(define-class <portal> (<network-object> <3d-shape>)
  (shape #:init-keyword #:shape #:init-value 
	 (make <plane> #:normal #f32(1 0 0) #:displacement 0.0))
  (passage #:init-value #f #:init-keyword #:to))

(define-method (other-side (portal <portal>))
  (let ((passage #[portal 'passage]))
    (cond ((equal? portal #[passage 'left-portal])
	   #[passage 'right-portal])
	  ((equal? portal #[passage 'right-portal])
	   #[passage 'left-portal])
	  (else
	   (throw 'invalid-portal-structure)))))

#|(define* (objects-visible-from space #:key (except #;portals '()) (range +inf.0))
  (cond ((<= range 0)
	 '())
	((is-a? space <passage>)
	 (let ((left-portal #[space 'left-portal])
	       (right-portal #[space 'right-portal]))
	   (let ((exceptions (union except (list left-portal right-portal))))
	     (append 
	      #[space 'objects]
	      (if (in? left-portal except)
		  '()
		  (objects-visible-from 
		   #[left-portal 'context] #:range (1- range)
		   #:except exceptions))
	      (if (in? right-portal except)
		  '()
		  (objects-visible-from 
		   #[right-portal 'context] #:range (1- range)
		   #:except exceptions))))))
	((is-a? space <subspace>)
	 (let* ((objects #[space 'objects])
		(portals (difference 
			  (filter (\ is-a? _ <portal>) objects)
			  except))
		(exceptions (append except portals)))
	   (append objects (append-map 
			    (lambda (portal)
			      (objects-visible-from #[portal 'passage]
						    #:range (1- range)
						    #:except exceptions))
			    portals))))
	(else
	 (throw 'invalid-argument))))|#

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

(define* (objects-visible-from space #:key (range +inf.0))
  (let ((objects (append-map 
		  #[_ 'objects] 
		  (subspaces-visible-from space #:range range))))
    (<< objects)
    objects))

(define-method (objects-visible-to (object <network-object>))
  (let ((context #[object 'context]))
    (if (is-a? context <subspace>)
	(objects-visible-from context)
	(list object))))


#|
(define *subspaces* 
  #f)

(let ((subspace-1 (make <subspace>))
      (subspace-2 (make <subspace>)))
  (let* ((passage (make <passage>))
	 (left-portal (make <portal> #:to passage))
	 (right-portal (make <portal> #:to passage)))
    (set! *subspaces* (list subspace-1 subspace-2 passage))
    (set! #[passage 'left-portal] left-portal)
    (add! left-portal subspace-1)
    (set! #[passage 'right-portal] right-portal)
    (add! right-portal subspace-2)))
|#