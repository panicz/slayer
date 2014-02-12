(define-module (widgets 3d-editor)
  #:use-module (oop goops)
  #:use-module (widgets base)
  #:use-module (widgets 3d)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra shape)
  #:use-module (extra ref)
  #:use-module (extra 3d)
  #:use-module (slayer)
  #:use-module (slayer 3d)
  #:export (<3d-editor> 
	    select-object!
	    unselect-object!
	    unselect-all!
	    draw-mesh! 
	    draw-objects
	    object-at-position
	    )
  #:re-export (
	       <3d-stage>
	       add-object! 
	       relative-turn!
	       relative-twist!
	       relative-move!
	       screen->3d
	       3d->screen
	       X-SENSITIVITY
	       Y-SENSITIVITY)
  )

(define-class <3d-editor> (<3d-view>)
  (object-groups #:init-form (make-vector (1+ (max-display-index)) '()))
  (draw-objects!
   #:allocation #:virtual
   #:slot-ref 
   (lambda (view)
     (array-map! #[view 'object-groups] (lambda _ '()))
     (let ((index 0))
       (for object in #[view : 'stage : 'objects]
	    (set-display-index! index)
	    (draw-model! object)
	    (when (in? object #[view 'selected])
	      (draw-contour! object))
	    (push! #[view : 'object-groups : index] object)
	    (set! index (modulo (+ index 1) (max-display-index))))))
   #:slot-set! noop)
  (objects #:init-thunk make-hash-table)
  ;; hash whose keys are objects and values -- display indices
  (selected #:init-value '()))

(define-method (object-at-position x y #;from (editor <3d-editor>))
  (and-let* ((n (display-index x y))
	     (candidates #[editor : 'object-groups : n]))
    (case (length candidates)
      ((0)
       #f)
      ((1)
       (first candidates))
      (else
       (format #t "ambiguous display-index candidates\n")))))

(define-method (select-object! (view <3d-editor>) (object <3d>))
  (if (not (in? object #[view 'selected]))
      (push! #[view 'selected] object)))

(define-method (unselect-object! (view <3d-editor>) (object <3d>))
  (set! #[view 'selected] (delete object #[view 'selected])))

(define-method (unselect-all! (view <3d-editor>))
  (set! #[view 'selected] '()))
