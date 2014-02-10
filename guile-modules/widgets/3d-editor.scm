(define-module (widgets 3d-editor)
  #:use-module (oop goops)
  #:use-module (widgets base)
  #:use-module (widgets 3d-view)
  #:use-module (extra common)
  #:use-module (extra math)
  #:use-module (extra shape)
  #:use-module (extra ref)
  #:use-module (extra 3d)
  #:use-module (slayer)
  #:use-module (slayer 3d)
  #:export (<3d-editor> 
	    add-object! 
	    select-object!
	    unselect-object!
	    unselect-all!
	    draw-mesh! 
	    draw-objects
	    object-at-position
	    )
  #:re-export (
	       relative-turn!
	       relative-twist!
	       relative-move!
	       screen->3d
	       3d->screen
	       X-SENSITIVITY
	       Y-SENSITIVITY)
  )

(define-class <3d-editor> (<3d-view>)
  (current-display-index #:init-value 0)
  (next-display-index!
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (let ((display-index #[self 'current-display-index]))
       (set! #[self 'current-display-index] 
	     (modulo (+ display-index 1) (max-display-index)))
       display-index))
   #:slot-set! noop)
  (draw-objects!
   #:allocation #:virtual
   #:slot-ref (lambda (view)
		(for (object => index) in #[view 'objects]
		     (set-display-index! index)
		     (draw-model! object)
		     (when (in? object #[view 'selected])
		       (set-display-index! #f)
		       (draw-contour! object))))
   #:slot-set! noop)
  (lit-objects!
   #:allocation #:virtual
   #:slot-ref (lambda (view)
		(for (object => index) in #[view 'objects]
		     (setup-lights! #[object '%lights])))
   #:slot-set! noop)
  (objects #:init-thunk make-hash-table)
  (object-groups #:init-form (make-vector (max-display-index) '()))
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

(define-method (add-object! (view <3d-editor>) (object <3d>))
  (let ((display-index #[view 'next-display-index!]))
    (set! #[view : 'objects : object] display-index)
    (push! #[view : 'object-groups : display-index] object)))

(define-method (select-object! (view <3d-editor>) (object <3d>))
  (if (not (in? object #[view 'selected]))
      (push! #[view 'selected] object)))

(define-method (unselect-object! (view <3d-editor>) (object <3d>))
  (set! #[view 'selected] (delete object #[view 'selected])))

(define-method (unselect-all! (view <3d-editor>))
  (set! #[view 'selected] '()))
