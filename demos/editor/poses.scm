(define-module (editor poses)
  #:use-module (extra common)
  #:use-module (editor relations)
  #:use-module (scum physics)
  #:export (null-pose combine-poses))

(define (null-pose #;for rig)
  (let ((joints (rig-joints rig)))
    `(pose (,(joint-name joints) . 0.0) ...)))

(define (combine-poses a b)
  (match-let ((('pose . pose-a) a)
	      (('pose . pose-b) b))
    `(pose ,@(replace-alist-bindings pose-a pose-b))))
