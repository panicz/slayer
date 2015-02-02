(define-module (editor poses)
  #:use-module (extra common)
  #:use-module (editor relations)
  #:use-module (scum physics)
  #:export (null-pose 
	    combine-poses
	    pose
	    clamped-pose))

(define (null-pose #;for rig)
  `(pose ,@(map (lambda (joint) 
		  `(,(joint-name joint) . 0.0)) 
		(rig-joints rig))))

(define (combine-poses a b)
  (match-let ((('pose . pose-a) a)
	      (('pose . pose-b) b))
    `(pose ,@(replace-alist-bindings pose-a pose-b))))

(define (pose #;of rig)
  `(pose ,@(map (lambda (joint)
		  `(,(joint-name joint) . ,(joint-property joint 'angle)))
		(rig-joints rig))))

(define (clamped-pose #;of rig)
  `(pose ,@(map (lambda (joint)
		  (let ((name (joint-name joint))
			(angle (joint-property joint 'angle))
			(hi (joint-property joint 'hi-stop))
			(lo (joint-property joint 'lo-stop)))
		    `(,name . ,((clamp lo hi) angle))))
		(rig-joints rig))))
