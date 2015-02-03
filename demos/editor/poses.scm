(define-module (editor poses)
  #:use-module (extra common)
  #:use-module (editor relations)
  #:use-module (scum physics)
  #:export (null-pose 
	    combine-poses
	    pose
	    clamped-pose
	    mirror-pose
	    left->right-pose
	    mass-center
	    ))

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

(define (mirror-pose original-pose)
  (let ((('pose . configuration) original-pose))
    `(pose ,@(map (lambda ((name . value))
		    (cond ((symbol-match "^left-(.*)$" name)
			   => (lambda ((suffix))
				`(,(symbol-append 'right- suffix) . ,value)))
			  ((symbol-match "^right-(.*)$" name)
			   => (lambda ((suffix))
				`(,(symbol-append 'left- suffix) . ,value)))
			  ((symbol-match "turn$" name)
			   `(,name . ,(- value)))
			  (else
			   `(,name . ,value))))
		  configuration))))

(define (left->right-pose original-pose)
  (let ((('pose . configuration) original-pose))
    `(pose ,@(append-map 
	      (lambda ((name . value))
		(cond ((symbol-match "^left-(.*)$" name)
		       => (lambda ((suffix))
			    `((,(symbol-append 'right- suffix) . ,value)
			      (,name . ,value))))
		      ((symbol-match "^right-" name)
		       '())
		      (else
		       `((,name . ,value)))))
	      configuration))))

(define (mass-center #;of rig)
  (let* ((bodies (rig-bodies rig))
	 (((positions . masses) ...) (map (lambda (body) 
					    `(,(body-property body 'position)
					      . ,(body-property body 'mass)))
					  bodies)))
    (/ (fold + #f32(0 0 0) (map * positions masses))
       (fold + 0.0 masses))))
