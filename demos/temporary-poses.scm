
(define (null-pose #;for rig)
  (let ((joints (rig-joints rig)))
    `(pose (,(joint-name joints) . 0.0) ...)))

(define (combine-poses a b)
  (match-let ((('pose . pose-a) a)
	      (('pose . pose-b) b))
    `(pose ,@(replace-alist-bindings pose-a pose-b))))

(define stand (null-pose the-legs))

(define left-up 
  (combine-poses stand `(pose (left-knee . -0.5)
			      (left-pelvis . 0.5)
			      (right-ankle . #f)
			      (balance . 0.5)
			      )))
(define left-front 
  (combine-poses left-up `(pose (left-knee . -0.2)
				(left-ankle . #f)
				(right-knee . #f)
				(balance . 0.8)
				)))
(define left-straight
  (combine-poses left-front `(pose (left-knee . 0)
				   (left-ankle . 0)
				   (left-pelvis . 0)
				   (balance . 0.5)
				   (right-pelvis . 0.5))))


(define right-up
  (combine-poses stand `(pose (right-knee . 0.5)
			      (right-pelvis . -0.5)
			      (left-ankle . #f)
			      (balance . -0.5)
			      )))
(define right-front 
  (combine-poses right-up `(pose (right-knee . 0.2)
				(right-ankle . #f)
				(left-knee . #f)
			      (balance . -0.8)
				)))
(define right-straight
  (combine-poses right-front `(pose (right-knee . 0)
				   (right-ankle . 0)
				   (right-pelvis . 0)
				   (balance . -0.5)
				   (left-pelvis . -0.5))))

(define duck
  `(pose (left-pelvis . 2.2)
	 (left-knee . -2.2)
	 (left-ankle . 0.9)
	 (right-pelvis . -2.2)
	 (right-knee . 2.2)
	 (right-ankle . -0.9)))

(keydn 0 (lambda () (set-pose! #;of the-legs #;to stand)))
(keydn 1 (lambda () (set-pose! #;of the-legs #;to left-up)))
(keydn 2 (lambda () (set-pose! #;of the-legs #;to left-front)))
(keydn 3 (lambda () (set-pose! #;of the-legs #;to left-straight)))
(keydn 4 (lambda () (set-pose! #;of the-legs #;to right-up)))
(keydn 5 (lambda () (set-pose! #;of the-legs #;to right-front)))
(keydn 6 (lambda () (set-pose! #;of the-legs #;to right-straight)))
(keydn 9 (lambda () (set-pose! #;of the-legs #;to duck)))
