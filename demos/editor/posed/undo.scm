(define-module (editor posed undo)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (editor poses)
  #:use-module (editor modes)
  #:use-module (scum physics)
  #:export (save-rig-state!
	    restore-previous-rig-state!))

(define history #[])

(define (save-rig-state! rig)
  (let ((state (rig-state rig)))
    (match* #[history rig]
      ((or #f () (and (_ . _) (? (lambda (prev) (not (equal? prev state))))))
       (set! #[history rig]
	 `(,state ,@(or #[history rig] '())))))))

(define (restore-previous-rig-state! rig)
  (match #[history rig]
    ((state . older)
     (set-rig-state! #;of rig #;to state)
     (set! #[history rig] older))
    (_
     (format #t "no more undo information for ~s\n" rig))))
