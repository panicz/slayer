(define-module (editor movesets)
  #:use-module (extra common)
  #:export (
	    same-movesets?
	    ))

(define (same-movesets? moveset-a moveset-b)
  (let ((('moveset ('poses . poses-a)
		   ('sequences . sequences-a)) moveset-a)
	(('moveset ('poses . poses-b)
		   ('sequences . sequences-b)) moveset-b))
    (and (equivalent-set? (lambda ((name-a . configuration-a)
			      (name-b . configuration-b))
			    (and (eq? name-a name-b)
				 (same-set? configuration-a
					    configuration-b)))
			  poses-a poses-b)
	 (same-set? sequences-a sequences-b))))
