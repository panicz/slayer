(define editor (make <tab-widget>))

(add-child! editor #;to *stage*)

(define-class <pose> ()
  (name #:init-keyword #:name)
  (configuration #:init-thunk
		 (lambda ()
		   (let ((('pose . configuration) (current-configuration)))
		     configuration))))

(define pose (make <pose> #:name 'unnamed-pose))

(define-class <sequence> ()
  (name #:init-keyword #:name))

(define sequence (make <sequence> #:name 'unnamed-sequence))

(define poses-widget (make <widget-distributor> #:min-w 120 #:min-h 100))

(define sequence-widget (make <sortable-container> #:min-w 120 #:min-h 150))

(set! #[sequence-widget 'accepts-widget?]
  (lambda (box placeholder) 
    (and (is-a? #[box 'target] <pose-entry>)
	 (let ((parent #[box 'original-parent]))
	   (or (eq? parent poses-widget)
	       (and placeholder (eq? parent sequence-widget)))))))

(define sequences-widget 
  (make <sortable-container> 
    #:min-w 120 #:min-h 100
    #:blocked? #t))

(define (load-moveset! moveset)
  (let ((('moveset ('poses . poses)
		   ('sequences . sequences)) moveset))
    (set! #[poses-widget 'children] '())
    (set! #[sequences-widget 'children] '())
    (for (name . configuration) in poses
      (add-child! (make <pose-entry> #:name name
			#:configuration configuration)
		  #;to poses-widget))
    (for (name . poses) in sequences
      (add-child! (make <sequence-entry> #:name name
			#:sequence poses)
		  #;to sequences-widget))))

(define (current-moveset)
  `(moveset 
    (poses ,@(map (lambda (pose-entry)
		    `(,#[pose-entry 'name] ,@#[pose-entry 'configuration]))
		  (map #[_ 'target] 
		       #[poses-widget 'children])))
    (sequences ,@(map (lambda (sequence-entry)
			`(,#[sequence-entry 'name]
			  ,@#[sequence-entry 'sequence]))
		      (map #[_ 'target]
			   #[sequences-widget 'children])))))

(define-class <named-entry> (<sprite>)
  (name #:init-keyword #:name))

(define-method (initialize (self <named-entry>) args)
  (next-method)  
  (set! #[self 'image] (render-text 
			(string-append "   " (->string #[self 'name]) "   ")
			*default-font*
			#x000000 #xffffff)))

(define-class <pose-entry> (<named-entry>)
  (configuration #:init-value '() #:init-keyword #:configuration))

(define-method (initialize (self <pose-entry>) args)
  (next-method)  
  (set! #[self 'activate]
    (lambda (x y)
      (let ((chest (body-named 'chest #;from the-rig)))
	(set-pose! #;of the-rig #;to `(pose ,@#[self 'configuration])
			#:keeping chest)))))

(define-class <sequence-entry> (<named-entry>)
  (sequence #:init-value '() #:init-keyword #:sequence))


(define-method (initialize (self <sequence-entry>) args)
  (next-method)  
  (set! #[self 'activate]
    (lambda (x y)
      ;; load given sequence
      (<< #[self 'name]))))

(define file-menu 
  ((layout)
   (label "         --- file options ---       ")
   (label "       --- camera settings ---      ")
   (label "         --- quick help ---         ")))

(define pose-editor
  ((layout)
   (property-editor 
    pose 
    ("name: " #[pose 'name]))
   ((layout #:lay-out lay-out-horizontally)
    (label "           ")
    (button #:text "  [ save ]  "
	    #:action
	    (lambda (x y)
	      (let ((('pose . configuration) (current-configuration)))
		(add/overwrite! (make <pose-entry> #:name #[pose 'name]
				      #:configuration configuration)
				#;to poses-widget
				     #;overwriting-if
				     (lambda (entry) (equal? #[entry 'name]
							#[pose 'name]))))))
    (label "          "))
   ((layout #:lay-out lay-out-horizontally)
    (button #:text "  [ <<< ]  "
	    #:action (lambda (x y)
		       (<< "previous-pose")))
    (label "           ")
    (button #:text "  [ >>> ]  "
	    #:action (lambda (x y)
		       (<< "next-pose"))))
   (label "                                 ")))

(let ((chest (body-named 'chest #;from the-rig)))
  (for (name . value) in #[pose 'configuration]
    (add-child! 
     (make <numeric-input> #:w 200 #:h 12 
	   #:label (let* ((name-string (->string name))
			  ((prefix parts ...) (string-split name-string #\-))
			  (name (string-join
				 `(,(match prefix
				      ("left" "L")
				      ("right" "R")
				      (_ prefix)) 
				   ,@(map (lambda (part)
					    (if (< (string-length part) 4)
						part
						(substring part 0 4)))
					  parts))
					       "-"))
			  (n (string-length name)))
		     (string-append name (make-string 
					  (max 0 (- 12 n)) #\space)))
	   #:target pose
	   #:accessor 
	   (make-procedure-with-setter
	    (lambda (pose) (assoc-ref (current-configuration) name))
	    (lambda (pose value)
	      (let ((('pose . configuration) (current-configuration)))
		(set-pose! #;of the-rig
				#;to `(pose ,@(map (lambda ((joint . angle))
						     (if (eq? joint name)
							 `(,joint . ,value)
							 `(,joint . ,angle)))
						   configuration))
				     #:keeping chest)))))
     #;to pose-editor)))

(define sequence-editor
  ((layout)
   (property-editor 
    sequence
    ("name: " #[sequence 'name]))
   ((layout #:lay-out lay-out-horizontally)
    (label "         ")
    (button #:text "  [ save ]  "
	    #:action (lambda (x y)
		       (add/overwrite! 
			(make <sequence-entry> #:name #[sequence 'name]
			      #:sequence (map #[_ : 'target : 'name]
					      #[sequence-widget 'children]))
			#;to sequences-widget
			     #;overwriting-if
			     (lambda (entry) (equal? #[entry 'name]
						#[sequence 'name])))))
    (label "         "))

   (label "         --- poses ---        ")
   poses-widget
   (label "   --- current sequence ---   ")
   sequence-widget
   ((layout #:lay-out lay-out-horizontally)
    (label "         ")
    (button #:text "  [ play ]  "
	    #:action (lambda (x y)
		       (<< "playing " #[sequence 'name])))
    (label "         "))

   (label "       --- sequences ---      ")
   sequences-widget
   ))

(add-tab! file-menu #;under-name "[file]" #;to editor)
(add-tab! pose-editor #;under-name "[pose]" #;to editor)
(add-tab! sequence-editor #;under-name "[sequence]" #;to editor)
(add-tab! ((layout)) #;under-name "[hide]" #;to editor)