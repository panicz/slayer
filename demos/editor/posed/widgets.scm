(define-module (editor posed widgets)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (slayer font)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (widgets base)
  #:use-module (widgets tab)
  #:use-module (widgets sprite)
  #:use-module (widgets sortable)
  #:use-module (widgets text-area)
  #:use-module (editor poses)
  #:use-module (editor posed undo)
  #:use-module (editor modes)
  #:use-module (editor control)
  #:use-module (editor camera)
  #:use-module (oop goops)
  #:export (<pose-editor-widget>))

;; if you ever conclude that the code contained in this module is terrible,
;; i'll have to agree with you

(define-class <selectable-named-entry> (<sprite>)
  (name #:init-keyword #:name)
  (selected? #:allocation #:each-subclass #:init-value (lambda (self) #f)
	     #:init-keyword #:selected?)
  (%image/selected #:init-value #f)
  (image
   #:allocation #:virtual
   #:slot-ref 
   (lambda (self) 
     (if (#[self 'selected?] self)
	 #[self '%image/selected]
	 #[self '%image]))
   #:slot-set! noop))

(define-method (initialize (self <selectable-named-entry>) args)
  (next-method) 
  (let* ((text (string-append "   " (->string #[self 'name]) "   "))
	 (image (render-text text *default-font* #x000000 #xffffff))
	 ((w h) (image-size image)))
    (set! #[self '%image] image)
    (set! #[self 'w] w)
    (set! #[self 'h] h)
    (set! #[self '%image/selected]
      (render-text text *default-font* #xffffff #xff7700))))

(define-class <pose> ()
  (name #:init-keyword #:name)
  (configuration #:init-keyword #:configuration))

(define-class <sequence> ()
  (poses-widget #:init-value #f)
  (sequence-widget #:init-value #f)
  (name #:init-keyword #:name)
  (poses #:init-keyword #:poses #:init-value '()))

(define-class <pose-entry> (<selectable-named-entry>)
  (owner #:init-keyword #:owner)
  (configuration #:init-value '() #:init-keyword #:configuration))

(define-method (initialize (self <pose-entry>) args)
  (next-method)
  (let* ((owner #[self 'owner])
	 (pose #[owner 'pose])
	 (rig #[owner 'rig]))
    (assert (is-a? owner <pose-editor-widget>))
    (set! #[self 'selected?]
      (lambda (self)
	(equal? #[self 'name] #[pose 'name])))
    (set! #[self 'activate]
      (lambda (x y)
	(set! #[pose 'name] #[self 'name])
	(set-pose! #;of rig #;to `(pose ,@#[self 'configuration])
			#:keeping (#[owner 'pivotal-body]))))))

(define-class <sequence-entry> (<selectable-named-entry>)
  (owner #:init-keyword #:owner)
  (sequence #:init-value '() #:init-keyword #:sequence))

(define-method (initialize (self <sequence-entry>) args)
  (next-method)  
  (let* ((owner #[self 'owner])
	 (sequence #[owner 'sequence]))
    (assert (is-a? owner <pose-editor-widget>))
    (set! #[self 'selected?]
      (lambda (self)
	(equal? #[self 'name] #[sequence 'name])))
    (set! #[self 'activate]
      (lambda (x y)
	(set-sequence! #[self 'name] #[self 'sequence] owner)))))

(define-class <pose-editor-widget> (<tab-widget>)
  (pivotal-body #:init-value noop #:init-keyword #:pivotal-body)
  (pause #:init-value #t #:init-keyword #:pause)
  (evaluations-file #:init-value "posed/evaluation.ss" 
		    #:init-keyword #:evaluations-file)
  (3d-view #:init-keyword #:3d-view)
  (moveset
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     `(moveset 
       (poses ,@(map (lambda (pose-entry)
		       `(,#[pose-entry 'name] ,@#[pose-entry 'configuration]))
		     (map #[_ 'target] 
			  #[self : 'poses-widget : 'children])))
       (sequences ,@(map (lambda (sequence-entry)
			   `(,#[sequence-entry 'name]
			     ,@#[sequence-entry 'sequence]))
			 (map #[_ 'target]
			      #[self : 'sequences-widget : 'children])))))
   #:slot-set!
   (lambda (self moveset)
     (let ((poses-widget #[self 'poses-widget])
	   (sequences-widget #[self 'sequences-widget])
	   (('moveset ('poses . poses)
		      ('sequences . sequences)) moveset))
       (set! #[poses-widget 'children] '())
       (set! #[sequences-widget 'children] '())
       (for (name . configuration) in poses
	 (add-child! (make <pose-entry> #:name name
			   #:configuration configuration
			   #:owner self)
		     #;to poses-widget))
       (for (name . poses) in sequences
	 (add-child! (make <sequence-entry> #:name name
			   #:sequence poses #:owner self)
		     #;to sequences-widget)))))
  (poses-widget #:init-value #f)
  (sequence-widget #:init-value #f)
  (sequences-widget #:init-value #f)
  (pose #:init-thunk (lambda () (make <pose> #:name 'unnamed-pose
				 #:configuration #f)))
  (sequence #:init-thunk (lambda () (make <sequence> #:name 'unnamed-sequence)))
  (rig #:init-keyword #:rig))

(define-method (pose-configuration pose-name #;in (editor <pose-editor-widget>))
  (and-let* ((pose-entry (find (lambda (pose-entry)
				 (equal? #[pose-entry 'name] pose-name))
			       (map #[_ 'target] 
				    #[editor : 'poses-widget : 'children]))))
    #[pose-entry 'configuration]))

(define-method (set-sequence! sequence-name poses-names
			      #;in (editor <pose-editor-widget>))
  (let ((sequence-widget #[editor 'sequence-widget])
	(sequence #[editor 'sequence]))
    (set! #[sequence 'name] sequence-name)
    (set! #[sequence 'poses] poses-names)
    (for child in #[sequence-widget 'children]
      (set! #[child 'target] #f))
    (set! #[sequence-widget 'children] '())
    (for name in (reverse poses-names)
      (and-let* ((configuration (pose-configuration name #;in editor)))
	(add-child! (make <pose-entry> #:name name
			  #:configuration configuration #:owner editor)
		    #;to sequence-widget)))))

(define (play-sequence! pose-names editor)
  (when #[editor 'pause]
    (set! #[editor 'pause] #f))
  (let ((sequence (map (lambda (name)
			 `(pose ,@(pose-configuration name #;in editor)))
		       pose-names)))
    (initiate-sequence! sequence #[editor 'rig])))

(define-method (shift-pose! #;by k #;in (editor <pose-editor-widget>))
  (let ((the-rig #[editor 'rig])
	(the-pose #[editor 'pose])
	(poses-widget #[editor 'poses-widget]))
  (and-let* ((poses (map #[_ : 'target : 'name]
			 #[poses-widget 'children]))
	     ((not (null? poses)))
	     (n (length #;of poses))
	     (i (or (order #;of #[the-pose 'name] #;in poses
				#:identified-using equal?)
		    0))
	     (i' (modulo (+ i k) n))
	     (pose-name #[poses i'])
	     (configuration (pose-configuration pose-name #;in editor)))
    (set! #[the-pose 'name] pose-name)
    (set-pose! #;of the-rig #;to `(pose ,@configuration)
		    #:keeping (#[editor 'pivotal-body])))))

(define (abbreviate name)
  (let* ((name-string (->string name))
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
    (string-append name (make-string (max 0 (- 12 n)) #\space))))

(define-method (initialize (self <pose-editor-widget>) args)
  ;; yes, functions shouldn't be written this way
  ;; (although if you imagine that this code is html-alike, it should
  ;; be easier). hs-minor-mode definitely recommended!
  (next-method)
  (let* ((the-pose #[self 'pose])
	 (('pose . configuration) (pose #;of #[self 'rig])))
    (set! #[the-pose 'configuration] configuration))
  (let ((poses-widget (make <widget-distributor> #:min-w 120 #:min-h 150))
	(sequence-widget (make <sortable-container> #:min-w 120 #:min-h 150))
	(sequences-widget (make <sortable-container> #:min-w 120 #:min-h 100)))
    (set! #[self 'poses-widget] poses-widget)
    (set! #[self 'sequence-widget] sequence-widget)
    (set! #[sequence-widget 'accepts-widget?]
      (lambda (box placeholder) 
	(and (is-a? #[box 'target] <pose-entry>)
	     (let ((parent #[box 'original-parent]))
	       (or (eq? parent poses-widget)
		   (and placeholder (eq? parent sequence-widget)))))))
    (set! #[self 'sequences-widget] sequences-widget)
    (set! #[sequences-widget 'accepts-widget?]
      (lambda (box placeholder)
	(and (is-a? #[box 'target] <sequence-entry>) placeholder))))
  (let ((the-pose #[self 'pose])
	(the-rig #[self 'rig])
	(evaluations-file #[self 'evaluations-file])
	(sequence #[self 'sequence])
	(sequence-widget #[self 'sequence-widget])
	(sequences-widget #[self 'sequences-widget])
	(poses-widget #[self 'poses-widget]))
    (let ((file-menu 
	   ((layout)
	    (label "         --- file options ---        ")
	    (label " --- evaluations (f1 after sexp) --- ")
	    (make <text-area> #:w 220 #:h 200 #:text-color #x000000
		  #:background-color #xffffff
		  #:text ""
		  #:on-create 
		  (lambda (self)
		    (when (file-exists? evaluations-file)
		      (let ((text (with-input-from-file evaluations-file
				    (lambda () (read-delimited "")))))
			(set! #[self 'text] text)
			(catch #t
			  (lambda ()
			    (let ((s-expressions (with-input-from-string text
						   read-s-expressions)))
			      (for sexp in s-expressions
				(eval sexp (current-module)))))
			  (lambda args
			    (format *stderr* "evaluation failed: ~s\n" args)))
			)))
		  #:on-exit
		  (lambda (self)
		    (with-output-file evaluations-file
		      (display #[self 'text]))))
	    (label "       --- camera settings ---       ")
	    ((layout #:lay-out lay-out-horizontally)
	     (button #:text " [ ahead (1) ] " #:action
		     (look ahead #;at the-rig #;using 
			   #[self : '3d-view : 'camera]))
	     (label " ")
	     (button #:text " [ back (shift+1) ] " #:action 
		     (look back #;at the-rig #;using 
			   #[self : '3d-view : 'camera])))
	    ((layout #:lay-out lay-out-horizontally)
	     (button #:text " [ left (2) ] " #:action
		     (look left #;at the-rig #;using 
			   #[self : '3d-view : 'camera]))
	     (label "  ")
	     (button #:text " [ right (shift+2) ]" #:action
		     (look right #;at the-rig #;using 
			   #[self : '3d-view : 'camera])))
	    ((layout #:lay-out lay-out-horizontally)
	     (button #:text " [ up (3) ] " #:action
		     (look up #;at the-rig #;using 
			   #[self : '3d-view : 'camera]))
	     (label "    ")
	     (button #:text " [ down (shift+3) ] " #:action
		     (look down #;at the-rig #;using 
			   #[self : '3d-view : 'camera])))
	    
	    (label "         --- quick help ---          ")))
	  (pose-editor
	   ((layout)
	    (property-editor 
	     the-pose 
	     ("name: " #[the-pose 'name]))
	    ((layout #:lay-out lay-out-horizontally)
	     (label " ")
	     (button #:text " [ <<< ] "
		     #:action (lambda (x y)(shift-pose! #;by -1 #;in self)))
	     (label " ")
	     (button
	      #:text " [ save ] "
	      #:action
	      (lambda (x y)
		(let ((('pose . configuration) (pose #;of the-rig)))
		  (add/overwrite! (make <pose-entry> 
				    #:name #[the-pose 'name]
				    #:configuration configuration
				    #:owner self)
				  #;to poses-widget
				       #;overwriting-if
				       (lambda (entry) 
					 (equal? #[entry 'name]
						 #[the-pose 'name]))))))
	     (label " ")
	     (button #:text " [ >>> ] "
		     #:action (lambda (x y)(shift-pose! #;by +1 #;in self)))
	     (label " "))
	    ((layout #:lay-out lay-out-horizontally)
	     (button 
	      #:text "  [ mirror ]  "
	      #:action 
	      (lambda (x y)
		(save-rig-state! the-rig)
		(set-pose! #;of the-rig
				#;to (mirror-pose (pose #;of the-rig))
				     #:keeping (#[self 'pivotal-body]))))
	     (label " ")
	     (button 
	      #:text " [ left->right ] "
	      #:action
	      (lambda (x y)
		(save-rig-state! the-rig)
		(set-pose! #;of the-rig
				#;to (left->right-pose (pose #;of the-rig))
				     #:keeping (#[self 'pivotal-body])))))
	    ((layout #:lay-out lay-out-horizontally)
	     (label "       ")
	     (button 
	      #:text "  [ apply-stops ]  "
	      #:action 
	      (lambda (x y)
		(save-rig-state! the-rig)
		(apply-stops! #;to the-rig 
				   #:keeping (#[self 'pivotal-body]))))
	     (label "       "))))
	  (sequence-editor
	   ((layout #:lay-out lay-out-vertically)
	    (property-editor 
	     sequence
	     ("name: " #[sequence 'name]))
	    ((layout #:lay-out lay-out-horizontally)
	     (label "         ")
	     (button
	      #:text "  [ save ]  "
	      #:action
	      (lambda (x y)
		(add/overwrite! 
		 (make <sequence-entry> 
		   #:name #[sequence 'name]
		   #:sequence (map #[_ : 'target : 'name]
				   #[sequence-widget 'children])
		   #:owner self)
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
	     (button 
	      #:text "  [ play ]  "
	      #:action 
	      (lambda (x y)
		(<< "playing " #[sequence 'name] #[sequence 'poses])
		(play-sequence! #[sequence 'poses] #;in self)))
	     (label "         "))
	    
	    (label "       --- sequences ---      ")
	    sequences-widget)))
      (for (name . value) in #[the-pose 'configuration]
	(add-child! 
	 (make <numeric-input> #:w 200 #:h 12 
	       #:label (abbreviate name)
	       #:target the-pose
	       #:accessor 
	       (make-procedure-with-setter
		(lambda (a-pose) (assoc-ref (pose #;of the-rig) name))
		(lambda (a-pose value)
		  (let ((('pose . configuration) (pose #;of the-rig)))
		    (set-pose! 
		     #;of the-rig
			  #;to `(pose ,@(map (lambda ((joint . angle))
					       (if (eq? joint name)
						   `(,joint . ,value)
						   `(,joint . ,angle)))
					     configuration))
			       #:keeping (#[self 'pivotal-body]))))))
	 #;to pose-editor))
      (add-child! 
       ((layout #:lay-out lay-out-vertically)
	(label "")
	(button 
	 #:text "  [ delete pose ]  "
	 #:action 
	 (lambda (x y)
	   (save-rig-state! the-rig)
	   (let ((name #[the-pose 'name]))
	     (set! #[poses-widget 'children]
	       (remove (lambda (proxy)
			 (equal? #[proxy : 'target : 'name] name))
		       #[poses-widget 'children])))
	   (shift-pose! #;by +1 #;in self))))
       #;to pose-editor)

      (add-tab! file-menu #;under-name "[file]" #;to self)
      (add-tab! pose-editor #;under-name "[pose]" #;to self)
      (add-tab! sequence-editor #;under-name "[sequence]" #;to self)
      (add-tab! ((layout)) #;under-name "[hide]" #;to self))))
