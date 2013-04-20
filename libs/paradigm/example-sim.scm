(let ((sim (make-simulation)))
  ;;(load-rigs #:for sim #:from "rigs/*.scm")
  (define-rigs-for sim
    (ground (with-input-from-file "rigs/ground.scm" read))
    (buggy '(rig
	     (bodies
	      (chassiss (box #:dimensions #(0.7 0.5 0.2) #:mass 1.0 
			     #:position #(0 0 0.5)))
	      (front-wheel (cylinder #:height 0.2 #:radius 0.5  
				     #:position #(3.5 0 0.4)))
	      (left-rear-wheel (cylinder #:height 0.2 #:radius 0.5  
					 #:position #(3.5 0 0.4)))
	      (right-rear-wheel (cylinder #:height 0.2 #:radius 0.5  
					  #:position #(3.5 0 0.4))))
	     (joints
	      (hinge-2 #:body-1 chassiss #:body-2 front-wheel 
		       #:anchor (position front-wheel) #:axis-1 #(0 0 1)
		       #:axis-2 #(0 1 0) #:suspension-erp 0.4
		       #:suspension-cfm 0.8)
	      (hinge-2 #:body-1 chassiss #:body-2 left-rear-wheel
		       #:anchor (position left-rear-wheel) #:axis-1 #(0 0 1)
		       #:axis-2 #(0 1 0) #:suspension-erp 0.4 
		       #:suspension-cfm 0.8 #:lo-stop 0 #:hi-stop 0)
	      (hinge-2 #:body-1 chassiss #:body-2 right-rear-wheel
		       #:anchor (position right-rear-wheel) #:axis-1 #(0 0 1) 
		       #:axis-2 #(0 1 0) #:suspension-erp 0.4 
		       #:suspension-cfm 0.8 #:lo-stop 0 #:hi-stop 0)))))
  (add-rig! sim 'ground)
  (add-rig! sim 'buggy)
  (add-rig! sim 'buggy #:position #(10 0 0))
  (while #t
    (simulation-step! sim)
    (for-each draw (rigs sim))
    (for-each handle (collisions sim))))
