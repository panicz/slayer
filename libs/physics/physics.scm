(define-module (libs physics)
  #:use-module (extra common)
  #:export-syntax (define-rigs-for-sim))

;;(load-extension "physics" "init")

(define-syntax-rule (define-rigs-for sim (name rig) ...)
  (let* ((name (make-rig-for sim rig)) ...)
    (name-rig! sim (quote name) name) ...))

(use-modules (extra common))

(define* (body-named body #:key from)
  (body-named- body from))

(define (make-rig-for sim rig-def)
  (match rig-def
    (; STRUCTURE
     ('rig
      ('bodies
       body-spec ...)
      ('joints
       joint-defs ...))
     ;; ACTION
     (let ((rig (make-rig sim)))
       (for (name (type props ...)) in body-spec
	    (let ((body (make-body sim rig type name)))
	      (for ((? keyword? property) value) in (map-n 2 list props)
		   (set-body-property! body (keyword->symbol property) value))
	      #;(add-body! rig body name)))
       (for (type props ...) in joint-defs
	    (let ((joint (make-joint sim rig type))
		  (literal-value (match-lambda
				   ((? symbol? body-name)
				    (body-named body-name #:from rig))
				   ((property-name body-name)
				    (body-property (body-named body-name 
							       #:from rig)
				     property-name))
				   (else 
				    else))))
	      (for ((? keyword? property) value) in (map-n 2 list props)
		   (set-joint-property! joint (keyword->symbol property)
					(literal-value value)))))
       rig))))

;;(with-input-from-port (open-pipe "echo '{S, {NP, {DET, the}, {N, cat}}, {VP {V caught} {NP, {DT, a}, {N, mouse}}}}' | tr '{},' '() '" OPEN_READ) read)
;;(use-modules (ice-9 popen))

;; primitive C functions (to be implemented!)
;; make-rig
;; make-body
;; set-body-property!
;; body-property
;; add-body!
;; make-joint
;; body-named
;; set-joint-property!
;; joint-property
;; add-rig!

#|
(use-modules (oop goops)
	     (ice-9 match)
	     (srfi srfi-1))

(define frequency (make-hash-table))

(define (word-count w)
  (or (hash-ref frequency w) 0))

(define (count-word! w)
  (hash-set! frequency w (1+ (word-count w))))


(define* (terminals syntax-tree #:optional (action list))
  (match syntax-tree
    ((N T)
     (list (action N T)))
    ((N A B)
     (append (terminals A action) (terminals B action)))))

(define* (transform syntax-tree #:optional (action list))
  (match syntax-tree
    ((N T)
     (action N T))
    ((N A B)
     (list N (transform A action) (transform B action)))))

(terminals '(S (NP (DET the) (N cat)) (VP (V caught) (NP (DT a) (N mouse))))
	   (lambda(d w)(count-word! w)(list w (word-count w))))

(transform '(S (NP (DET the) (N cat)) (VP (V caught) (NP (DT a) (N mouse))))
	   (lambda (a b) a #;(list a b)))
|#
