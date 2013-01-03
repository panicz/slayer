(display "loading defs.scm...\n")
(set! %load-path (append '("./" "../")  %load-path))

(use-modules (oop goops)
	     (srfi srfi-1) 
	     (srfi srfi-2)
	     (srfi srfi-4)
	     (srfi srfi-4 gnu)
	     (srfi srfi-11)
	     (ice-9 match)
	     (ice-9 format)
	     (ice-9 optargs)
	     (ice-9 pretty-print)
	     (ice-9 local-eval)
	     (ice-9 regex)
	     (system base compile)
	     (system syntax)
	     (extra ref)
	     (extra vector-lib)
	     (extra common)
	     (extra function)
	     ((rnrs) :version (6)))

(define (local-lexicals id)
  (filter (lambda (x)
	    (eq? (syntax-local-binding x) 'lexical))
	  (syntax-locally-bound-identifiers id)))

(define-syntax lexicals
  (lambda (x)
    (syntax-case x ()
      ((lexicals) #'(lexicals lexicals))
      ((lexicals scope)
       (with-syntax (((id ...) (local-lexicals #'scope)))
	 #'(list (cons 'id id) ...))))))

(define-syntax function
  (lambda (x)
    (syntax-case x ()
      ((function args body ...)
       #'(let ((procedure (lambda args body ...))
	       (get-lexicals (lambda()(lexicals function))))
	   (set-procedure-property! procedure 'source '(function args body ...))
	   (set-procedure-property! procedure 'get-lexicals get-lexicals)
	   procedure)))))

(define (procedure-lexicals proc)
  (match-let ((('function args body ...) (procedure-source proc)))
	     (let ((vars (free-variables `(lambda ,args ,@body))))
	       (filter (match-lambda ((name . value) (in? name vars)))
		       ((procedure-property proc 'get-lexicals))))))

(define *stdout* (current-output-port))
(define *stdin* (current-input-port))
(define *stderr* (current-error-port))

(define (shout obj)
  (display (string-append (with-output-to-string (lambda()(display obj))) "\n") (current-error-port))
  obj)

(define *soft-port-sources* (make-hash-table))

(define (symbol->regexp s)
  (let ((s (symbol->string s)))
    ((compose 
      (lambda(s)(regexp-substitute/global #f "(^|[^.])([*+?])" s 'pre 1 "\\" 2 'post))) s)))
      
(define (make-soft-port* pv modes)
  (let ((port (make-soft-port pv modes)))
    (set! #[ *soft-port-sources* port ] (cons pv modes))
    port))

(define *stdio* 
  (make-soft-port
   (vector 
    (function (c) (write c *stdout*)) ; procedure accepting one character for output
    (function (s) (display s *stdout*)) ; procedure accepting a string for output
    (function () (force-output *stdout*)) ; thunk for flushing output
    (function () (read-char)) ; thunk for getting one character
    (function () (close-port *stdout*)) ; thunk for closing port (not by garbage collector)
    #f) ; (if present and not `#f') thunk for computing the number
   ;; of characters that can be read from the port without blocking
   "rw"))

(set-current-input-port *stdio*)
(set-current-output-port *stdio*)

(define (port-source port)
  (cond ((equal? port (current-input-port))
	 '(current-input-port))
	((equal? port (current-output-port))
	 '(current-output-port))
	((equal? port (current-error-port))
	 '(current-error-port))
	((and-let* ((source #[ *soft-port-sources* port ]))
	   (match-let (((pv . mode) source))
		      `(make-soft-port* 
			(vector ,@(map (lambda(p)
					(and (procedure? p)(procedure-source* p)))
				      (vector->list pv))) ,mode))))
	(#t '*stdio*)))

(define (vector-source v)
  (or (and-let* ((size (vector-length v))
		 ((> size 7)) 
		 (value-count (make-hash-table))
		 ((vector-for-each (lambda(e)
				     (set! #[ value-count e ]
					   (+ (or #[ value-count e ] 0) 1)))
				   v))
		 (value-score (sort (hash-map->list cons value-count)
				    (match-lambda* (((val1 . cnt1) (val2 . cnt2))
						   (> cnt1 cnt2)))))
		 (dominant-count (match value-score (((value . count) . rest) count)))
		 ((> (/ dominant-count size) 1/2)))
	(let ((dominant-value (match value-score (((value . count) . rest) value)))
	      (w (gensym "vec-")))
	  `(let ((,w (make-vector ,size ,(object-source dominant-value))))
	     ,@(filter-map (lambda(i)
			     (let ((i-th #[ v i ]))
			       (and (not (eq? i-th dominant-value))
				    `(vector-set! ,w ,i ,(object-source i-th)))))
			   (iota size))
	     ,w)))
      (vector-map object-source v)))

(define (hash-source value)
  (let ((h (gensym "hash-")))
    `(let ((,h (make-hash-table)))
       ,@(map (match-lambda ((key . value)
			     `(hash-set! ,h ,(object-source key) ,(object-source value))))
	      (hash-map->list cons value))
       ,h)))

(define (procedure-source* proc)
  (let ((lexis (procedure-lexicals proc)))
    (if (and (list? lexis) (not (null? lexis)))
	`(let* ,(map (match-lambda ((symbol . value)
				   `(,symbol ,(object-source value))))
		    lexis)
	   ,(procedure-source proc))
	(procedure-source proc))))

(define objects=>symbols (make-fluid (make-hash-table)))

(define (goops-symbol object)
  (or (and-let* ((objects=>symbols (fluid-ref objects=>symbols))
		 ((hash-table? objects=>symbols))
		 (symbol #[ objects=>symbols object ]))
	symbol)
      (let ((o (gensym "object-")))
	(set! #[ (fluid-ref objects=>symbols) object ] o)
	o)))

(define (goops-code-to-create-object object)
  (let ((class (class-of object)))
    `(make ,(class-name class))))
 
(define (goops-code-generator-to-assign-values-to-slots object)
  (lambda (name)
    (let ((class (class-of object)))
      (map (match-lambda((slot-name . info)
			 (let ((value (slot-ref object slot-name)))
			   `(slot-set! ,name ',slot-name ,(object-source value)))))
	   (class-slots class)))))

(define (object-source value)
  ;(shout `(object-source ,value))
  (cond 
   ((symbol? value)
    `',value)
   ((procedure? value)
    (or (procedure-name value) (procedure-source* value) 'noop))
   ((list? value)
    `(list ,@(map object-source value)))
   ((vector? value) 
    (vector-source value))
   ((hash-table? value)
    (hash-source value))
   ((port? value)
    (port-source value))
   ((is-a? value <image>)
    `(array->image ,(image->array value)))
   ((is-a? value <font>)
    '*default-font*)
   ((instance? value)
    (goops-symbol value))
   (#t value)))

(define (complete-source value)
  (with-fluids ((objects=>symbols (make-hash-table)))
    ;; we need to make entries for all the goops objects that appear in the
    ;; slots of this object, so we call the goops-code-generator... (maybe
    ;; this isn't too pretty, but it's ok for now)
    ((goops-code-generator-to-assign-values-to-slots value) (goops-symbol value))
    `(let ,(map (match-lambda ((object . symbol)
			       `(,symbol ,(goops-code-to-create-object object))))
		(hash-map->alist (fluid-ref objects=>symbols)))
       ,@(append-map (match-lambda((object . symbol)
				   ((goops-code-generator-to-assign-values-to-slots object) symbol)))
		     (hash-map->alist (fluid-ref objects=>symbols)))
       ,#[ objects=>symbols value ])))

(define (save file)
  (with-output-to-port (open-file file "w")
    (lambda()
      ;(display (string-append "saving to file "file":\n"))      
      (let ((kdn (key-bindings 'pressed))
	    (kup (key-bindings 'released))
	    (dump-key 
	     (lambda (table proc-name i)
	       (or (and-let* ((f #[ table i ])
			      ((not (in? f `(noop ,noop))))
			      ((procedure? f)))
		     `((,proc-name ,#[ *key-names* i ] 
				   ,(or (procedure-source* f)
					(procedure-name f) noop))))
		   '()))))
	(pretty-print
	 (cons 'begin (append-map (lambda(i)
				    (append (dump-key kdn 'keydn i)
					    (dump-key kup 'keyup i)))
				  (iota (vector-length kdn))))))
      (pretty-print `(mousemove ,(procedure-source* (mousemove-binding))))
      (pretty-print `(define *stage* ,(complete-source *stage*))))))


(display "loaded defs.scm\n")
