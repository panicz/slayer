(define-module (extra common)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-18)
  #:use-module (srfi srfi-31)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 expect)
  #:use-module (system base compile)

  #:use-module ((rnrs) :version (6) 
		:select (make-bytevector 
			 utf8->string string->utf8 
			 bytevector-fill!)
		)

  ;;  #:use-module ((rnrs) #:version (6))
  #:re-export (;; srfi-1
	       iota
	       proper-list? dotted-list? null-list? not-pair?
	       first second third fourth fifth sixth seventh eighth ninth tenth
	       car+cdr take drop take-right drop-right split-at last
	       concatenate zip unzip1 unzip2 unzip3 unzip4 unzip5
	       fold fold-right pair-fold reduce reduce-right unfold unfold-right
	       append-map filter-map partition remove remove! find find-tail
	       take-while drop-while span break any every list-index
	       delete delete! delete-duplicates delete-duplicates!
	       lset<= lset= lset-adjoin lset-union lset-intersection
	       lset-difference lset-xor lset-diff+intersection
	       ;; srfi-2, srfi-11
	       and-let* let-values let*-values
	       ;; ice-9 match
	       match match-let match-let* match-lambda match-lambda*
	       ;; ice-9 regex
	       string-match match:substring regexp-substitute
	       regexp-substitute/global regexp/icase regexp/newline
	       regexp/basic regexp/extended regexp/notbol regexp/noteol
	       regexp? list-matches fold-matches regexp-match?
	       ;; ice-9 optargs
	       let-keywords let-keywords* let-optional let-optional*
	       ;; ice-9 expect
	       expect expect-strings expect-port expect-timeout
	       expect-timeout-proc expect-eof-proc expect-char-proc
	       expect-strings-compile-flags expect-strings-exec-flags
	       expect-select expect-regexec
	       ;; r6rs
	       make-bytevector 
	       utf8->string string->utf8 
	       bytevector-fill!
	       pretty-print format
	       )
  #:export (
	    expand-form ?not ?and ?or in? 
	    hash-keys hash-values hash-copy hash-size
	    make-applicable-hash-table
	    union intersection difference adjoin unique
	    map-n for-each-n equivalence-classes
	    atom? symbol<
	    insert rest
	    tree-find tree-map
	    depth array-map array-append
	    kw-list->hash-map
	    list->uniform-vector list->uniform-array
	    contains-duplicates?
	    module->hash-map module->list module-symbols
	    symbol->list hash-map->alist 
	    alist->hash-map
	    last-sexp-starting-position
	    properize flatten
	    cart all-tuples all-pairs all-triples
	    take-at-most drop-at-most split-before
	    remove-keyword-args
	    array-size
	    random-array
	    read-string write-string
	    with-output-to-utf8
	    list-directory shell
	    << die
	    real->integer
	    make-locked-mutex
	    last-index indexed
	    demand
	    )
  #:export-syntax (TODO \ for if* matches? equals?
		   safely export-types
		   define-curried publish define-accessors
		   supply applicable-hash applicable-hash-with-default
		   hash-table
		   rec expand letrec-macros
		   transform! increase! decrease! multiply!
		   push! pop!)
  #:replace ((cdefine . define)
	     (cdefine* . define*))
  )

(define (demand to-do-something-with . args)
  (call/cc (lambda(go-on)
	     (apply throw 'demand go-on to-do-something-with args))))

(define-syntax supply
  (syntax-rules ()
    ((_ (((to-do-something-with . args) do-what ...) ...)
	actions ...)
     (let ((handlers (make-hash-table))
	   (unsupported (lambda details
			  (apply throw 'unsatisfied-demand
				 details))))
       (hash-set! handlers (quote to-do-something-with)
		  (lambda args do-what ...))
       ...
       (catch 'demand
	 (lambda () actions ...)
	 (lambda (key go-on demand . args*)
	   (go-on (apply (hash-ref handlers demand unsupported) args*))))))))

(define-syntax-rule (hash-table (key value) ...)
  (let ((new-hash-table (make-hash-table)))
    (hash-set! new-hash-table key value)
    ...
    new-hash-table))

(define (split-before criterion list)
  (split-at list (or (list-index criterion list)
		     (length list))))

(define-syntax cdefine
  (syntax-rules ()
    ((_ ((head . tail) . rest) body body* ...)
     (cdefine (head . tail)
       (lambda rest body body* ...)))
    ((_ (head . rest) body body* ...)
     (define head
       (lambda rest body body* ...)))
    ((_ . rest)
     (define . rest))))

(define-syntax cdefine*
  (syntax-rules ()
    ((_ ((head . tail) . rest) body body* ...)
     (cdefine* (head . tail)
       (lambda* rest body body* ...)))
    ((_ (head . rest) body body* ...)
     (define* head
       (lambda* rest body body* ...)))
    ((_ . rest)
     (define* . rest))))

;; `define-curried' is not a curried definition!
;; It defines a new macro which generates an appropreate
;; procedure, if insufficient number of arguments is given.
;; A good example is given below, in the `matches?' macro
;; definition

(define-macro (define-curried signature . body)
  (match signature
    ((name args ...)	  
     `(define-syntax ,name
	(syntax-rules ()
	  ((_ ,@args)
	   (begin ,@body))
	  ,@(let loop ((args* args))
	      (match args*
		(() '())
		((first ... last)
		 (cons `((_ ,@first #;...)
			 (lambda(,last)(,name ,@args*)))
		       (loop first #;...))))))))))

;; If the `matches?' macro is called with two arguments, it behaves
;; as a regular binary predicate, which returns true if the second
;; argument matches the first (in terms of Wright/Shinn pattern matcher,
;; a.k.a. (ice-9 match) module). If the last argument is omitted,
;; it returns a procedure which checks whether its argument matches
;; a given pattern.

(define-curried (matches? pattern x)
  (match x 
    (pattern #t)
    (else #f)))

(define-curried (equals? value x)
  (equal? value x))

;; The `publish' macro is used to provide means to separate public
;; definitions from private ones (such that are visible only from within
;; the public procedures and from within themselves).
;; For example, the form
;; 
;; (publish
;;   (define (f x) (+ a x))
;;   (define (g y) (* a y))
;;  where
;;   (define a 5))
;;
;; is equivalent to
;;
;; (begin
;;   (define f #f)
;;   (define g #f)
;;   (let ()
;;     (define a 5)
;;     (set! f (let () (define (f x) (+ a x)) f))
;;     (set! g (let () (define (g x) (* a y)) g))))


(define-macro (publish . definitions)
  (define (interface-name interface)
    (match interface
      ((head . tail)
       (interface-name head))
      ((? symbol? name)
       name)))
  (let-values (((public-definitions where&private-definitions)
		(split-before (equals? 'where) definitions)))
    `(begin ,@(map (match-lambda
		       ((define-variant interface . body)
			`(define ,(interface-name interface) #f)))
		   public-definitions)
	    (let ()
	      ,@(match where&private-definitions
		  (('where . private-definitions)
		   private-definitions)
		  (() '()))
	      ,@(map (match-lambda
			 ((define-variant interface . body)
			  (let ((name (interface-name interface)))
			    `(set! ,name
				   (let () 
				     (,define-variant ,interface . ,body)
				     ,name)))))
		     public-definitions)))))

(define-syntax rec
  (syntax-rules ()
    ((rec (NAME . VARIABLES) . BODY)
     (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
    ((rec NAME EXPRESSION)
     (letrec ( (NAME EXPRESSION) ) NAME))))

(define-syntax-rule (TODO something ...) (rec (f . x) f))

;; `letrec-macros' behaves similar to `let-syntax', but it is restricted
;; to take `macro' keyword in place of the latter's `syntax-rules' (or its
;; equivalents), because unlike syntax-transformers, macros in guile are
;; no longer first-class objects
(define-syntax letrec-macros
  (syntax-rules (macro)
    ((_ ((name (macro args definition ...)) ...) body ...)
     (let ()
       (define-macro (name . args) definition ...)
       ...
       body ...))))

(define (make-locked-mutex)
  (let ((m (make-mutex)))
    (lock-mutex m)
    m))

(define* (die #:optional (message #f))
  (if message
      (let ((stderr (current-error-port)))
	(display message stderr)
	(newline stderr)))
  (quit))

(define (real->integer number)
  (let ((near (round number)))
    (if (exact? number)
	number
	(inexact->exact number))))

(define (write-string object)
  (with-output-to-string (lambda()(write object))))

(define (read-string string)
  (with-input-from-string string read))

(define (unique lst)
  (let ((result (make-hash-table)))
    (for-each (lambda(item)(hash-set! result item #t)) lst)
    (hash-map->list (lambda(k v)k) result)))

(define* (insert new l condition 
		 #:key (prefix '()) (right-bound +inf.0))
  (match l
    ((this next . rest)
     (cond ((and (null? prefix) (condition new this next))
	    (append prefix (list new this next) rest))
	   ((condition this new next)
	    (append prefix (list this new next) rest))
	   (else 
	    (insert new (cons next rest) condition 
		    #:prefix (append prefix (list this))
		    #:right-bound right-bound))))
    ((last)
     (cond ((and (null? prefix) (condition new last right-bound))
	    (append prefix (list new last)))
	   ((condition last new right-bound)
	    (append prefix (list last new)))
	   (else
	    (append prefix (list last)))))
    (()
     (append prefix (list new)))))

(define (symbol< a b)
  (string< (symbol->string a) (symbol->string b)))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define rest cdr)

(define (symbol->list s)
  (string->list (symbol->string s)))

(define (intersection . lsets)
  (apply lset-intersection equal? lsets))

(define (union . lsets)
  (apply lset-union equal? lsets))

(define (difference lset . lsets)
  (apply lset-difference equal? lset lsets))

(define (adjoin lset . elements)
  (apply lset-adjoin equal? lset elements))

(define (depth x)
  (if (list? x)
      (1+ (apply max (map depth x)))
      0))

(define (tree-find pred tree)
  (if (null? tree)
      #f
      (or (find pred tree)
	  (tree-find pred (concatenate (filter list? tree))))))

(define (tree-map proc tree)
  (map (lambda (item)
	 (if (pair? item)
	     (tree-map proc item)
	     (proc item)))
       tree))

(define (equivalence-classes equivalent? set)
  (let next-item ((set set)(result '()))
    (match set
      (()
       result)
      ((item . set)
       (match result
	 (()
	  (next-item set `((,item) . ,result)))
	 ((this . next)
	  (let next-class ((past '()) (present this) (future next))
	    (match present
	      ((paradigm . _)
	       (if (equivalent? item paradigm)
		   (next-item set `((,item . ,present)
				    . (,@past ,@future)))
		   (match future
		     (()
		      (next-item set `((,item) ,@result)))
		     ((this . next)
		      (next-class `(,present . ,past) this next)))))))))))))

(define-syntax safely 
  (syntax-rules ()
    ((_ sexp) 
     (catch #t (lambda () (values sexp #t))
       (lambda (key . args)
	 (with-output-to-port (current-output-port)
	   (lambda()
	     ;;(backtrace)
	     (display `(error calling sexp : ,key ,args))
	     (values (if #f #f) #f))))
       (lambda args (backtrace))))))

(define-syntax-rule (export-types symbol ...)
  `((symbol ,symbol) ...))

(define-syntax transform!
  (syntax-rules ()
    ((_ fx x args ...)
     (set! x (fx x args ...)))))
  
(define-macro (increase! x . args)
  `(transform! + ,x ,@args))

(define-macro (decrease! x . args)
  `(transform! - ,x ,@args))

(define-macro (multiply! x . args)
  `(transform! * ,x ,@args))

(define-macro (push! l e) 
  `(set! ,l (cons ,e ,l)))

(define-syntax-rule (pop! l)
  (let ((result (car l)))
    (set! l (cdr l))
    result))

(define (last-index array)
  (1- (vector-length array)))

(define (indexed list)
  (zip (iota (length list)) list))

(define-syntax for
  (syntax-rules (in .. =>)
    ((_ x in first .. last body ...)
     (let ((final last))
       (let loop ((x first))
	 (if (<= x final)
	     (begin
	       body ...
	       (loop (1+ x)))))))
     ((_ (key => value) in hash-map body ...)
      (hash-for-each (match-lambda* ((key value) body ...) 
		       (else (throw 'invalid-for-clause else)))
		     hash-map))
     ((_ x in list body ...)
      (for-each (match-lambda (x body ...)
		  (else (throw 'invalid-for-clause else))) list))))

(define (hash-size hash-map)
  (length (hash-values hash-map)))

(define (hash-keys hash-map)
  (hash-map->list (lambda(key value) key) hash-map))

(define (hash-values hash-map)
  (hash-map->list (lambda(key value) value) hash-map))

(define (hash-copy h)
  (let ((result (make-hash-table)))
    (for (key . value) in (hash-map->list cons h)
	 (hash-set! result key value))
    result))

(define* (make-applicable-hash-table #:optional (default #f) . initial-values)
  (let ((hash (make-hash-table)))
    (for (key value) in initial-values
	 (hash-set! hash key value))
    (case-lambda
      (() (hash-map->list list hash))
      ((key) (hash-ref hash key default))
      ((key value) (hash-set! hash key value)))))

(define-syntax-rule (applicable-hash-with-default default (key value) ...)
  (make-applicable-hash-table default `(,key ,value) ...))

(define-syntax-rule (applicable-hash (key value) ...)
  (applicable-hash-with-default #f (key value) ...))

(define-syntax if*
  (syntax-rules (in)
    ((_ condition then else)
     (let ((value condition))
       (if (not (unspecified? value))
	   (if value
	       then
	       else))))
    ((_ condition then)
     (let ((value condition))
       (if (not (unspecified? value))
	   (if value then))))))

(define (<< . messages)
  (if #t
      (with-output-to-port (current-error-port) ;(current-error-port)
	(lambda ()
	  (for message in messages
	       (display message))
	  (newline)
	  (flush-all-ports)))))

(define* (in? obj list #:key (compare equal?))
  (any (lambda(x)(compare x obj)) list))

(define (properize src . dst)
  (cond ((pair? src)
	 (apply properize (cdr src) (cons (car src) dst)))
	((null? src)
	 (reverse dst))
	(else 
	 (reverse (cons src dst)))))

(define (array-map proc first . rest)
  (let ((dest (apply make-typed-array (array-type first) (if #f #f) 
		     (array-dimensions first))))
    (apply array-map! dest proc first rest)
    dest))

(define (array-append first . rest)
  (list->typed-array 
   (array-type first)
   (length (array-dimensions first))
   (apply append (map array->list (cons first rest)))))

(define (list->uniform-vector type list)
  (list->typed-array type 1 list))

(define (list->uniform-array numbers)
  (let ((shape (depth numbers))
	(type 
	 (let ((flat-list (flatten numbers)))
	   (cond ((not (every number? flat-list))
		  #t)
		 ((not (every real? flat-list))
		  'c32)
		 ((let ((upper (apply max flat-list))
			(lower (apply min flat-list)))
		    (cond ((not (every exact? flat-list))
			   'f32)
			  ((< lower 0)
			   (cond ((<= (- 1 (expt 2 7)) lower upper (expt 2 7))
				  's8)
				 ((<= (- 1 (expt 2 15)) lower upper (expt 2 15))
				  's16)
				 (else
				  's32)))
			  (else
			   (cond ((<= upper (expt 2 8))
				  'u8)
				 ((<= upper (expt 2 16))
				  'u16)
				 (else
				  'u32))))))))))
	(list->typed-array type shape numbers)))

(define* (module->hash-map #:optional (module (current-module)))
  (module-obarray module))

(define* (module->list #:optional (module (current-module)))
  (hash-map->list (lambda(key variable)
		    (cons key (variable-ref variable)))
		  (module->hash-map module)))

(define* (module-symbols #:optional (module (current-module)))
  (map car (module->list module)))

(define (hash-map->alist hash)
  (hash-map->list cons hash))

(define (alist->hash-map alist)
  (let ((h (make-hash-table)))
    (for-each (lambda(kv)(hash-set! h (car kv) (cdr kv))) alist)
    h))

;; (define (contains-duplicates? l)
;;   (call/cc (lambda(break)
;; 	     (let ((keys (make-hash-table)))
;; 	       (for-each (lambda(key)
;; 			   (if (hash-ref keys key)
;; 			       (break #t)
;; 			       (hash-set! keys key #t)))
;; 			 l))
;; 	     #f)))

(define (contains-duplicates? l)
  (let ((keys (make-hash-table)))
    (letrec ((next (lambda (l)
		     (cond ((null? l) #f)
			   ((hash-ref keys (car l)) #t)
			   (#t (hash-set! keys (car l) #t)
			       (next (cdr l)))))))
      (next l))))

(define-macro (\ f . args)
  (let* ((prefix "_")
	 (placeholder '_)
	 (ellipsis '...)
	 (rest (if (equal? (last args) ellipsis) ellipsis '()))
	 (args (if (symbol? rest) (drop-right args 1) args))
	 (max-arg 0)
	 (next-arg 
	  (lambda ()
	    (set! max-arg (+ max-arg 1))
	    (string->symbol (string-append 
			     prefix (number->string max-arg))))))
    (letrec ((process-arg
	      (lambda(arg)
		(cond ((eq? arg placeholder)
		       (next-arg))
		      ((and-let* 
			   (((symbol? arg))
			    (arg-string (symbol->string arg))
			    (match-struct (string-match 
					   (string-append 
					    "^" 
					    (regexp-quote prefix) 
					    "([0-9]+)$")
					     arg-string))
			    (number (string->number 
				     (match:substring 
				      match-struct 1))))
			 (if (> number max-arg) 
			     (set! max-arg number))
			 #t)
		       arg)
		      ((and (list? arg)
			    (not (null? arg))
			    (not (in? (first arg) '(\ quote))))
		       (map process-arg arg))
		      (else arg)))))
      (let ((args (map process-arg args)))
	`(lambda ,(append
		   (map (lambda (n) 
			  (string->symbol 
			   (string-append prefix (number->string n)))) 
			(iota max-arg 1))
		   rest)
	   ,(if (symbol? rest)
		`(apply ,f ,@args ,rest)
		`(,f ,@args)))))))

(define* (expand-form e #:key (opts '()))
  (let-values (((exp env) (decompile 
			   (compile e #:from 'scheme 
				    #:to 'tree-il 
				    #:env (current-module))
			   #:from 'tree-il 
			   #:to 'scheme 
			   #:opts opts)))
    exp))

(define-syntax-rule (expand expression)
  (expand-form 'expression))

(define (unix-environment)
  (let ((env (make-hash-table)))
	(for-each 
	 (lambda(s)
	   (match-let (((name . values)
			(string-split s #\=)))
	     (hash-set! env name (string-join values "="))))
	 (environ))
	env))

(define (cart . lists)
  (match lists
    (() '())
    ((only) (map list only))
    ((first . rest)
     (append-map (lambda(x)
		   (map (lambda(y) (cons y x))
			first))
		 (apply cart rest)))))

(define (all-tuples n l)
  (cond
   ((= n 0) '())
   ((= n 1) (map list l))
   ((> n 1)
    (match l
      (() '())
      ((first . rest)
       (append (map (\ cons first _) (all-tuples (1- n) rest))
	       (all-tuples n rest)))))))

(define (all-pairs l)
  (all-tuples 2 l))

(define (all-triples l)
  (all-tuples 3 l))

(define (?not pred)(lambda(x)(not (pred x))))

;; Note that (?and p? q? ...) is equivalent to 
;; (lambda(x)(and (p? x) (q? x)) ...)
;; (the same applies /mutatis mutandis/ to disjunction, i.e. ?or)
(define (?and . predicates)
  (lambda(x) (every (lambda(p)(p x)) predicates)))

(define (?or . predicates)
  (lambda(x) (any (lambda(p)(p x)) predicates)))

(define (map-n n fn l . lists) 
  (if (any (lambda(l)(< (length l) n)) (cons l lists))
      '()
      (cons (apply fn (append-map (lambda(l)(take l n)) (cons l lists)))
	    (apply map-n n fn (map (lambda(l)(drop l n)) (cons l lists))))))

(define (for-each-n n fn lst)
  (if (<= n (length lst))
      (begin
	(apply fn (take lst n))
	(for-each-n n fn (drop lst n)))))

(define (kw-list->hash-map kw-list)
  (let ((result (make-hash-table)))
    (for-each-n 2 (\ hash-set! result _ _) kw-list)
    result))

(define* (last-sexp-starting-position 
	  str #:key 
	  (opening-braces '(#\( #\[))
	  (closing-braces '(#\) #\]))
	  (braces (append opening-braces closing-braces)))

  (define* (rewind #:key string while starting-from)
    (let loop ((pos starting-from))
      (if (and (>= pos 0) (while (string-ref string pos)))
	  (loop (1- pos))
	  pos)))

  (define (last-symbol-starting-position str init-pos)
    (rewind #:string str #:starting-from init-pos 
	    #:while (lambda(c)
		      (and (not (char-whitespace? c)) 
			   (not (in? c braces))))))

  (define (last-whitespaces-starting-position str init-pos)
    (rewind #:string str #:starting-from init-pos 
	    #:while char-whitespace?))

  (define (last-string-starting-position str init-pos)
    (let loop ((pos init-pos))
      (if (and (eq? (string-ref str pos) #\")
	       (or (= pos 0) 
		   (not (eq? (string-ref str (1- pos)) #\\))))
	  pos
	  (loop (1- pos)))))
  ;; function definition
  (let eat ((pos (1- (string-length str)))
	    (level 0))
    (cond ((< pos 0)
	   0)
	  ((char-whitespace? (string-ref str pos)) ; eat whitespace
	   (eat (last-whitespaces-starting-position str pos) level))
	  ((eq? (string-ref str pos) #\") ; eat strings
	   (if (= level 0)
	       (last-string-starting-position str (1- pos))
	       (eat (1- (last-string-starting-position str (1- pos)))
		    level)))
	  ((not (in? (string-ref str pos) braces))
	   (if (= level 0)
	       (1+ (last-symbol-starting-position str pos))
	       (eat (last-symbol-starting-position str pos) level)))
	  ((in? (string-ref str pos) closing-braces)
	   (eat (1- pos) (1+ level)))
	  ((in? (string-ref str pos) opening-braces)
	   (cond ((= level 1)
		  (if (and (> pos 0)
			   (not (char-whitespace? 
				 (string-ref str (1- pos))))
			   (not (in? (string-ref str (1- pos)) 
				     braces)))
		      (let ((pos* (1+ (last-symbol-starting-position 
				      str (1- pos)))))
			(if (and (in? (string-ref str pos*) 
				      '(#\# #\' #\`))
				 (not (eq? (string-ref str (1+ pos*))
					   #\:)))
			    pos*
			    pos))
		      pos))
		 ((> level 1)
		  (eat (1- pos) (1- level)))
		 (#t
		  (throw 'mismatch-braces)))))))

(define (cart . args)
  (let ((n (length args)))
    (cond ((= n 0) '())
	  ((= n 1) (map list (car args)))
	  (#t (append-map (lambda(x)
			    (map (lambda(y)
				   (cons y x))
				 (car args)))
			  (apply cart (cdr args)))))))

(define (flatten l)
  (if (list? l)
    (append-map flatten l)
    (list l)))

(define (take-at-most lst i)
  (take lst (min i (length lst))))

(define (drop-at-most lst i)
  (drop lst (min i (length lst))))

(define remove-keyword-args
  (rec (self list)
       (match list
	 (((? keyword?) (? (?not keyword?)) . rest)
	  (self rest))
	 (((? (?not keyword?) x) . rest)
	  (cons x (self rest)))
	 (((? keyword?) . rest)
	  (self rest))
	 (() '()))))

(define* (random-array #:key (range 1.0)(type #t)(mean 0) #:rest dims)
  (let ((dims (remove-keyword-args dims)))
    (array-map (lambda (mean) (+ mean (- (random (* 2 range)) range)))
		(apply make-typed-array type mean dims))))

(define (array-size a)
  (apply * (map (match-lambda((lower upper) 
			      (1+ (abs (- upper lower)))))
		(array-shape a))))

(define (with-output-to-utf8 thunk)
  (string->utf8 (with-output-to-string thunk)))

(define (list-directory directory)
  (let ((dir (opendir directory)))
    (let loop ((listed-files '()) 
	       (file (readdir dir)))
      (cond ((eof-object? file)
	     (closedir dir)
	     listed-files)
	    (else 
	     (loop (cons file listed-files)
		   (readdir dir)))))))

(define (shell command)
  (let ((pipe (open-pipe command OPEN_READ)))
    (let loop ((lines '())
	       (line (read-line pipe)))
      (cond ((eof-object? line)
	     (close-pipe pipe)
	     (reverse lines))
	    (else
	     (loop (cons line lines)
		   (read-line pipe)))))))

;; a tremendous macro from Stchislav Dertch.
;; The idea is that if we represent an entity using a list,
;; say (property-a property-b property-c ...),
;; then instead of writing
;; (define (property-a entity) (car entity))
;; (define (property-b entity) (cadr entity))
;; (define (property-c entity) (caddr entity))
;; ...
;; one can simply write
;; (define-accessors (property-a property-b property-c ...))
;; The macro also works for trees, so it is ok to do things like
;; (define-accessors ((a b) ((c) d) e))
(define-macro (define-accessors tree)
  (letrec ((gather-leaves 
	    (lambda (arg subtree)
	      (cond ((null? subtree) '())
		    ((symbol? subtree) (list (cons subtree arg)))
		    ((pair? subtree) (append (gather-leaves `(car ,arg)
							    (car subtree))
					     (gather-leaves `(cdr ,arg)
							    (cdr subtree))))
		    (else (error "invalid accessor definition"))))))
    `(begin ,@(map (match-lambda((name . value)
				 `(define (,name s) ,value)))
		   (gather-leaves 's tree)))))

;; (expand '(define-accessors (a (b c 2))))

;; do dalszej rozkminki
;; (define (collatz n)
;;   (cond 
;;    ((= n 1) 1)
;;    ((= (modulo n 2) 0) (collatz (/ n 2)))
;;    (else (collatz (+ (* 3 n) 1)))))

;; (every (lambda(x)(= x 1))(map collatz (iota 1000 1)))

;; (define-syntax do
;;   (syntax-rules (where while)
;;     ((_ body ... (where bindings ...))
;;      (let (bindings ...) 
;;        body ...))
;;     ((_ body ... (while condition))
;;      (let loop ()
;;        body  ... 
;;        (if condition (loop))))))
