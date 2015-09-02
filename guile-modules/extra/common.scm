(define-module (extra common)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-18)
  #:use-module (srfi srfi-31)
  #:use-module (srfi srfi-60)
  #:use-module (ice-9 nice-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 expect)
  #:use-module (system base compile)
  #:use-module (system vm frame)
  #:use-module ((rnrs) :version (6) 
		:select (make-bytevector 
			 utf8->string string->utf8 
			 bytevector-fill!
			 vector-map vector-for-each
			 native-endianness endianness
			 )
		)
  #:use-module ((rnrs bytevectors) :version (6))

  ;;  #:use-module ((rnrs) #:version (6))
  #:re-export (;; nice-9
	       match and-let* publish define lambda let let* let-syntax define-syntax
	       ;; srfi-1
	       iota circular-list every any
	       proper-list? dotted-list? null-list? not-pair? circular-list?
	       first second third fourth fifth sixth seventh eighth ninth tenth
	       car+cdr take drop take-right drop-right split-at last
	       concatenate zip unzip1 unzip2 unzip3 unzip4 unzip5
	       fold fold-right pair-fold reduce reduce-right unfold-right
	       append-map filter-map partition remove remove! find find-tail
	       take-while drop-while span break list-index
	       delete delete! delete-duplicates delete-duplicates!
	       lset<= lset= lset-adjoin lset-union lset-intersection
	       lset-difference lset-xor lset-diff+intersection
	       ;; srfi-11
	       let-values let*-values
	       ;; srfi-60
	       bitwise-and bitwise-ior bitwise-xor bitwise-not 
	       integer->list list->integer arithmetic-shift
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
	       vector-map vector-for-each
	       make-bytevector 
	       utf8->string string->utf8 
	       bytevector-fill!
	       native-endianness endianness
	       ;; ice-9 format, ice-9 pretty-print, ice-9 rdelim
	       pretty-print format read-line read-delimited
	       )
  #:export (
	    unknot listify stringify every. any.
	    expand-form ?not ?and ?or in?
	    hash-keys hash-values hash-copy hash-size merge-hashes!
	    make-applicable-hash-table
	    union intersection difference adjoin unique same-set?
	    equivalent-set? equiv?
	    map-n for-each-n unfold-n unzip chunk-list
	    equivalence-classes min+max argmin argmax argmin+argmax clamp
	    atom? symbol< natural?
	    rest element head tail length+last-tail
	    tree-find tree-map map* depth copy scan map/values order
	    array-map array-map/typed array-append array-copy array-pointer
	    keyword-args->hash-map keyword-args->alist alist->keyword-args
	    list->uniform-vector list->uniform-array
	    contains-duplicates?
	    module->hash-map module->list module-symbols
	    symbol->list hash-map->alist 
	    replace-alist-bindings
	    alist->hash-map assoc? assoc->hash assoc->hash/deep
	    last-sexp-starting-position
	    proper-list flatten last-tail
	    cart cart-pow all-tuples all-pairs all-triples combinations 
	    permutations insertions
	    take-at-most drop-at-most rotate-left rotate-right sublist
	    remove-keyword-args keyword-ref
	    delete-first alter pick skip
	    array-size
	    random-array
	    read-string write-string display-string ->string ->string*
	    string-remove-prefix string-remove-suffix
	    substitute-pattern
	    fill-template
	    with-output-to-utf8
	    u8->list u8->bitvector bytevector->list bytevector->bitvector
	    unpack pack extend-right extend-left
	    current-working-directory list-directory change-directory
	    with-changed-working-directory read-file read-s-expressions
	    shell next-available-file-name
	    << die first-available-input-port
	    real->integer
	    make-locked-mutex
	    last-index indexed
	    demand SPECIFIC-CONTEXT
	    now
	    iterations
	    RUN-TESTS TEST-RESULTS
	    WARN
	    arity impose-arity
	    )
  #:export-syntax (TODO \ for for-every exists matches? equals? prototype
		   safely export-types e.g. observation: match* assert
		   reassurance: deprecated:
		   upto once changed?
		   values->list list<-values
		   define-curried-syntax define-accessors
		   define-template define-values
		   with-literal within-module
		   with-output-port with-output-file with-output-string
		   with-input-port with-input-file with-input-string
		   with-error-port with-error-file 
		   with-working-directory
		   publish-with-fluids
		   define-fluid with-default without-default specify
		   supply applicable-hash applicable-hash-with-default
		   hash-table
		   string-match-all string-matches
		   rec expand letrec-macros
		   transform! increase! decrease! multiply!
		   push! pop!
		   symbol-match
		   n-lambda trace
		   !# check <<< let** measured
		   )
  #:replace (compose 
	     (unfold-facade . unfold)
	     quasiquote
	     (procedure-property-with-setter . procedure-property))
  )

(define-syntax define-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ definienda ... definientia)
       (every identifier? #'(definienda ...))
       (with-syntax (((temporaria ...) 
		      (generate-temporaries #'(definienda ...))))
	 #'(begin 
	     (define definienda #f)
	     ...
	     (call-with-values (lambda () definientia)
	       (lambda (temporaria ...)
		 (set! definienda temporaria)
		 ...))))))))

;; every and any that work for dotted lists:
(define (every. pred l)
  (match l
    ((h . t)
     (and (pred h) (every. pred t)))
    (()
     #t)
    (else
     (pred l))))

(define (any. pred l)
  (match l
    ((h . t)
     (or (pred h) (any. pred t)))
    (()
     #f)
    (else
     (pred l))))

(define-syntax restructured
  (lambda (stx)
    (define (...? x)
      (eq? x '...))
    (define (restructure-form-variables form)
      (define (extract-form-variables form)
	(match form
	  (('quote x)               '())
	  (()                       '())
	  (((? symbol? s) (? ...?)) (list s))
	  (((? symbol? s) . tail)   (append-map extract-form-variables tail))
	  ((head . tail)
	   (append (extract-form-variables head)
		   (append-map extract-form-variables tail)))
	  (else                     (list form))))
      (define (form-variables form)
	(match form
	  (('unquote x)             (extract-form-variables x))
	  (('quote _)               '())
	  ((head . tail)            (append (form-variables head) 
					    (form-variables tail)))
	  (else                     '())))
      (delete-duplicates (filter symbol? (form-variables form)) eq?))
    (define (drop-ellipses call) 
      (match call
	((f (? ...?))             (drop-ellipses f))
	((head . tail)            (cons (drop-ellipses head) 
					(map drop-ellipses tail)))
	(else                     call)))
    (define (restructure form)
      (match form
	(('unquote x)             (drop-ellipses x))
	((head . tail)            (list 'cons (restructure head) 
					(restructure tail)))
	(x                        (list 'quote x))))
    (syntax-case stx ()
      ((_ form)
       (with-syntax (((args ...) 
		      (datum->syntax 
		       #'form (restructure-form-variables (syntax->datum #'form))))
		     (body (datum->syntax 
			    #'form (restructure (syntax->datum #'form)))))
	 #'(map (lambda (args ...) body) args ...))))))

(define-syntax quasiquote 
  (lambda (stx)
    (syntax-case stx (unquote unquote-splicing quasiquote) 
      ((_ (unquote form)) 
       #'form)
      ((_ (form ellipsis . rest))
       (eq? (syntax->datum #'ellipsis) '...)
       #'(append (restructured form) (quasiquote rest)))
      ((_ ((unquote-splicing form) . rest))
       #'(append form (quasiquote rest)))
      ((_ (quasiquote form) . depth) 
       #'(list 'quasiquote (quasiquote form #f . depth))) 
      ((_ (unquote form)  x . depth) 
       #'(list 'unquote (quasiquote form . depth))) 
      ((_ (unquote-splicing form) x . depth) 
       #'(list 'unquote-splicing (quasiquote form . depth))) 
      ((_ (car . cdr) . depth) 
       #'(cons (quasiquote car . depth) (quasiquote cdr . depth))) 
      ((_ #(elt ...) . depth) 
       #'(list->vector (quasiquote (elt ...) . depth))) 
      ((_ atom . depth) 
       #''atom))))

(set-port-encoding! (current-input-port) "UTF-8")
(set-port-encoding! (current-output-port) "UTF-8")
(set-port-encoding! (current-error-port) "UTF-8")
(fluid-set! %default-port-encoding "UTF-8")

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

#;(define-syntax mlambda
  (lambda (stx)
    (define (keyword-args* args body)
      (define (keyword-args args normal keyword)
	(match args
	  (()
	   `(,normal ,keyword))
	  (((? keyword? k) (? symbol? s) . rest)
	   (keyword-args rest normal `((,s #f ,k) ,@keyword)))
	  (((? keyword? k) ((and (? symbol?) 
				 (not (or 'quote 'quasiquote)) s) val) . rest)
	   (keyword-args rest normal `((,s ,val ,k) ,@keyword)))
	  (((? keyword? k) val . rest)
	   (keyword-args rest normal `((,(keyword->symbol k) ,val ,k) 
				       ,@keyword)))
	  (((? (?not keyword?) x) . rest)
	   (keyword-args rest `(,@normal ,x) keyword))))
      
      (datum->syntax stx `(,(syntax->datum body)
			   ,@(keyword-args (syntax->datum args) '() '()))))
    (syntax-case stx ()
      ((_ (first-arg ... last-arg . rest-args) body ...)
       (and (every identifier? #'(first-arg ... last-arg))
	    (or (identifier? #'rest-args) (null? #'rest-args)))
       #'(lambda (first-arg ... last-arg . rest-args) body ...))
      ((_ arg body ...)
       (or (identifier? #'arg) (null? #'arg))
       #'(lambda arg body ...))
      ((_ (args ...) body ...)
       (any keyword? (syntax->datum #'(args ...)))
       (with-syntax ((((body ...) (args ...) (key ...))
		      (keyword-args* #'(args ...) #'(body ...))))
	 #'(lambda* (args ... #:key key ...)
	     body ...)))
      ((_ args body ...)
       #'(match-lambda* (args body ...))))))

#;(define-syntax cdefine
  (syntax-rules ()
    ((_ ((head . tail) . args) body ...)
     (cdefine (head . tail)
       (mlambda args body ...)))
    ((_ (name . args) body ...)
     (define name (mlambda args body ...)))
    ((_ . rest)
     (define . rest))))

#;(define-syntax cdefine*
  (syntax-rules ()
    ((_ ((head . tail) . rest) body body* ...)
     (cdefine* (head . tail)
       (lambda* rest body body* ...)))
    ((_ (head . rest) body body* ...)
     (define* head
       (lambda* rest body body* ...)))
    ((_ . rest)
     (define* . rest))))

(define-syntax-rule (values->list call)
  (call-with-values (lambda () call) list))

(define-syntax-rule (list<-values call)
  (values->list call))

(define-syntax define-fluid
  (syntax-rules ()
    ((_ (interface . args) body ...)
     (define-fluid interface (lambda args body ...)))
    ((_ name value)
     (define name (make-fluid value)))))


(define-syntax within-module
  (syntax-rules ()
    ((_ module action ...)
     (begin
       (eval 'action module)
       ...))))

(define-syntax rec
  (syntax-rules ()
    ((rec (NAME . VARIABLES) . BODY)
     (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
    ((rec NAME EXPRESSION)
     (letrec ( (NAME EXPRESSION) ) NAME))))

(define-syntax-rule (TODO something ...) (rec (f . x) f))

(define-syntax-rule (assert condition ...) (if #f #f))

;; (define-syntax prototype
;;   (syntax-rules (-> :)
;;     ((_ (name . arg-types) -> result-type)
;;      (TODO))
;;     ((_ (name : arg-types ... -> result-type))
;;      (TODO))))

;; ;;(prototype (give! : <any> x <queue> -> <unknown>))
;; (prototype (give! item #;to (medium <queue>)))
;; ;; natomiast odbiorca chciałby albo sprawdzić, czy dane
;; ;; w kolejce są dostępne,
;; (prototype (item-available? (medium <queue>)) -> <boolean>)
;; ;; albo po prostu na owe dane czekać:
;; (prototype (get! #;from (qmedium <queue>)) -> <any>)
(define-fluid RUN-TESTS #f)

(define-fluid TEST-RESULTS '())

(define-syntax-rule (define-type name . properties)
  (TODO))

(define-syntax-rule (prototype . args) 
  (TODO))

(define-syntax e.g. 
  (lambda (stx)
    (syntax-case stx (===>)
      ((_ expression ===> values ...)
       #'(e.g. expression 
	       (lambda(a b)
		 (if (array? a)
		     (array-equal? a b)
		     (equal? a b)))
	       'values ...))
      ((_ expression comparison values ...)
       (with-syntax (((labels ...) (generate-temporaries #'(values ...))))
	 #'(if (fluid-ref RUN-TESTS)
	       (let-values (((labels ...) expression))
		 (fluid-set! TEST-RESULTS
			     (cons `(,(if (and (comparison labels values) ...)
					  'pass: 
					  'fail:)
				     expression
				     ,labels ...
				     comparison
				     values ...
				     ,(current-source-location))
				   (fluid-ref TEST-RESULTS)))))))
      ((_ expression)
       #'(e.g. (not expression) eq? #f)))))

(define-syntax-rule (observation: . args) (TODO))

(define-syntax-rule (reassurance: . args) (TODO))

(define-syntax-rule (suggest: . args) (if #f #f))

(define-syntax-rule (claim: . args) (TODO))

(define-syntax deprecated:
  (syntax-rules (--deprecated)
    ((_ definitions ... --deprecated)
     (begin
       definitions ...))))


(define (demand to-do-something-with . args)
  (call/cc (lambda (go-on)
	     (apply throw 'demand go-on to-do-something-with args))))

(define-syntax supply
  (syntax-rules ()
    ((_ (((<to-do-something-with> . <args>) <do-what> ...) ...) . <actions>)
     (let ((handlers (make-hash-table))
	   (unsupported (lambda details
			  (apply throw 'unsatisfied-demand
				 details))))
       (hash-set! handlers (quote <to-do-something-with>)
		  (lambda <args> <do-what> ...))
       ...
       (catch 'demand
	 (lambda () . <actions>)
	 ;; this is very strange: after replacing primitive-lambda
	 ;; with mlambda from (ice-9 nice-9) module, the expander
	 ;; returns an error
	 (primitive-lambda (key go-on demand . the-args)
	   (go-on (apply (hash-ref handlers demand unsupported) the-args))))))))

(e.g.
 (let ((people '()))
   (supply (((free person)
	     (set! people (cons person people))))
     (let ((the-person 'Nelson-Mandela))
       (demand 'free the-person)))
   people)
 ===> (Nelson-Mandela))

(define-fluid SPECIFIC-CONTEXT (make-hash-table))

(let-syntax ((specific-literal-syntax
	      (syntax-rules ()
		((_ binding-structure name value)
		 (lambda (stx)
		   (assert (appears? #'name #;in #'binding-structure))
		   (syntax-case stx ()
		     ((_ (binding-structure (... ...)) actions . *)
		      (with-syntax ((specific (datum->syntax stx 'specific)))
			#'(let-syntax 
			      ((specific
				(syntax-rules (name (... ...))
				  ((_ name)
				   (let ((default (hash-ref
						   (fluid-ref
						    SPECIFIC-CONTEXT)
						   'name '())))
				     (if (null? default)
					 value
					 (first default))))
				  (... ...))))
			    actions . *)))))))))
  (define-syntax with-default 
    (specific-literal-syntax (name value) name value))
  (define-syntax without-default 
    (specific-literal-syntax name name (throw 'unspecified 
					      #:name 'name
					      #:context:
					      (hash-map->alist 
					       (fluid-ref SPECIFIC-CONTEXT))
					      ))))

(define-syntax specify
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((name expression) ...)
	  actions ...)
       (with-syntax (((value ...) (generate-temporaries #'(expression ...))))
	 #'(let ((value expression) ...)
	     (dynamic-wind
	       (lambda ()
		 (hash-set! (fluid-ref SPECIFIC-CONTEXT) 'name
			    (cons value (hash-ref (fluid-ref SPECIFIC-CONTEXT) 
						  'name '())))
		 ...)
	       (lambda ()
		 actions ...)
	       (lambda ()
		 (hash-set! (fluid-ref SPECIFIC-CONTEXT) 'name
			    (rest (hash-ref (fluid-ref SPECIFIC-CONTEXT) 'name)))
		 ...))))))))

(e.g.                                   ; this is how the trio 'with-default',
 (let ()                                ; 'specific' and 'specify' can be used
   (with-default ((x 10)
		  (y 20))
     (define (f)
       `(,(specific x) ,(specific y))))
   (specify ((x 30))
     (f)))
 ===> (30 20))

(e.g.
 (catch 'unspecified
   (lambda ()
     (without-default (x y)
       (define (f)
	 `(,(specific x) ,(specific y))))
     (specify ((x 30))
       (f)))
   list) ===> (unspecified y))

(define-fluid LIMITED-ACTIONS-RECORD (make-hash-table))

(define-syntax-rule (upto n #;times perform-action . *)
  (let* ((here (current-source-location))
	 (times-performed (or (hash-ref (fluid-ref LIMITED-ACTIONS-RECORD) here)
			      0)))
    (if (< times-performed n)
	(begin 
	  (hash-set! (fluid-ref LIMITED-ACTIONS-RECORD) here 
		     (1+ times-performed))
	  perform-action . *))))

(define-syntax-rule (once perform-action . *)
  (upto 1 #;time perform-action . *))

(define-fluid VALUE-RECORD (make-hash-table))

(define-syntax-rule (changed? value)
  (let* ((here (current-source-location))
	 (actual-value value)
	 (previous-value (match (hash-get-handle (fluid-ref VALUE-RECORD) here)
			   ((key . previous-value)
			    previous-value)
			   (else
			    (not actual-value)))))
    (if (equal? actual-value previous-value)
	#f
	(begin
	  (hash-set! (fluid-ref VALUE-RECORD) here actual-value)
	  #t))))

;; `define-curried-syntax' is not a curried definition!
;; It defines a new macro which generates an appropreate
;; procedure, if insufficient number of arguments is given.
;; For example,
;;
;; (define-curried-syntax (f a b c d) (list a b c d))
;; 
;; would expand to
;;
;; (define-syntax f 
;;   (syntax-rules () 
;;     ((f a b c d) 
;;      (begin (list a b c d))) 
;;     ((f a b c) 
;;      (lambda(d) 
;;        (f a b c d))) 
;;     ((f a b) 
;;      (lambda(c) 
;;        (f a b c))) 
;;     ((f a) 
;;      (lambda(b) 
;;        (f a b))) 
;;     ((f) 
;;      (lambda(a) 
;;        (f a)))))
;;
;; A more realistic example is given below, in the `matches?' macro
;; definition. 

(define-syntax define-curried-syntax/helper
  (syntax-rules ()
    ((_ name () ((pattern template) ...))
     (define-syntax name
       (syntax-rules ()
	 (pattern template) ...)))

    ((_ name (args ... last) ((pattern template) ...))
     (define-curried-syntax/helper name (args ...)
       ((pattern template) ... ((name args ...) 
				(lambda (last)
				  (name args ... last))))))
    ))

(define-syntax-rule (define-curried-syntax (name args ...) body . *)
  (define-curried-syntax/helper name (args ...) (((name args ...)
						  (begin body . *)))))

(define-curried-syntax (string-match-all pattern string)
  (let loop ((n 0)
	     (all '()))
    (let ((m (string-match pattern string n)))
      (if m
	  (loop (match:end m) (cons m all))
	  (reverse all)))))

;; If the `matches?' macro is called with two arguments, it behaves
;; as a regular binary predicate, which returns true if the second
;; argument matches the first (in terms of Wright/Shinn pattern matcher,
;; a.k.a. (ice-9 match) module). If the last argument is omitted,
;; it returns a procedure which checks whether its argument matches
;; a given pattern.

(define-curried-syntax (matches? pattern x)
  (match x 
    (pattern #t)
    (else #f)))

;; match* is like match, but if there's no matching pattern, it
;; doesn't raise an error
(define-syntax match*
  (syntax-rules ()
    ((_ structure (pattern action . *) ...)
     (match structure
       (pattern action . *)
       ...
       (else (if #f #f))))))

(define-curried-syntax (equals? value x)
  (equal? value x))

(define (substitute-pattern pattern replacement string)
  "substitute all occurrences of PATTERN in STRING with REPLACEMENT"
  (let ((replacement-length (string-length replacement)))
    (let substitute ((matches (string-match-all pattern string))
		     (string string)
		     (offset 0))
      (match matches
	(()
	 string)
	((ms . rest)
	 (let* ((start (match:start ms))
		(end (match:end ms))
		(match-length (- end start)))
	   (substitute rest 
		       (string-replace string replacement
				       (+ start offset)
				       (+ end offset))
		       (+ offset (- replacement-length match-length)))))))))

(define-curried-syntax (string-matches pattern string)
  (and-let* ((matches (string-match-all pattern string))
	     ((not (null? matches)))
	     (result (append-map 
		      (lambda (ms)
			(let ((count (match:count ms)))
			  (filter-map (lambda (n) (match:substring ms n))
				      (if (= count 1) '(0) (iota (1- count) 1)))))
		      matches))
	     ((not (null? result))))
    result))

(define-curried-syntax (symbol-match pattern symbol)
  (assert (and (symbol? symbol) (string? pattern)))
  (and-let* ((matches (string-matches pattern (symbol->string symbol))))
    (map string->symbol matches)))

;; string-matches returns a list of all expressions that match a given
;; pattern (as a raw text, contrary to the match struct from "string-match"
;; and "string-match-all")

(e.g.
 (string-matches "[0-9]" "1a 2b 3c 4d")
 ===> ("1" "2" "3" "4"))

;; if parenthesized expressions appear, only those are caught as
;; expressions (the whole match is excluded from results)

(e.g.
 (string-matches "([0-9])([a-z])" "1a 2b 3c 4d")
 ===> ("1" "a" "2" "b" "3" "c" "4" "d"))

;; borrowed from http://community.schemewiki.org/?scheme-faq-language
 (define (curry f n) 
   (if (zero? n) 
       (f) 
       (lambda args 
         (curry (lambda rest 
                  (apply f (append args rest))) 
                (- n (length args))))))

;; tutaj trzeba dopieścić, bo curry zwraca funkcję od dowolnie
;; wielu argumentów, a my chcielibyśmy case-lambdę
(define-syntax curried-lambda
  (syntax-rules ()
    ((_ (args ...) body body* ...)
     (letrec ((f (lambda (args ...) body body* ...)))
       (curry f (length '(args ...)))))))


(define-syntax publish-with-fluids          ; publish-with-fluids is like
  (lambda (stx)                             ; with-fluids, but it allows
    (define (interfaces+names+bodies specs) ; internal definitions only
      (define (interface-name interface)
	(match interface
	  ((head . tail)
	   (interface-name head))
	  ((? symbol? name)
	   name)))
      (map (lambda (spec)
	     (syntax-case spec ()
	       ((interface . body)
		(datum->syntax stx `(,(syntax->datum #'interface)
				     ,(interface-name (syntax->datum 
						       #'interface))
		   ,(syntax->datum #'body))))))
	   specs))
    (syntax-case stx ()
      ((_ fluids (define-variant . spec) ...)
       (with-syntax ((((interface name body) ...)
		      (interfaces+names+bodies #'(spec ...))))
	 #'(begin
	     (define name (and (defined? 'name) name)) 
	     ...
	     (with-fluids fluids
	       (set! name (let () (define-variant interface . body) name))
	       ...)))))))

;; `letrec-macros' behaves similar to `let-syntax', but it is restricted
;; to take `macro' keyword in place of the latter's `syntax-rules' (or its
;; equivalents), because unlike syntax-transformers, macros in guile are
;; no longer first-class objects. this is completely pointless.
(define-syntax letrec-macros
  (syntax-rules (macro)
    ((_ ((name (macro args definition ...)) ...) body ...)
     (let ()
       (define-macro (name . args) definition ...)
       ...
       body ...))))

(define (min+max first . args)
  (assert (and (number? first)
	       (every number? args)))
  (let loop ((min first)
	     (max first)
	     (remaining args))
    (match remaining
      (()
       (values min max))
      ((current . remaining)
       (cond ((< current min)
	      (loop current max remaining))
	     ((> current max)
	      (loop min current remaining))
	     (else
	      (loop min max remaining)))))))

(e.g.
 (min+max 5 4 6 3 7 2 8 1)
 ===> 1 8)

(publish
 (define (argmin property element . elements)
   (apply argopt < property element elements))
 (define (argmax property element . elements)
   (apply argopt > property element elements))
 where
 (define (argopt < property element . elements)
   (let next-trial ((champion element)
		    (mastery (property element))
		    (opponents elements))
     (if (null? opponents)
	 champion
	 (let* ((rival (first opponents))
		(challenge (property rival)))
	   (if (< challenge mastery)
	       (next-trial rival challenge (rest opponents))
	       (next-trial champion mastery (rest opponents))))))))

(e.g.
 (argmin length '(1 2) '(3) '(4 5 6))
 ===> (3))

(define (argmin+argmax property element . elements)
  (let ((quality (property element)))
    (let next-trial ((winner element)
		     (looser element)
		     (mastery quality)
		     (failure quality)
		     (opponents elements))
      (if (null? opponents)
	  (values looser winner)
	  (let* ((rival (first opponents))
		 (quality (property rival)))
	    (cond ((< quality failure)
		   (next-trial winner rival mastery quality (rest opponents)))
		  ((> quality mastery)
		   (next-trial rival looser quality failure (rest opponents)))
		  (else
		   (next-trial winner looser mastery failure 
			       (rest opponents)))))))))

(e.g.
 (argmin+argmax length '(1 2) '(3) '(4 5 6))
 ===> (3) (4 5 6))

(define (clamp min max)
  (lambda (x)
    (assert (and (number? x) (< min max)))
    (cond ((< x min)
	   min)
	  ((> x max)
	   max)
	  (else
	   x))))

(e.g.
 (map (clamp 2 5) '(1 2 3 4 5 6))
 ===> (2 2 3 4 5 5))

(define (unknot circular-list)
  (let next ((rewrite '()) (pending circular-list) (seen `(,circular-list ())))
    (match pending
      ((unknot first . rest)
       (if (memq rest seen)
	   (reverse (cons first rewrite))
	   (next (cons first rewrite) rest (cons rest seen)))))))

(e.g.
 (unknot (cons 0 (circular-list 1 2 3)))
 ===> (0 1 2 3))

(define (length+last-tail l)
  "length- can receive a dotted list"
  (define (inner-length l n)
    (match l
      ((h . t)
       (inner-length t (+ 1 n)))
      (else
       (values n l))))
  (inner-length l 0))

(define (listify x)
  (if (list? x)
      x
      (list x)))

(define (stringify x)
  (if (string? x)
      x
      (->string x)))

(define (make-locked-mutex)
  (let ((m (make-mutex)))
    (lock-mutex m)
    m))

(define* (die #:optional (message #f))
  (if message
      (let ((stderr (current-error-port)))
	(display message stderr)
	(newline stderr)))
  (exit -1))

(define (first-available-input-port . ports)
  (let again ((descriptors (select ports '() '())))
    (match descriptors
      ((read _ _)
       (if (null? read)
	   (again (select ports '() '()))
       #;else
	   (list-ref read (random (length read))))))))

(define (real->integer number)
  (let ((lower (floor number)))
    (if (exact? lower)
	lower
	(inexact->exact lower))))

(define (write-string object)
  (with-output-to-string (lambda()(write object))))

(define (display-string object)
  (with-output-to-string (lambda()(display object))))

(define ->string write-string)

(define ->string* display-string)

(define (read-string string)
  (with-input-from-string string read))

(define (string-remove-suffix suffix string)
  (let ((regex (string-append (regexp-quote suffix) "$")))
    (regexp-substitute #f (string-match regex string) 'pre)))

(define (string-remove-prefix prefix string)
  (let ((regex (string-append "^" (regexp-quote prefix))))
    (regexp-substitute #f (string-match regex string) 'post)))

(define (unique lst)
  (let ((result (make-hash-table)))
    (for-each (lambda(item)(hash-set! result item #t)) lst)
    (hash-map->list (lambda(k v)k) result)))

(e.g.
 (unique '(a b a c a d b a))
 same-set? '(a b c d))

(define (symbol< a b)
  (string< (symbol->string a) (symbol->string b)))

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(define (natural? n)
  (and (integer? n)
       (>= n 0)))

(define rest cdr)

(define element car)

(assert (let ((l (a list?))) 
	  (if (not (null? l)) 
	      (in? (element l) l))))

(define head (make-procedure-with-setter car set-car!))

(define tail (make-procedure-with-setter cdr set-cdr!))

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

(define (equivalent-set? equiv? lset . lsets)
  (apply lset= equiv? lset lsets))

(define (same-set? lset . lsets)
  (apply equivalent-set? equal? lset lsets))

(define (depth x)
  (if (and (list? x) (not (null? x)))
      (1+ (apply max (map depth x)))
      0))

(define (tree-map proc tree)
  (map (lambda (item)
	 (if (pair? item)
	     (tree-map proc item)
	     (proc item)))
       tree))

(define (tree-find predicate? tree)
  `(,@(if (predicate? tree)
	  `(,tree)
	  '())
    ,@(match tree
	((head . tail)
	 `(,@(tree-find predicate? head) ,@(tree-find predicate? tail)))
	(_
	 '()))))

(define* (order #;of element #;in list #:key (identified-using eq?))
  (list-index (lambda (x) (identified-using element x)) list))

(with-default ((first car)
	       (rest cdr)
	       (empty? null?))
  (define (map* proc . ls)
    (if (any (specific empty?) ls)
	'()
	`(,(apply proc (map (specific first) ls))
	  ,@(apply map* proc (map (specific rest) ls))))))

(define (map/values f . ls)
  (assert (pair? ls))
  (let loop ((ls ls)
	     (result '()))
    (if (null? (car ls))
	(if (null? result)
	    '()
	    (unzip (length (car result)) (reverse result)))
	(loop (map cdr ls)
	      `(,(call-with-values (lambda () (apply f (map car ls))) list)
		,@result)))))

(e.g.
 (map/values values '(1 2 3) '(a b c))
 ===> (1 2 3) (a b c))

(define (scan op e l)
  (match l
    (()
     '())
    ((h . t)
     (let ((e* (op e h)))
       `(,e* . ,(scan op e* t))))))

(e.g.
 (scan * 1 '(1 2 3 4 5 6)) ===> (1 2 6 24 120 720))

;; equivalence classes with partial order preserved
(define (equivalence-classes equivalent? set)
  (let next-item ((set set)(result '()))
    (match set
      (()
       (reverse (map reverse result)))
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

(e.g.
 (equivalence-classes (lambda(x y)(= (modulo x 3) (modulo y 3))) (iota 9))
 ===> ((0 3 6) (1 4 7) (2 5 8)))

(observation:
 (for-all <procedure>
     (if (and (allows-arity? <procedure> 2)
	      (equivalence? <procedure>))
	 (for-all <list> x y
	   (if (and (in? x <list>) (in? y <list>)
		    (<procedure> x y)
		    (< (list-index (equals? x) <list>)
		       (list-index (equals? y) <list>)))
	       (let* ((classes (equivalence-classes <procedure> <list>))
		      (the-class (find (lambda(class)(in? x class)) classes)))
		 (and (list? the-class)
		      (< (list-index (equals? x) the-class)
			 (list-index (equals? y) the-class)))))))))

(define-syntax-rule (safely sexp)
  (catch #t (lambda () (values sexp #t))
    (lambda (key . args)
      (with-output-to-port (current-output-port)
	(lambda ()
	  ;;(backtrace)
	  (display `(error calling sexp : ,key ,args))
	  (values (if #f #f) #f))))
    (lambda args (backtrace))))

(define-syntax-rule (export-types symbol ...)
  `((symbol . ,symbol) ...))

(define-syntax-rule (transform! fx x args ...)
  (set! x (fx x args ...)))

(define-syntax increase!
  (syntax-rules ()
    ((_ variable value)
     (transform! + variable value))
    ((_ variable)
     (increase! variable 1))))

(define-syntax decrease!
  (syntax-rules ()
    ((_ variable value)
     (transform! - variable value))
    ((_ variable)
     (decrease! variable 1))))

(define-syntax-rule (multiply! variable value)
  (transform! * variable value))

(define-syntax-rule (push! l e) 
  (set! l (cons e l)))

(define-syntax-rule (pop! l)
  (let ((result (car l)))
    (set! l (cdr l))
    result))

(define (last-index array)
  (1- (vector-length array)))

(define (indexed list)
  (zip (iota (length list)) list))

(e.g. (indexed '(a b c)) ===> ((0 a) (1 b) (2 c)))

(define-syntax for
  (syntax-rules (in .. => indexed)
    ((_ x in (indexed list-of-values) body ...)
     (let loop ((n 0) 
		(l list-of-values))
       (match l
	 (()
	  (if #f #f))
	 ((first . rest)
	  (match (list n first)
	    (x body ...))
	  (loop (1+ n) rest)))))
    ((_ x in first .. last body ...)
     ;; this form should be made obsolete in favour of the next one
     (let ((final last))
       (let loop ((x first))
	 (if (<= x final)
	     (begin
	       body ...
	       (loop (1+ x)))))))
    ((_ x in (first .. last) body ...)
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

(define-syntax for-every 
  (syntax-rules (in ..)
    ((_ var in (first .. last) predicate)
     (for-every var in (iota (- last first -1) first)
       predicate))
    ((_ var in set predicate)
     (every (match-lambda (var predicate) (_ #f)) set))))

(e.g.
 (for-every (x y) in '((1 6) (2 5) (3 4))
   (= (+ x y) 7)))

(define-syntax exists 
  (syntax-rules (in)
    ((_ var in set predicate)
     (any (match-lambda (var predicate) (_ #f)) set))))

(define (equiv? . args)
  "logical equivalence"
  (or (every (lambda(x)x) args)
      (every not args)))

(define-syntax-rule (hash-table (key value) ...)
  (let ((new-hash-table (make-hash-table)))
    (hash-set! new-hash-table key value)
    ...
    new-hash-table))

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

(define (merge-hashes! dest . sources)
  (for source in sources
       (for (key => value) in source
	    (hash-set! dest key value))))

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

(e.g.
 (let ((table (applicable-hash ('a 5) ('b 10))))
   (table 'b 15)
   (+ (table 'a) (table 'b))) ===> 20)

(define (->strings object)
  (cond ((list? object)
	 (map ->strings object))
	((hash-table? object)
	 `(#:< ,@(map ->strings (hash-map->list list object)) #:>))
	((pair? object)
	 (cons (->strings (car object)) (->strings (cdr object))))
	(else
	 object)))

(define (<< . messages)
  (with-output-to-port (current-output-port) ;(current-error-port)
    (lambda ()
      (for message in messages
	(cond
	 ((hash-table? message)
	  (display (->strings message)))
	 (else
	  (display message)
	  (display " "))))
      (newline)
      (flush-all-ports))))

(define* (in? obj list #:key (compare equal?))
  (any (lambda(x)(compare x obj)) list))

(define (proper-list src . dst)
  (cond ((pair? src)
	 (apply proper-list (cdr src) (cons (car src) dst)))
	((null? src)
	 (reverse dst))
	(else 
	 (reverse (cons src dst)))))

(define (last-tail l)
  (match l
    ((h . t)
     (last-tail t))
    (else
     l)))

(define (array-map/typed type proc arg1 . args)
  (let ((result (apply make-typed-array type (if #f #f) 
		       (array-dimensions arg1))))
    (apply array-map! result proc arg1 args)
    result))

(define (array-map proc arg1 . args)
  (apply array-map/typed (array-type arg1) proc arg1 args))

(define (array-append first . rest)
  (list->typed-array 
   (array-type first)
   (length (array-dimensions first))
   (apply append (map array->list (cons first rest)))))

(define (array-copy array)
  (let* ((shape (array-shape array))
	 (copy (apply make-typed-array (array-type array) (if #f #f) shape)))
    (array-copy! array copy)
    copy))

(e.g.
 (let* ((array #f32(1 2 3))
	(copy (array-copy array)))
   (and (array-equal? array copy)
	(not (eq? array copy)))))

(define (copy object)
  (cond ((list? object)
	 (map copy object))
	((array? object)
	 (array-copy object))
	((string? object)
	 (string-copy object))
	;;((instance? object)
	;; (deep-clone object))
	(else object)))

(define (array-pointer array . address)
  (let ((shape (array-shape array)))
    (assert (every (lambda (index (lower upper))
		     (<= lower index upper))
		   address shape))
    (cond ((= (length address) (length shape))
	   (make-procedure-with-setter 
	    (lambda () (apply array-ref array address))
	    (lambda (value) (apply array-set! array value address))))
	  ((< (length address) (length shape))
	   (let ((view (apply make-shared-array array 
			      (lambda args (apply list `(,@address ,@args)))
			      (drop shape (length address)))))
	     (make-procedure-with-setter
	      (lambda () view)
	      (lambda (value) (array-copy! value view)))))
	  (else
	   (error "Invalid array address")))))

(e.g.
 (let* ((A #2f32((0 1 2)
		 (3 4 5)))
	(*A/1/1 (array-pointer A 1 1))
	(initial-A/1/1 (*A/1/1)))
   (set! (*A/1/1) 8)
   (values initial-A/1/1 (*A/1/1) A))
 ===> 4.0 8.0 #2f32((0 1 2)
		    (3 8 5)))

(e.g.
 (let* ((A #2f32((0 1 2)
		 (3 4 5)))
	(*A/1 (array-pointer A 1))
	(initial-A/1 (array-copy (*A/1))))
   (set! (*A/1) #(2 1 0))
   (values initial-A/1 (*A/1) A))
 ===> #f32(3 4 5) #f32(2 1 0) #2f32((0 1 2)
				    (2 1 0)))

(define (list->uniform-vector type list)
  (list->typed-array type 1 list))

(define (list->uniform-array numbers)
  (let ((shape (depth numbers))
	(type 
	 (let ((flat-list (flatten numbers)))
	   (cond ((or (null? flat-list)
		      (not (every number? flat-list)))
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

(define (assoc? l)
  (matches? ((key . value) ...) l))

(define (assoc->hash assoc)
  (let ((hash (make-hash-table)))
     (for (key . value) in assoc
	  (hash-set! hash key value))
     hash))

(define (assoc->hash/deep l)
  (cond ((assoc? l)
	 (assoc->hash (map (match-lambda ((key . value)
					  `(,key . ,(assoc->hash/deep value))))
			   l)))
	((list? l)
	 (map assoc->hash/deep l))
	(else
	 l)))

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

(define (cart . lists)
  (match lists
    (() '())
    ((only) (map list only))
    ((first . rest)
     (append-map (lambda(x)
		   (map (lambda(y) (cons y x))
			first))
		 (apply cart rest)))))

(define (cart-pow set n)
  (apply cart (make-list n set)))

(define (all-tuples n l)
  (suggest: (rename #;to subsets))
  (suggest: swap-arguments)
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
  (suggest: remove)
  (all-tuples 2 l))

(e.g.
 (all-pairs '(a b c))
 ===> '((a b) (a c) (b c)))

(define (all-triples l)
  (suggest: remove)
  (all-tuples 3 l))

(e.g.
 (all-triples '(a b c d))
 ===> '((a b c) (a b d) (a c d) (b c d)))

(define (combinations #;from-set A #;of-length n)
  (suggest: (rename #;to multicombinations))
  (assert (not (contains-duplicates? A)))
  (cond ((= n 0)
	 '())
	((= n 1)
	 (map list A))
	(else
	 (append-map (lambda(combination)
		       (map (lambda(a)
			      `(,a . ,combination)) A))
		     (combinations #;from-set A #;of-length (- n 1))))))

(e.g.
 (combinations #;from-set '(a b) #;of-length 3)
 same-set?
 '((a a a) (b a a) (a b a) (b b a) (a a b) (b a b) (a b b) (b b b)))

(define (permutations l)
  (match l
    (()
     '(()))
    ((head . tail)
     (append-map (lambda (sub)
		   (insertions head sub))
		 (permutations tail)))))

(define (insertions x l)
  (match l
    (()
     `((,x)))
    ((head . tail)
     `((,x ,head . ,tail) . ,(map (lambda (y)
				    `(,head . ,y))
				  (insertions x tail))))))

(define (interlaces . sequences)
  (define (interlaces2 a b)
    (assert (type interlaces2 (a ...) (a ...) -> ((a ...) ...)))
    (match `(,a ,b)
      ((a ())
       `(,a))
      ((() b)
       `(,b))
      (((a* . a+) (b* . b+))
       `(,@(map (lambda (A)
		  `(,a* . ,A))
		(interlaces2 a+ b))
	 ,@(map (lambda (B)
		  `(,b* . ,B))
		(interlaces2 a b+))))))
  ;; in order to generalize interlaces2 using reduce, it needs to be lifted
  ;; to internal binary operation
  (define (interlaces2* A B)
    (assert (type interlaces2* ((a ...) ...) ((a ...) ...) -> ((a ...) ...)))
    (append-map (lambda (ab) (apply interlaces2 ab)) (cart A B)))
  (assert (type interlaces (a ...) ... -> ((a ...) ...)))
  (let ((lifted (map list sequences)))
    (reduce interlaces2* '() lifted)))

(e.g. 
 (interlaces '(a b) '(1 2))
 same-set? 
 '((a b 1 2) (a 1 b 2) (1 a b 2) (1 a 2 b) (1 2 a b) (a 1 2 b)))

(assert
 (let ((sequences (a list?)))
   (if (and (every list? sequences)
	    (not (contains-duplicates? sequences))
	    (not (any contains-duplicates? sequences)))
       (not (contains-duplicates? (apply interlaces sequences))))))

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

(define* (sublist sequence #:optional 
	      #;from (first 0) #;to (last (- (length sequence) 1)))
  (if (< last first)
      (reverse (sublist sequence #;from last #;to first))
      (take (drop sequence first) (- last first -1))))

(define (for-each-n n fn lst)
  (if (<= n (length lst))
      (begin
	(apply fn (take lst n))
	(for-each-n n fn (drop lst n)))))

(define (unzip n l)
  (apply values (take (apply map list l) n)))

(define (chunk-list l . chunk-sizes)
  (assert (and (list? l)
	       (every natual? chunk-sizes)
	       (<= (fold + 0 chunk-sizes) (length l))))
  (let loop ((result '())
	     (sizes chunk-sizes)
	     (rest l))
    (match sizes
      (()
       (reverse result))
      ((size . sizes)
       (let-values (((this rest) (split-at rest size)))
	 (loop `(,this ,@result) sizes rest))))))

(e.g.
 (chunk-list '(1 2 3 4 5 6) 1 2 3) ===> ((1) (2 3) (4 5 6)))

(define* (unfold-facade stop? transform generate seed 
			#:optional (tail-gen (lambda(x)'())))
  (unfold stop? transform generate seed tail-gen))

(define (unfold-n n next seed)
  (let loop ((count (- n 1))
	     (result `(,seed)))
    (if (<= count 0)
	(reverse result)
	(loop (- count 1) `(,(next (first result))
			    ,@result)))))

(e.g.
 (unfold-n 10 1+ 1)
 ===> (1 2 3 4 5 6 7 8 9 10))

(define* (extend-right l #;to size #;with #:optional (fill #f))
  (let ((extension-size (- size (length l))))
    (if (< extension-size 0)
	(error "list length exceeds the desired length")
	`(,@l ,@(make-list extension-size fill)))))

(e.g. (extend-right '(1 2 3) 5 0) ===> (1 2 3 0 0))

(define* (extend-left l #;to size #;with #:optional (fill #f))
  (let ((extension-size (- size (length l))))
    (if (< extension-size 0)
	(error "list length exceeds the desired length")
	`(,@(make-list extension-size fill) ,@l))))

(e.g. (extend-left '(1 2 3) 5 0) ===> (0 0 1 2 3))

(define (keyword-args->hash-map kw-list)
  (let ((result (make-hash-table)))
    (for-each-n 2 (\ hash-set! result _ _) kw-list)
    result))

(define (u8->list u8)
  (integer->list u8 8))

(define (u8->bitvector u8)
  (list->bitvector (u8->list u8)))

(define (bytevector->list bv)
  (append-map u8->list (bytevector->u8-list bv)))

(define (bytevector->bitvector bv)
  (list->bitvector (bytevector->list bv)))

(define (unpack bv . chunk-sizes)
  (apply values 
	 (map list->integer 
	      (apply chunk-list (bytevector->list bv) chunk-sizes))))

(define (integer->list/warning value size)
  (let ((min-size (integer-length value)))
    (if (< size min-size)
	(warn "truncating" value "to fit" size "bits (instead of"min-size")"))
    (integer->list value size)))

(define (pack . values+sizes)
  (match (map-n 2 list values+sizes)
    (((value size) ...)
     (let* ((bits (append-map integer->list/warning value size))
	    (size (* 8 (ceiling-quotient (length bits) 8))))
       (u8-list->bytevector
	(map list->integer 
	     (map-n 8 list (extend-right bits #;to size #;with #f))))))))

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

(define (flatten l)
  (if (list? l)
    (append-map flatten l)
    (list l)))

(e.g.
 (flatten '((a) () (b ()) () (c)))
 ===> (a b c))

(define (take-at-most n from-list)
  (take from-list (min n (length from-list))))

(define (drop-at-most n from-list)
  (drop from-list (min n (length list))))

(define (rotate-left lst n)
  (let-values (((left right) (split-at lst n)))
    `(,@right ,@left)))

(e.g.
 (rotate-left '(1 2 3 4 5 6) 2)
 ===> (3 4 5 6 1 2))

(define (rotate-right lst n)
  (let-values (((left right) (split-at lst (- (length lst) n))))
    `(,@right ,@left)))

(e.g.
 (rotate-right '(1 2 3 4 5 6) 2)
 ===> (5 6 1 2 3 4))

(define (keyword-args->alist kwlist)
  (match kwlist
    (()
     '())
    (((? keyword? kw) val . rest)
     `((,(keyword->symbol kw) . ,val) . ,(keyword-args->alist rest)))))

(e.g.
 (keyword-args->alist '(#:a 1 #:b 2 #:c 3))
 same-set?
 '((a . 1)(b . 2)(c . 3)))

(define (alist->keyword-args alist)
  (append-map (lambda (key.value)
		(match key.value
		  ((key . value)
		   `(,(symbol->keyword key) ,value))))
	      alist))

(e.g.
 (map-n 2 list (alist->keyword-args '((a . 1)(b . 2)(c . 3))))
 same-set?
 (map-n 2 list '(#:a 1 #:b 2 #:c 3)))

(define (replace-alist-bindings bindings #;with other)
  (map (lambda ((key . value))
	 (match (assoc key other)
	   ((same-key . other-value)
	    `(,same-key . ,other-value))
	   (else
	    `(,key . ,value))))
       bindings))

(e.g.
 (replace-alist-bindings '((a . 1)(b . 2)(c . 3))
			 #;with '((a . 4)(c . 0)(d . 7)))
 same-set?
 '((a . 4)(b . 2)(c . 0)))

(define (keyword-ref l keyword)
  (match l
    (((? keyword? key) value . rest)
     (if (eq? key keyword)
	 value
	 (keyword-ref rest keyword)))
    ((first . rest)
     (keyword-ref rest keyword))
    (()
     #f)))

(e.g. (keyword-ref '(#:a 1 #:b 2 #:c 3) #:b) ===> 2)

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

(define* (delete-first element #;from lst #:optional (= equal?))
  (match lst
    (() '())
    ((first . rest)
     (if (= first element)
	 rest
	 `(,first . ,(delete-first element rest =))))))

(define (skip #;element-number n #;in list)
  (let (((head . tail) list))
    (if (= n 0)
	tail
	`(,head . ,(skip #;element-number (- n 1) #;in tail)))))

(e.g. (skip #;element-number 1 #;in '(a b c)) ===> (a c))

(define (alter #;element-number n #;in list #;with replacement)
  (let (((head . tail) list))
    (if (= n 0)
	`(,replacement . ,tail)
	`(,head . ,(alter #;element-number (- n 1) 
					   #;in tail #;with replacement)))))

(e.g.
 (alter #;element-number 1 #;in '(ząb dupa zębowa) #;with 'zupa)
 ===> (ząb zupa zębowa))

(define (pick n)
  (lambda args
    (list-ref args n)))

(e.g. ((pick 1) 'a 'b 'c) ===> b)

(define* (random-array #:key (range 1.0)(type #t)(mean 0) #:rest dims)
  (let ((dims (remove-keyword-args dims)))
    (array-map (lambda (mean) (+ mean (- (random (* 2 range)) range)))
		(apply make-typed-array type mean dims))))

(define (array-size a)
  (fold * 1.0 (map (match-lambda((lower upper) 
				 (1+ (abs (- upper lower)))))
		   (array-shape a))))

(define (with-output-to-utf8 thunk)
  (string->utf8 (with-output-to-string thunk)))

(letrec-syntax ((define-port-redirect-syntax
		  (lambda (x)
		    (define (join-symbols-with delimiter . symbols)
		      (string->symbol 
		       (string-join (map symbol->string symbols) 
				    (symbol->string delimiter))))
		    (define (with . args)
		      (datum->syntax x (apply 
					join-symbols-with '-
					`(with ,@(map syntax->datum args)))))
		    (syntax-case x ()
		      ((_ type (i/o preposition))
		       (with-syntax ((with-i/o-type (with #'i/o #'type))
				     (with-i/o-preposition-type
				      (with #'i/o #'preposition #'type)))
			 #'(define-syntax-rule (with-i/o-type type action . *)
			     (with-i/o-preposition-type
			      type (lambda () action . *))))))))
		(define-port-redirect-syntaxes-for-type
		  (syntax-rules ()
		    ((_ type ((i/o preposition) ...))
		     (begin
		       (define-port-redirect-syntax type (i/o preposition))
		       ...))))
		(define-port-redirect-syntaxes
		  (syntax-rules ()
		    ((_ (type ...) ((direction preposition) ...))
		     (begin
		       (define-port-redirect-syntaxes-for-type type
			 ((direction preposition) ...))
		       ...)))))
  (define-port-redirect-syntaxes (port file)
    ((output to) (input from) (error to)))
  ;; expands to
  ;; (define-syntax-rule (with-output-port port action . *)
  ;;   (with-output-to-port port (lambda () action . *)))
  ;; ...
  (define-port-redirect-syntax string (input from)))

(define-syntax-rule (with-output-string action . *)
  (with-output-to-string (lambda () action . *)))

(define* (read-s-expressions #:optional (port (current-input-port)))
  (let loop ((content '()))
    (let ((datum (read port)))
      (if (eof-object? datum)
	  (reverse content)
	  (loop (cons datum content))))))

(define (read-file filename)
  (read-s-expressions (open-input-file filename)))

(define current-working-directory getcwd)

(define change-directory chdir)

(define (with-changed-working-directory dir thunk)
  (let ((cwd (current-working-directory)))
    (dynamic-wind (lambda () (change-directory dir))
		  thunk
		  (lambda () (change-directory cwd)))))

(define-syntax-rule (with-working-directory dir action . *)
  (with-changed-working-directory dir (lambda () action . *)))

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

(define (next-available-file-name base)
  (let loop ((counter 0))
    (let ((name (string-append base (format #f ".~3,'0x" counter))))
      (if (file-exists? name)
	  (loop (1+ counter))
	  name))))

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

(define (unix-environment)
  (let ((env (make-hash-table)))
	(for-each 
	 (lambda(s)
	   (let (((name . values)
		  (string-split s #\=)))
	     (hash-set! env name (string-join values "="))))
	    (environ))
	env))

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
;;
;; Furthermore, the accessors also contain setter
;; procedures, so one can (set! (property-c entity) value)
(define-macro (define-accessors tree)
  (define (gather-leaves arg subtree)
    (cond ((null? subtree) '())
	  ((symbol? subtree) (list (cons subtree arg)))
	  ((pair? subtree) (append (gather-leaves `(head ,arg)
						  (head subtree))
				   (gather-leaves `(tail ,arg)
						  (tail subtree))))
	  (else (error "invalid accessor definition"))))
  `(begin ,@(map (match-lambda((name . body)
			       `(define ,name
				  (make-procedure-with-setter
				   (lambda (s)
				     ,body)
				   (lambda (s v)
				     (set! ,body v))))))
		 (gather-leaves 's tree))))

(define (fill-template . args)
  (match args
    ((parameters ... template)
     (let fill-next ((template template)
		     (parameters parameters))
       (match parameters
	 (()
	  template)
	 ((keyword value . rest)
	  (fill-next (substitute-pattern (string-append 
					  "<" (symbol->string
					       (keyword->symbol keyword)) ">")
					 (if (string? value) 
					     value
					     (->string value))
					 template)
		     rest)))))))

(define-macro (define-template interface template)
  (match interface
    ((name args ...)
     (let ((kw-list (append-map (lambda (arg)
				  `(,(symbol->keyword arg)
				    (list 'quote ,arg)))
				args)))
       `(define-macro ,interface
	  `(display (fill-template ,,@kw-list ,,template)))))))

(define procedure-property-with-setter
  (make-procedure-with-setter procedure-property set-procedure-property!))

(define (arity procedure)
  (assert (procedure? procedure))
  (or (procedure-property procedure 'imposed-arity)
      (procedure-property procedure 'arity)))

(define-syntax-rule (n-lambda n args body + ...)
  (let ((the-procedure (lambda args (assert (= (length args) n)) body + ...)))
    (set-procedure-property! the-procedure 'imposed-arity `(,n 0 #f))
    the-procedure))

(define (impose-arity n procedure)
  (set-procedure-property! procedure 'imposed-arity n)
  procedure)

(define (compose . fns)
  (define (make-chain fn chains)
    (lambda args
      (call-with-values 
	  (lambda () (apply fn args)) 
	chains)))
  (impose-arity (if (null? fns) 
		    '(0 0 #t)
		    (arity (last fns)))
		(reduce make-chain values fns)))

(e.g.
 ((compose 1+ 1+ 1+) 0)
 ===> 3)

(define (iterations n f)
  (apply compose (make-list n f)))

(e.g.
 ((iterations 3 1+) 0)
 ===> 3)

(define (now)
  (get-internal-real-time))

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

(define-syntax-rule (measured expression)
  (let ((starting-time (now)))
    (let ((result (list<-values expression)))
      (format #t "~s: ~s ns\n" 'expression (- (now) starting-time))
      (apply values result))))

(define-syntax-rule (let** ((names ... expression) ...)
			  body ...)
  (let*-replacement ((names ... (measured expression)) ...)
    body ...))

;; the "!#" macro is used for debugging, and its name is intentionally
;; obscure, as it should not appear in the production code
(define-syntax !# 
  (syntax-rules ()
    ((_ datum)
     `(datum ',datum))
    ((_ data ...)
     `((data ',data) ...))))

(define-syntax-rule (<<< messages ...)
  (<< (!# messages ...)))

(define (common-prefix a b)
  (let loop ((a a) (b b)     
	     (result '()))
    (match `(,a ,b)
      (((a* . a+) (b* . b+))
       (if (eq? a* b*)
	   (loop a+ b+ (cons a* result))
	   (reverse result)))
      (else
       (reverse result)))))

(define-syntax-rule (WARN messages ...)
  (let* ((location (current-source-location))
	 (file (assoc-ref location 'file))
	 (line (assoc-ref location 'line)))
    (display "WARNING ")
    (when file
      (display file)
      (display "/"))
    (when line
      (display line))
    (display ": ")
    (display messages) ... (newline)))

(define-syntax-rule (trace)
  (let* ((stack (make-stack #t))
	 (restricted '(eval for-each make-stack dynamic-wind catch-closure
			    catch slot-ref)))
    (filter-map (lambda (n)
		  (and-let* ((frame (stack-ref stack n))
			     (proc (frame-procedure frame))
			     (name (procedure-name proc))
			     ((not (in? name restricted))))
		    name))
		(iota (stack-length stack)))))

(define-syntax check
  (syntax-rules ()
    ((_ condition expression)
     (let ((value expression))
       (unless (condition expression)
	 (let* ((trace (trace))
		(location (current-source-location))
		(file (assoc-ref location 'file))
		(line (assoc-ref location 'line)))
	   (format #t "Assertion ~a failed at ~a:~a with ~a ~a\n" 
		   '(condition expression)
		   file line value trace)))))
    ((_ assertion)
     (check values assertion))))
