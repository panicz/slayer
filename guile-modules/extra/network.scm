(define-module (extra network)
  #:export-syntax (protocol))

(define-syntax protocol
  (syntax-rules ()
    ((_ (clause ...) ...)
     (protocol env (clause ...) ...))
    ((_ environment (clause ...) ...)
     (let ((environment (make-hash-table)))
       (build-protocol environment ((clause ...) ...))))
    ))

(define-syntax build-protocol
  (syntax-rules (export)
    ((_ table ())
     table)
    ((_ table ((export name) . rest))
     (begin
       (hash-set! table 'name name)
       (build-protocol table rest)))
    ((_ table ((define (name . args) body ...) . rest))
     (let ()
       (define (name . args) body ...)
       (hash-set! table 'name name)
       (let ()
	 (build-protocol table rest))))
    ((_ table ((define name value) . rest))
     (begin
       (hash-set! table 'name value)
       (build-protocol table rest)))))
