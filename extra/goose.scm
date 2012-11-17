(define-module (extra goose)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-2)
  :use-module (srfi srfi-11)
  :use-module (oop goops)
  :use-module (ice-9 match)
  :use-module (ice-9 optargs)
  ;:use-module (extra slayer)
  :use-module (extra ref)
  :use-module (extra common)
  :use-module (extra network)
  :use-module ((rnrs) :version (6))
  :export (
	   objects-visible-to
	   <goose>
	   state-of
	   ))

(define-class <goose> ()
  (owners #:init-value #f #:init-keyword #:owners)
  (context #:init-value #f) ; the subspace to which it belongs
  (private-slots #:init-value '()) ; slots visible only to the owners
  (client-slots #:init-value '())
  (id #:init-thunk (\ gensym "g-")))

(define (state-of object)
  (map (lambda (slot) (list slot #[object slot]))
       (lset-difference equal?
			(map first (class-slots (class-of object)))
			(map first (class-slots <goose>))
			#[object 'client-slots])))

(define-method (objects-visible-to object)
  (list object))

