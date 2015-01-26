(define-module (editor limbs)
  #:use-module (scum physics)
  #:use-module (editor relations)
  #:use-module (extra math)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:export (
	    tip?
	    link?
	    copula?
	    hub?
	    body-part-type
	    part-of-limb?
	    part-of-corpus?
	    ))

(define (tip? body-part)
  (= 1 (length (joints-attached-to body-part))))

(define (link? body-part)
  (= 2 (length (joints-attached-to body-part))))

(without-default (joint-property-getter)
  (define (copula? body-part)
    (and-let* (((first-joint second-joint) (joints-attached-to body-part)))
      (< (square (- ((specific joint-property-getter) first-joint 'anchor)
		    ((specific joint-property-getter) second-joint 'anchor)))
	 #[TOLERANCE]))))

(define (hub? body-part)
  (< 2 (length (joints-attached-to body-part))))

(define (body-part-type body-part)
  (cond ((tip? body-part)
	 'tip)
	((copula? body-part)
	 'copula)
	((link? body-part)
	 'link)
	((hub? body-part)
	 'hub)))

(define (part-of-limb? body-part #:except '())
  (or (tip? body-part)
      (and-let* (((left right) (bodies-attached-to body-part))
		 (exclusion `(,body-part ,@except)))
	(or (and (not (in? left except))
		 (part-of-limb? left #:except exclusion))
	    (and (not (in? right except))
		 (part-of-limb? right #:except exclusion))))))

(define (part-of-corpus? body-part)
  (not (part-of-limb? body-part)))
