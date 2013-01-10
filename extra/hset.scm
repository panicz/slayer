(define-module (extra hset)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:export (hset 
	    list->hset hset->list
	    hset<= hset=
	    hset-union 
	    hset-intersection
	    hset-difference
	    hash->hset
	    hash=
	    hash-empty?
	    ))

(define (list->hset l)
  (let ((result #[]))
    (for x in l
	 (set! #[result x] #t))
    result))

(define (hset . elements)
  (list->hset elements))

(define (hset->list hset)
  (filter-map (match-lambda ((key . value)
			     (and value key))) 
	      (hash-map->list cons hset)))

(define (hset<= . args)
  (match args
    (() #t)
    ((singleton) #t)
    ((a b . rest)
     (and (every #[b _] (hset->list a))
	  (apply hset<= b rest)))))

(define (hset= . args)
  (and (apply hset<= args)
       (apply hset<= (reverse args))))

(define (hset-union . hsets)
  (let ((result #[]))
    (for set in hsets
	 (for (key . value) in (hash-map->list cons set)
	      (if value
		  (set! #[result key] #t))))
    result))

(define (hset-intersection hset . hsets)
  (let ((result #[]))
    (for x in (hset->list hset)
	 (if (every #[_ x] hsets)
	     (set! #[result x] #t)))
    result))

(define (hset-difference hset . hsets)
  (let ((result (hash-copy hset)))
    (for hset in hsets
	 (for x in (hset->list hset)
	      (hash-remove! result x)))
    result))

(define (hash->hset hash)
  (let ((hset #[]))
    (for pair in (hash-map->list cons hash)
	 (set! #[hset pair] #t))
    hset))

(define (hash= . hashes)
  (apply hset= (map hash->hset hashes)))

(define (hash-empty? hash)
  (hash= hash #[]))
