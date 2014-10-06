(define-module (extra redis)
  #:use-module (extra common)
  #:use-module (extra ref)
  #:use-module (oop goops)
  #:use-module ((redis commands hashes) #:prefix redis:)
  #:use-module ((redis commands lists) #:prefix redis:)
  #:use-module (redis main)
  #:export (<redis-proxy> 
	    <redis-object-proxy> 
	    keys
	    erase!
	    list->redis!
	    redis->list
	    redis-list-retriever
	    redis-list-establisher
	    redis-list-getter
	    redis-hash-getter
	    redis-hash-setter
	    )
  )

;; slot-ref needs to be used to refer to slots of all classes that inherit
;; from <ref-interface>, because otherwise its internal getters will be
;; invoked

(define-class <redis-proxy> ()
  (redis-name #:init-keyword #:redis-name #:init-value #f)
  (redis #:init-keyword #:redis #:init-thunk redis-connect))

(define-class <redis-object-proxy> (<redis-proxy> <ref-interface>)
  (getter 
   #:allocation #:class
   #:init-value
   (lambda (self key)
     (let* ((connection (slot-ref self 'redis))
	    (name (->string (slot-ref self 'redis-name)))
	    (query (redis:hget name (->string key)))
	    (response (redis-send connection query))
	    (result (read-string response)))
       (match result
	 (#(key ...)
	  (make <redis-object-proxy> #:target self #:as key))
	 (else
	  result)))))
  (setter
   #:allocation #:class
   #:init-value
   (lambda (self key value)
     (let* ((name (slot-ref self 'redis-name))
	    (data (cond ((or (hash-table? value)
			     (instance? value))
			 (make <redis-object-proxy>
			   #:parent self
			   #:target value
			   #:as key)
			 `#(,@name ,key))
			((circular-list? value)
			 (unknot value))
			(else
			 value))))
       (redis-send (slot-ref self 'redis)
		   (redis:hset (->string name)
			       (->string key)
			       (->string data))))))
  (redis-name #:init-value '()))

(define-method (initialize (self <redis-object-proxy>) args)
  (next-method)
  (let-keywords args #t ((parent #f)
			 (target #f)
			 (as #f))
    (unless as (throw 'keyword-argument-required #:as))
    (slot-set! self 'redis-name
	       `(,@(slot-ref (or parent self) 'redis-name)
		 ,@(listify as)))
    (cond ((hash-table? target)
	   (for (key => value) in target
	     (set! #[self key] value)))
	  ((and (instance? target)
		(not (is-a? target <redis-proxy>)))
	   (for key in (slots target)
	     (set! #[self key] (slot-ref target key)))))))

(define-method (keys (proxy <redis-object-proxy>))
  (map read-string 
       (redis-send (slot-ref proxy 'redis)
		   (redis:hkeys (->string (slot-ref proxy 'redis-name))))))

(define-method (erase! (data <redis-object-proxy>))
  (for key in (keys data)
    (let ((datum #[data key]))
      (if (is-a? datum <redis-object-proxy>)
	  (erase! datum))
      (redis-send (slot-ref data 'redis)
		  (redis:hdel (->string (slot-ref data 'redis-name)) 
			      `(,(->string key)))))))

(define (list->redis! l redis-name connection)
  (match l
    (()
     redis-name)
    ((first . rest)
     (redis-send connection (redis:rpush redis-name (list (->string first))))
     (list->redis! rest redis-name connection))))

(define (redis->list redis-name connection)
  (map read-string (redis-send connection (redis:lrange redis-name 0 -1))))

(define ((redis-list-retriever redis-list-name) redis-proxy)
  (let ((name (->string `(,@(listify (slot-ref redis-proxy 'redis-name))
			  ,redis-list-name))))
    (reverse (redis->list name (slot-ref redis-proxy 'redis)))))

(define ((redis-list-establisher redis-list-name) redis-proxy value)
  (assert (list? value))
  (let ((name (->string `(,@(listify (slot-ref redis-proxy 'redis-name))
			  ,redis-list-name))))
    (list->redis! value name (slot-ref redis-proxy 'redis))))

(define ((redis-list-getter redis-list-name index) redis-proxy)
  (let ((name (->string `(,@(listify (slot-ref redis-proxy 'redis-name))
			  ,redis-list-name))))
    (read-string (redis-send (slot-ref redis-proxy 'redis)
			     (redis:lindex name (- -1 index))))))

(define ((redis-hash-getter redis-hash-name key) redis-proxy)
  (let ((name (->string `(,@(listify (slot-ref redis-proxy 'redis-name))
			  ,redis-hash-name))))
    (read-string (redis-send (slot-ref redis-proxy 'redis)
			     (redis:hget name (->string key))))))

(define ((redis-hash-setter redis-hash-name key) redis-proxy value)
  (let ((name (->string `(,@(listify (slot-ref redis-proxy 'redis-name))
			  ,redis-hash-name))))
    (redis-send (slot-ref redis-proxy 'redis)
		(redis:hset name (->string key) (->string value)))))
