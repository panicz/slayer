(define-module (extra blob)
  #:use-module (extra common)
  #:use-module (system foreign)
  #:use-module (extra u8vectors))

(TODO support multidimensional arrays)

(publish
 (define (structure-definition? s)
   (match s
     (((name offset type) ...)
      (and (every structure-definition? type)
	   (every integer? offset)
	   (every symbol? name)))
     (('array type size ...)
      (and (structure-definition? type)
	   (every positive? size)
	   (> (length size) 0)))
     ((? symbol?)
      (assoc-ref c-types s))
     (else
      #f)))
 (define (structure-definition-size s)
   (assert (and (structure-definition? s)
		(natural? (structure-size s))))
   (match s
     (((name offset type) ...)
      (apply max `(,(+ offset (structure-definition-size type)) ...)))
     (('array type dimensions ...)
      (fold * (structure-definition-size type) dimensions))
     (else
      (sizeof (assoc-ref c-types s)))))
 where
 (define c-types 
   (export-types
    ;;int
    ;;unsigned-int
    ;;long
    ;;unsigned-long
    ;;size_t
    int8
    uint8
    int16
    uint16
    int32
    uint32
    int64
    uint64
    float 
    double)))

(publish 
 (define* ((as structure #:optional (endianness (native-endianness))) bv)
   (define (accessor . fields)
     (let-values (((offset field) (retrieve structure fields)))
       (match field
	 ((? symbol? type)
	  ((assoc-ref accessors type) bv offset endianness))
	 (else ;;(or ((names offsets fields) ...) ('array type dimensions ...))
	  ((as field) 
	   (make-shared-array 
	    bv (lambda(i)(list (+ i offset)))
	    `(0 ,(- (structure-definition-size field) 1))))))))
   (set-procedure-property! accessor 'type 'structure)
   (set-procedure-property! accessor 'storage bv)
   (set-procedure-property! accessor 'definition structure)
   accessor)
 where
 (define (retrieve structure-definition path)
   (match path
     (()
      (values 0 structure-definition))
     (((? integer? first) . rest)
      (match structure-definition
	(('array type (? (lambda(size)(< first size)) size))
	 (let-values (((child-offset child-type) (retrieve type rest)))
	   (values (+ child-offset (* (structure-definition-size type) first))
		   child-type)))))
     ((first . rest)
      (match (assoc-ref structure-definition first)
	((offset type)
	 (let-values (((child-offset child-type) (retrieve type rest)))
	   (values (+ offset child-offset) child-type)))))))
 (define-syntax-rule (type->getter+setter (type getter setter) ...)
   `((type 
      . ,(lambda (bytevector offset endianness)
	   (make-procedure-with-setter
	    (lambda ()
	      (getter bytevector offset endianness))
	    (lambda (value)
	      ;; zasygnalizuj waść zmianę
	      (setter bytevector offset value endianness)))))
     ...))
 (define accessors
   (type->getter+setter
    (int8 bytevector-s8-ref/shared bytevector-s8-set!/shared)
    (int16 bytevector-s16-ref/shared bytevector-s16-set!/shared)
    (int32 bytevector-s32-ref/shared bytevector-s32-set!/shared)
    (int64 bytevector-s64-ref/shared bytevector-s64-set!/shared)
    (uint8 bytevector-s8-ref/shared bytevector-s8-set!/shared)
    (uint16 bytevector-u16-ref/shared bytevector-u16-set!/shared)
    (uint32 bytevector-u32-ref/shared bytevector-u32-set!/shared)
    (uint64 bytevector-u64-ref/shared bytevector-u64-set!/shared)
    (float bytevector-ieee-single-ref/shared 
	   bytevector-ieee-single-set!/shared)
    (double bytevector-ieee-double-ref/shared 
	    bytevector-ieee-double-set!/shared))))

(define (new structure-definition . init-values)
  (let* ((storage (make-bytevector (structure-definition-size
				    structure-definition)))
	 (structure ((as structure-definition) storage)))
    structure))

(define (structure? s)
  (and (procedure? s)
       (eq? (procedure-property s 'type) 'structure)))

(define (structure-storage s)
  (assert (structure? s))
  (procedure-property s 'storage))

(define (structure-definition s)
  (assert (structure? s))
  (procedure-property s 'definition))

(define some-structure
  '((x 0 int32)
    (y 4 int32)
    (c 8 (array uint8 16)) ;; tablicę uint8 możemy traktować specjalnie
    (u 24 ((f 0 float)
	   (d 0 double)
	   (i 0 int32)))
    (type 32 int8)
    (size 40 ((w 0 int16)
	      (h 2 int16)))))

(define s (new some-structure))

(retrieve some-structure '(u d))

(set! (((s 'c) 0)) 20)

(set! ((s 'u 'd)) 50.0) 20.0)

(structure-storage s)
