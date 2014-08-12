(define-module (extra u8vectors)
  #:use-module (extra common)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-60)
  #:export (
	    u8vector-uint-ref
	    u8vector-uint-set!
	    u8vector-sint-ref
	    u8vector-sint-set!

	    u8vector-u8-ref
	    u8vector-u8-set!
	    u8vector-u16-ref
	    u8vector-u16-set!
	    u8vector-u32-ref
	    u8vector-u32-set!
	    u8vector-u64-ref
	    u8vector-u64-set!

	    u8vector-s8-ref
	    u8vector-s8-set!
	    u8vector-s16-ref
	    u8vector-s16-set!
	    u8vector-s32-ref
	    u8vector-s32-set!
	    u8vector-s64-ref
	    u8vector-s64-set!

	    u8vector-ieee-single-ref
	    u8vector-ieee-single-set!
	    u8vector-ieee-double-ref
	    u8vector-ieee-double-set!
	    ))

;; this module is to be thrown away once the bytevector-*-ref family
;; of procedures get fixed in the core of guile to work with shared
;; arrays, and later, when arrays themselves get to be thrown away,
;; replaced by a list of lists (once scheme* is plead to life)

(define (proper-u8-vector-boundaries? n v offset)
  (and (u8vector? v)
       (positive? n) (integer? n)
       (natural? offset)
       (< offset (- (u8vector-length v) n))))

(publish 
 (define (u8vector-uint-ref v index endianness size)
   (case endianness
     ((little)
      (u8vector-read-unsigned-bytes/little-endian size v index))
     ((big)
      (u8vector-read-unsigned-bytes/big-endian size v index))
     (else
      (error "invalid endianness value" endianness))))
 (define (u8vector-uint-set! v index value endianness size)
   (case endianness
     ((little)
      (u8vector-write-unsigned-bytes/little-endian size v index value))
     ((big)
      (u8vector-write-unsigned-bytes/big-endian size v index value))
     (else
      (error "invalid endianness value" endianness))))
 where
 (define (u8vector-read-unsigned-bytes/little-endian n v offset)
   (assert (proper-u8-vector-boundaries? n v offset))
   (fold + 0 (map (lambda(i)
		    (* (u8vector-ref v (+ i offset)) (expt 256 i)))
		  (iota n))))
 (define (u8vector-write-unsigned-bytes/little-endian n v offset value)
   (assert (and (proper-u8-vector-boundaries? n v offset)
		(integer? value)
		(>= n (integer-length value))))
   (let loop ((i 0)
	      (bits value))
     (when (< i n)
       (u8vector-set! v (+ i offset) (bitwise-and bits #xff))
       (loop (+ i 1) (arithmetic-shift bits -8)))))
 (define (u8vector-read-unsigned-bytes/big-endian n v offset)
   (assert (proper-u8-vector-boundaries? n v offset))
   (fold + 0 (map (lambda(i)
		    (* (u8vector-ref v (+ i offset)) (expt 256 (- n i 1))))
		  (iota n))))
 (define (u8vector-write-unsigned-bytes/big-endian n v offset value)
   (assert (and (proper-u8-vector-boundaries? n v offset)
		(integer? value)
		(>= n (integer-length value))))
   (let loop ((i (- n 1))
	      (bits value))
     (when (>= i 0)
       (u8vector-set! v (+ i offset) (bitwise-and bits #xff))
       (loop (- i 1) (arithmetic-shift bits -8))))))

(publish
 (define (u8vector-sint-ref v index endianness size)
   (case endianness
     ((little)
      (u8vector-read-signed-bytes/little-endian size v index))
     ((big)
      (u8vector-read-signed-bytes/big-endian size v index))
     (else
      (error "invalid endianness value" endianness)))) 
 (define (u8vector-sint-set! v index value endianness size)
   (case endianness
     ((little)
      (u8vector-write-signed-bytes/little-endian size v index value))
     ((big)
      (u8vector-write-signed-bytes/big-endian size v index value))
     (else
      (error "invalid endianness value" endianness))))
 where
 (define (u8vector-read-signed-bytes/little-endian n v offset)
   (assert (proper-u8-vector-boundaries? n v offset))
   (let*-values (((n-1) (- n 1))
		 ((v/n-1) (u8vector-ref v (+ offset n-1)))
		 ((sign highest-value)
		  (values (bitwise-and v/n-1 #b10000000)
			  (bitwise-and v/n-1 #b01111111))))
     (fold + (* (- highest-value sign) (expt 256 n-1))
	   (map (lambda(i)
		  (* (u8vector-ref v (+ i offset)) (expt 256 i)))
		(iota n-1)))))
 (define (u8vector-write-signed-bytes/little-endian n v offset value)
   (let ((minus (if (negative? value) 1 0)))
     (assert (and (proper-u8-vector-boundaries? n v offset)
		  (integer? value)
		  (>= n (+ minus (integer-length value)))))
     (let loop ((i 0)
		(bits (if (positive? value)
			  value
			  (+ value (* 1/2 (expt 256 n))))))
       (when (< i n)
	 (if (= i (- n 1))
	     (u8vector-set! v (+ i offset)
			    (bitwise-ior (arithmetic-shift minus 7)
					 (bitwise-and bits #b01111111)))
	     (u8vector-set! v (+ i offset) (bitwise-and bits #xff)))
	 (loop (+ i 1) (arithmetic-shift bits -8))))))
 (define (u8vector-read-signed-bytes/big-endian n v offset)
   (assert (proper-u8-vector-boundaries? n v offset))
   (let*-values (((n-1) (- n 1))
		 ((v/0) (u8vector-ref v offset))
		 ((sign highest-value)
		  (values (bitwise-and v/0 #b10000000)
			  (bitwise-and v/0 #b01111111))))
     (fold + (* (- highest-value sign) (expt 256 n-1))
	   (map (lambda(i)
		  (* (u8vector-ref v (+ i offset 1)) (expt 256 (- n-1 i 1))))
		(iota n-1)))))
 (define (u8vector-write-signed-bytes/big-endian n v offset value)
   (let ((minus (if (negative? value) 1 0)))
     (assert (and (proper-u8-vector-boundaries? n v offset)
		  (integer? value)
		  (>= n (+ minus (integer-length value)))))
     (let loop ((i (- n 1))
		(bits (if (positive? value)
			  value
			  (+ value (* 1/2 (expt 256 n))))))
       (when (>= i 0)
	 (if (= i 0)
	     (u8vector-set! v (+ i offset)
			    (bitwise-ior (arithmetic-shift minus 7)
					 (bitwise-and bits #b01111111)))
	     (u8vector-set! v (+ i offset) (bitwise-and bits #xff)))
	 (loop (- i 1) (arithmetic-shift bits -8)))))))

#|

(reassurance:
 (match-let (((bv index value endianness size) 
	      (legal-arguments u8vector-uint-set!)))
   (and  (= (u8vector-uint-ref bv index endianness size)
	    (bytevector-uint-ref bv index endianness size))
	 (= (u8vector-sint-ref bv index endianness size)
	    (bytevector-sint-ref bv index endianness size)))))

(reassurance:
 (match-let (((bv index value endianness size) 
	      (legal-arguments u8vector-uint-set!)))
   (u8vector-uint-set! bv index value endianness size)
   (= (u8vector-uint-ref bv index endianness size)
      (bytevector-uint-ref by index endianness size)
      value)))

(reassurance:
 (match-let (((bv index value endianness size) 
	       (legal-arguments u8vector-sint-set!)))
   (u8vector-sint-set! bv index value endianness size)
   (= (u8vector-sint-ref bv index endianness size)
      (bytevector-sint-ref by index endianness size)
      value)))

(e.g.
 (let* ((random-sequence 
	 (lambda (n min max)
	   (rest (unfold-n (+ n 1) 
			   (lambda (_) (+ (random (- max min -1)) min)) 0))))
	(random-bytevector 
	 (lambda (n)
	   (u8-list->bytevector (random-sequence n 0 255))))
	(bv (random-bytevector 256)))
   (for-every size in (1 .. 8)
     (let ((last-index (- 256 size))
	   (max-value+1 (expt 256 size)))
       (for-every endianness in '(big little)
	 (and (for-every index in (0 .. last-index)
		(and (= (u8vector-uint-ref bv index endianness size)
			(bytevector-uint-ref bv index endianness size))
		     (= (u8vector-sint-ref bv index endianness size)
			(bytevector-sint-ref bv index endianness size))))
	      (for-every index in (0 .. last-index)
		;; note that this code cannot be parallelised easily
		;; because of side effects -- therefore we make use
		;; of the fact that the code is executed sequentially
		(and (let ((unsigned-value (random max-value+1)))
		       (u8vector-uint-set! bv index unsigned-value 
					   endianness size)
		       (= (u8vector-uint-ref bv index endianness size)
			  unsigned-value))
		     (let ((signed-value (- (random max-value+1)
					    (* 1/2 max-value+1))))
		       (u8vector-sint-set! bv index signed-value 
					   endianness size)
		       (= (u8vector-sint-ref bv index endianness size)
			 signed-value))))))))))

|#

(let-syntax ((define-u8vector-accessors
	       (lambda (stx)
		 (define (syntax-name . syntaxes)
		   (datum->syntax 
		    stx
		    (string->symbol
		     (fold-right string-append "" 
				 (map ->string (map syntax->datum 
						    syntaxes))))))
		 (define (bits->bytes bits)
		   (let ((bits (syntax->datum bits)))
		     (datum->syntax stx (ceiling (/ bits 8)))))
		 (syntax-case stx ()
		   ((_ bits)
		    (let ((bits (syntax->datum #'bits)))
		      (and (integer? bits)
			   (positive? bits)))
		    (with-syntax ((u8vector-u*-ref 
				   (syntax-name #'u8vector-u #'bits #'-ref))
				  (u8vector-u*-set!
				   (syntax-name #'u8vector-u #'bits #'-set!))
				  (u8vector-s*-ref 
				   (syntax-name #'u8vector-s #'bits #'-ref))
				  (u8vector-s*-set!
				   (syntax-name #'u8vector-s #'bits #'-set!))
				  (byte-size (bits->bytes #'bits)))
		      #'(begin
			  (define (u8vector-u*-ref bv index endianness)
			    (u8vector-uint-ref bv index endianness byte-size))
			  (define (u8vector-u*-set! bv index value endianness)
			    (u8vector-uint-set! bv index value 
						endianness byte-size))
			  (define (u8vector-s*-ref bv index endianness)
			    (u8vector-sint-ref bv index endianness byte-size))
			  (define (u8vector-s*-set! bv index value endianness)
			    (u8vector-sint-set! bv index value 
						endianness byte-size))
			  )))))))
  (define-u8vector-accessors 8)
  (define-u8vector-accessors 16)
  (define-u8vector-accessors 32)
  (define-u8vector-accessors 64))

(let-syntax 
    ((define-u8vector-ieee-accessors
       (lambda (stx)
	 (define (syntax-name . syntaxes)
	   (datum->syntax 
	    stx
	    (string->symbol
	     (fold-right string-append "" 
			 (map ->string (map syntax->datum  syntaxes))))))
	 (define (bytes->bits bytes)
	   (let ((bytes (syntax->datum bytes)))
	     (datum->syntax stx (* bytes 8))))			      
	 (syntax-case stx ()
	   ((_ name bytes)
	    (let ((bits (bytes->bits #'bytes)))
	      (with-syntax ((u8vector-ieee-*-ref
			     (syntax-name #'u8vector-ieee- #'name #'-ref))
			    (u8vector-ieee-*-set!
			     (syntax-name #'u8vector-ieee- #'name #'-set!))
			    (bytevector-ieee-*-ref
			     (syntax-name #'bytevector-ieee- #'name #'-ref))
			    (bytevector-ieee-*-set!
			     (syntax-name #'bytevector-ieee- #'name #'-set!))
			    (u8vector-u/bits/-ref
			     (syntax-name #'u8vector-u bits #'-ref))
			    (u8vector-u/bits/-set!
			     (syntax-name #'u8vector-u bits #'-set!))
			    (bytevector-u/bits/-ref
			     (syntax-name #'bytevector-u bits #'-ref))
			    (bytevector-u/bits/-set!
			     (syntax-name #'bytevector-u bits #'-set!)))
		#'(begin
		    (define (u8vector-ieee-*-ref bv index endianness)
		      (let ((box (make-bytevector bytes)))
			(bytevector-u/bits/-set!
			 box 0 (u8vector-u/bits/-ref bv index endianness)
			 endianness)
			(bytevector-ieee-*-ref box 0 endianness)))
		    (define (u8vector-ieee-*-set! bv index value endianness)
		      (let ((box (make-bytevector bytes)))
			(bytevector-ieee-*-set! box 0 value endianness)
			(u8vector-u/bits/-set! 
			 bv index (bytevector-u/bits/-ref box 0 endianness)
			 endianness)))))))))))
  (define-u8vector-ieee-accessors single 4)
  (define-u8vector-ieee-accessors double 8))
