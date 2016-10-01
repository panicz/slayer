(define-module (butterfly cursor)
  #:use-module (grand scheme)
  #:export (cursor?
	    focus
	    cursor-next
	    cursor-previous
	    cursor-points-to-location?
	    cursor-points-to-expression?
	    splice-subexpression
	    replace-subexpression
	    remove-subexpression
	    cursor<
	    cursor-contains?
	    cursor-expression+location
	    cursor-depth))

(define (cursor? x)
  (or (natural? x)
      (null? x)
      (and-let* (((h . t) x)
		 ((natural? h))
		 ((cursor? t))))))

(e.g.
 (and (cursor? 0)
      (cursor? '())
      (cursor? '(0 1 0))
      (cursor? '(0 1 0 . 0))))

(define (focus expression #;on cursor)
  "Select a subexpression pointed to by a cursor."
  (match cursor
    (()
     expression)
    ((h . t)
     (focus (list-ref expression h) #;on t))
    (_
     (drop expression cursor))
    ))

(e.g.
 (focus '(+ (* 2 3) (/ 3 5)) '(1 0)) ===> *)

(e.g.
 (focus '(+ (* 2 3) (/ 3 5)) '(1 . 1)) ===> (2 3))

(define (cursor-next #;to cursor #;in expression)
  (if (null? cursor)
      (cursor-next #;to 0 #;in expression)
      (match `(,expression ,cursor)
	(((head . tail) (n . next))
	 (let* ((subexpression (list-ref expression n))
		(subcursor (cursor-next #;to next #;in subexpression)))
	   (if (null? subcursor)
	       (+ n 1)
	       `(,n . ,subcursor))))
	(((head . tail) n)
	 (let ((subexpression (drop expression n)))
	   (if (pair? subexpression)
	       `(,n)
	       '())))
	(_
	 '())
	)))

(define (cursor-previous #;to cursor #;in expression)
  (let ((cursors (unfold-right-until
		  (lambda (c) (or (equal? c cursor) (null? c)))
		  #;using (lambda (c) (cursor-next #;to c #;in expression))
			  #;starting-from 0)))
    (match cursors
      ((previous . _)
       previous)
      (_
       '())
      )))

(define (cursor-points-to-location? cursor)
  ;;(assert (cursor? cursor))
  (or (natural? cursor)
      (and-let* (((n . subcursor) cursor))
	(cursor-points-to-location? subcursor))))

(define (cursor-points-to-expression? cursor)
  ;;(assert (cursor? cursor))
  (list? cursor))

(define (splice-subexpression x #;to expression #;at cursor)
  ;;(assert (points-to-location? cursor))
  (match cursor
    ((n . subcursor)
     (let ((prefix (subexpression . suffix) (split-at expression n)))
       `(,@prefix
	 ,(splice-subexpression x #;to subexpression #;at subcursor)
	 ,@suffix)))
    (_
     (let ((prefix suffix (split-at expression cursor)))
       (if (null? suffix)
	   `(,@prefix ,@x) ;; handle dotted pairs
	   `(,@prefix ,@x ,@suffix))))
    ))

(e.g.
 (splice-subexpression '(c) #;to '(* (+ a b) (/ d e)) #;at '(1 . 3))
 ===> (* (+ a b c) (/ d e)))

(define (replace-subexpression #;of expression #;at cursor #;with x)
  (match cursor
    ((n . subcursor)
     (let ((prefix (subexpression . suffix) (split-at expression n)))
       `(,@prefix
	 ,(replace-subexpression #;of subexpression #;at subcursor
				      #;with x)
	 ,@suffix)))
    (()
     x)
    (_
     `(,@(take expression cursor) ,@x))
    ))

(e.g.
 (replace-subexpression #;of '(* (+ a 1) (/ c d)) #;at '(1 2)
			     #;with 'b)
 ===> (* (+ a b) (/ c d)))

(define (remove-subexpression #;of expression #;at/after cursor)
  (cond ((natural? cursor)
	 (take expression cursor))
	((null? cursor)
	 '())
	(else
	 (let* (((n . subcursor) cursor)
		(prefix (subexpression . suffix) (split-at expression n)))
	   `(,@prefix
	     ,@(if (null? subcursor)
		   '()
		   `(,(remove-subexpression #;of subexpression
						 #;at subcursor)))
	     ,@suffix)))
	))

(e.g.
 (remove-subexpression #;of '(* (+ a c b) (/ c d)) #;at '(1 2))
 ===> (* (+ a b) (/ c d)))

(e.g.
 (remove-subexpression #;of '(* (+ a b c) (/ c d)) #;after '(1 . 1))
 ===> (* (+) (/ c d)))

(define (cursor< cursor-a cursor-b)
  "Is cursor-a before cursor-b?"
  (cond ((and (number? cursor-a) (number? cursor-b))
	 (< cursor-a cursor-b))
	
	((null? cursor-b)
	 (not (null? cursor-a)))
	
	((null? cursor-a)
	 => not)
	
	((number? cursor-a)
	 (let (((m . _) cursor-b))
	   (<= cursor-a m)))
	
	((number? cursor-b)
	 (let (((n . _) cursor-a))
	   (< n cursor-b)))
	
	(else
	 (let (((n . n+) cursor-a)
	       ((m . m+) cursor-b))
	   (or (< n m)
	       (and (= n m)
		    (cursor< n+ m+)))))
	))

(define (cursor-contains? outer inner)
  "Does outer contain inner?"
  (or (null? outer)
      (and-let* (((a . a*) outer)
		 ((b . b*) inner)
		 ((= a b))
		 ((cursor-contains? a* b*))))))

(e.g.
 (cursor-contains? '(1 0) '(1 0 0 . 1)))

(define (cursor-expression+location cursor)
  (match cursor
    ((prefix . rest)
     (let ((expression location (cursor-expression+location rest)))
       (values `(,prefix . ,expression) location)))
    (_
     (values '() cursor))
    ))

(e.g.
 (cursor-expression+location '(1 2 . 3)) ===> (1 2) 3)

(define (cursor-depth cursor)
  (match cursor
    ((n . subcursor)
     (+ 1 (cursor-depth subcursor)))
    (()
     0)
    (_
     1)
    ))
