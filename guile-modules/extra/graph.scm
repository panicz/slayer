(define-module (extra graph)
  #:use-module (extra common)
  #:export (graph? (nodes . graph-nodes) (paths . graph-paths)
		   (path? . graph-path?)))

(define (graph? x)
  (and (list? x)
       (every list? x)
       (let ((nodes (map first x)))
	 (and (not (contains-duplicates? nodes))
	      (for-every (node links ...) in x
		(for-every link in links
		  (in? link nodes)))))))

(e.g.
 (graph? '((a b)
	   (b c)
	   (c d)
	   (d a))))

(e.g.
 (graph? '((a b c d)
	   (b)
	   (c)
	   (d))))

(assert (let ((G (a graph?)))
	  (assoc? G)))

(define (nodes #;of graph)
  (assert (graph? graph))
  (match-let ((((nodes . _) ...) graph))
    nodes))

(e.g. (nodes '((a b)
	       (b c)
	       (c d)
	       (d a)))
      same-set? '(a b c d))

(define (symmetric-graph? graph)
  (assert (graph? graph))
  (for-every (node-a a-links ...) in graph
    (for-every (node-b b-links ...) in graph
      (equiv? (in? node-b a-links)
	      (in? node-a b-links)))))

(e.g. (not (symmetric-graph? '((a b)
			       (b c)
			       (c d)
			       (d a)))))

(e.g. (symmetric-graph? '((a b d)
			  (b c a)
			  (c d b)
			  (d a c))))

(define (extend-to-symmetric-graph graph)
  (assert (graph? graph))
  (map (lambda ((node-a a-links ...))
	 `(,node-a ,@(apply union a-links 
			    (filter-map (lambda ((node-b b-links ...))
					  (and (not (eq? node-a node-b))
					       (in? node-a b-links)
					       `(,node-b)))
					graph))))
       graph))

(define (same-graph? a b)
  (assert (graph? a)
	  (graph? b))
  (and (same-set? (nodes a) (nodes b))
       (for-every node in (nodes a)
	 (same-set? (assoc-ref a node)
		    (assoc-ref b node)))))

(e.g.
 (extend-to-symmetric-graph '((a b)
			      (b c)
			      (c d)
			      (d a)))
 same-graph? '((a b d)
	       (b c a)
	       (c d b)
	       (d a c)))

(assert (let ((G (a graph?)))
	  (symmetric-graph? (extend-to-symmetric-graph G))))

(define (path? x #;on graph)
  (assert (and (list? x)
	       (graph? graph)
	       (subset? x (nodes graph))
	       (unique? x)))
  (match x
    ((start end)
     (in? end (assoc-ref graph start)))
    ((first second rest ...)
     (and (in? second (assoc-ref graph first))
	  (path? `(,second ,@rest) #;on graph)))
    (else
     #f)))

(define* (path #;from start #;to end #;on graph #:key (except '())
		      (reorder (lambda(links)links)))
  "returns a path leading from start to end, or #f if it does not exist.\
 An optional #:reorder procedure can be given to optimize search."
  (assert (and (in? start (nodes graph))
	       (in? end (nodes graph))
	       (not (eq? start end))
	       (subset? except (nodes graph))))
  (let ((links (difference (assoc-ref graph start) except)))
    (cond ((or (null? links) (in? start except))
	   #f)
	  ((in? end links)
	   `(,start ,end))
	  (else
	   (and-let* ((subpath (find-map
				(lambda(x)
				  (path #;from x #;to end #;on graph
					       #:except `(,start ,@except)))
				(reorder links))))
	     `(,start ,@subpath))))))

(define* (paths #;from start #;to end #;on graph #:key (except '()))
  "returns all paths in the graph leading from start to end"
  (assert (and (in? start (nodes graph))
	       (in? end (nodes graph))
	       (subset? except (nodes graph))))
  (let ((links (difference (assoc-ref graph start) except)))
    (cond ((or (null? links) (in? start except))
	   #f)
	  ((in? end links)
	   `((,start ,end)))
	  (else
	   (let ((subpaths (concatenate 
			    (filter-map 
			     (lambda(x)
			       (paths #;from x #;to end #;on graph
					     #:except `(,start ,@except)))
			     links))))
	     (and (not (null? subpaths))
		  (map (lambda (subpath)
			 `(,start ,@subpath))
		       subpaths)))))))

(e.g. (let ((G '((A B C)
		 (B L M A)
		 (C N O A)
		 (L B D E)
		 (M B F G)
		 (N C H I)
		 (O C J K)
		 (D L)
		 (E L)
		 (F M)
		 (G M)
		 (H N)
		 (I N)
		 (J O)
		 (K O))))
	(paths #;from 'K #;to 'D #;in G))
      ===> ((K O C A B L D)))

(assert (let* ((G (a graph?))
	       (x (a (lambda(x)(node? x G))))
	       (y (a (lambda(y)(and (node? y G)
				    (not (eq? y x))))))
	       (p (path #;from x #;to y #;on G))
	       (P (paths #;from x #;to y #;on G)))
	  (if p (and (path? p)
		     (for-all x in P
		       (path? x #;on G))
		     (in? p P)))))

(assert (let* ((G (a symmetric-graph?))
	       (x (a (lambda(x)(node? x G))))
	       (y (a (lambda(y)(and (node? y G)
				    (not (eq? y x))))))
	       (P (paths #;from x #;to y #;on G))
	       (R (paths #;from y #;to x #;on G))
	       (p (element P)))
	  (in? (reverse p) R)))
