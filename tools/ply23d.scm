#!/usr/bin/guile \
-e main -s
!#
(use-modules (extra common)
	     (extra ref))

;; convert the stanford's PLY (3d-mesh description format) to SLAYER's
;; .3d format

;; the code here might seem fairly complex as for a simple
;; task that it does, which is parsing the Stanford's PLY format.
;; The complexity stems from the flexibility which the PLY format offers.
;; In short, the converter first reads the header of a PLY file, and then
;; reads the rest of the contents, according to a specification given
;; in the header. This "according to" is the most tricky part

(define (main args)
  (match args
    ((program-name input-file-names ...)
     (for input-file-name in input-file-names
	  (let* ((base-name (string-remove-suffix ".ply" input-file-name))
		 (output-file-name (string-append base-name ".3d")))
	    (with-output-to-file output-file-name
	      (lambda ()
		(pretty-print 
		 (remove (matches? ('texture-coords . _))
			 (process-ply-ascii-file input-file-name))))))))))

;; helper function to construct regular expressions -- it takes a list
;; of patterns an returns a regular expression that matches a sequence
;; consisting of those patterns, separated by one or more spaces, and
;; anchored to the beginning and end of line
(define ($ . args)
  (apply string-append `("^\\s*" ,(string-join args "\\s+") "\\s*$")))

;; regexp that captures a label (used throughout the code)
(define label "([_a-zA-Z0-9]+)")

;; generates procedure to be called for malformed input
(define ((cry message) expression)
  (error message expression))

(define (process-ply-ascii-file file)
  (process-ply-ascii-port (open-input-file file)))

;; the most general procedure to process PLY data from a specified port
(define (process-ply-ascii-port port)
  (with-input-from-port port
    (lambda ()
      (expect-strings
       ("^ply$"
	(expect-strings
	 (($"format" "ascii" "[-0-9._a-zA-Z]+")
	  (process-body (process-header)))
	 (($".*") => (cry "unknown format"))))
       (($".*") => (cry "unrecognised file type"))))))

;; reads the header of a ply file from (current-input-port).
;; If the header looks like this
;; ===========================================================================
;; element vertex 52
;; property float x
;; property float y
;; property float z
;; property uchar red
;; property uchar green
;; property uchar blue
;; element face 18
;; property list uchar uint vertex_indices
;; end_header
;; ===========================================================================
;; then the returned value will look like this:
;; (("vertex" 52 (("x" "float") ("y" "float") ("z" "float") 
;;	       ("red" "uchar") ("green" "uchar") ("blue" "uchar"))) 
;; ("face" 18 (("vertex_indices" ("list" "uchar" "uint")))))
(define (process-header)
  (let next ((header '()))
    (expect-strings
     (($"comment" ".*") (next header))
     (($"\\s*") (next header))
     (($"element"label"([0-9]+)$")
      => (lambda (_ element number)
	   (let ((number (string->number number))
		 (properties '()))
	     (next `((,element ,number ,properties) ,@header)))))
     (($"property" "list" label label label)
      => (lambda (_ count-type element-type property)
	   (match header
	     (((element number properties) . rest)
	      (let ((properties* `(,@properties 
				   (,property 
				    ("list" ,count-type ,element-type)))))
		(next `((,element ,number ,properties*) ,@rest)))))))
     (($"property" label label)
      => (lambda (_ type property)
	   (match header
	     (((element number properties) . rest)
	      (let ((properties* `(,@properties (,property ,type))))
		(next `((,element ,number ,properties*) ,@rest)))))))
     (($"end_header")
      (reverse header))
     (($".*") => (cry "corrupted header or unsupported feature")))))

;; the header obtained from `process-header' is passed directly to
;; `process-body' procedure
(define (process-body header)
  `(mesh
    ,@(append-map 
       (match-lambda
	   ((type count properties)
	    (match properties
	      (((property ("list" count-format element-format)))
	       (process-element-lists count count-format element-format))
	      (((names formats) ...)
	       (process-elements count type properties))
	      (else 
	       (throw "invalid header" else))
	      )))
       header)))

(publish
 (define (process-element-lists count count-format element-format)
   (let ((faces #[])
	 (count-pattern (format->pattern count-format))
	 (element-pattern (format->pattern element-format))
	 (guile-format (format:ply->guile element-format)))
     (for i in 0 .. (- count 1)
	  (let* ((size (read-size count-pattern))
		 (elements (read-elements size element-pattern))
		 (gl-primitive (size->gl-primitive size)))
	    (hash-set! faces gl-primitive
		       `(,elements ,@(hash-ref faces gl-primitive '())))))
     `((faces ,@(hash-map->list 
		 (lambda (gl-primitive indices)
		   (let ((indices (reverse indices)))
		     (if (eq? gl-primitive 'polygon)
			 (map (lambda(index-set)
				`(polygon ,(list->typed-array 
					    guile-format
					    1 index-set)))
			      indices)
			 `(,gl-primitive ,(list->typed-array 
					   guile-format 
					   2 indices)))))
		 faces))))) 
 where
 (define size->gl-primitive
   (applicable-hash-with-default 
    'polygon
    (1 'points)
    (2 'lines)
    (3 'triangles)
    (4 'quads)))
 (define (read-size count-pattern)
   (expect-strings
    ((string-append"^\\s*"count-pattern"\\s*")
     => (lambda (_ size) (string->number size)))
    (($".*") => (cry "invalid list"))))
 (define (read-elements size element-pattern)
   (expect-strings
    ((apply $ (make-list size element-pattern))
     => (lambda (_ . items)
	  (map string->number items)))
    (($".*") => (cry "invalid list elements"))))
 )

(publish
 (define (process-elements count type properties)
   (match properties
     (((names formats) ...)
      (let ((result #[])
	    (pattern (apply $ (map format->pattern formats)))
	    (indices (map (lambda(name)
			    (property->index type name)) 
			  names))
	    (matrices (map (lambda(name)
			     (property->matrix type name)) 
			   names))
	    (descriptions (matrix-descriptions type properties)))
	(for (type size format) in descriptions
	     (set! #[result type] (make-typed-array (format:ply->guile format)
						    0 count size)))
	(for n in 0 .. (- count 1)
	     (let ((data (read-elements pattern)))
	       (for (type index value) in (zip matrices indices data)
		    (set! #[#[result type] n index] value))))
	(hash-map->list list result)))))
 where
 (define (read-elements pattern)
   (expect-strings
    (pattern => (lambda (_ . args) (map string->number args)))
    (($"comment"".*") (read-elements pattern))
    (($".*") => (cry "invalid elements"))))
 )

(publish
 (define (format->pattern format)
   (format-pattern (format-description format)))
 (define (format:ply->guile format)
   (guile-format (format-description format)))
 where
 (define format-description
   ;; the `format-description' allows to parse the body using regexps.
   ;; The word ``format'' is used whroughout the code to designate 
   ;; the entries like "float", "uchar", "int" from the PLY files.
   ;; Those entries are usually called "types", but here the word
   ;; "type" is reserved for types of 3d data (vertices, normals etc.)
   (applicable-hash-with-default 
    '("([-+]?[0-9]*\\.?[0-9]+[df]?)" f32)
    ("float"  '("([-+]?[0-9]*\\.?[0-9]+[df]?)" f32))
    ("double"  '("([-+]?[0-9]*\\.?[0-9]+[df]?)" f64))
    ("uchar"  '("([0-9]+)" u8))
    ("char"  '("([-+]?[0-9]+)" s8))
    ("ushort"  '("([0-9]+)" u16))
    ("short"  '("([-+]?[0-9]+)" s16))
    ("uint"  '("([0-9]+)" u32))
    ("int" '("([-+]?[0-9]+)" s32))))
 (define-accessors (format-pattern guile-format))
 )

;; for a given element type (currently only "vertex") and set of properties
;; provide a description of the expected result, consisting of a type, size
;; (or number of dimensions for the given type) and format, e.g.
;; (matrix-descriptions "vertex" '(("x" "float") ("z" "int"))) would return
;; ((vertices 3 "float")), because if the "z" property is given, the matrix
;; is three-dimensional (in this case, the value of "y" will be zero)
(publish
 (define (matrix-descriptions element-type properties)
   (and-let* ((property->matrix-type 
	       (element:->:property->matrix-type element-type)))
     (match properties
       (((property-names property-formats) ...)
	(map (match-lambda (((types sizes indices formats) ...)
			    `(,(first types) ,(apply max sizes) 
			      ,(fold broader-format (first formats)
				     (rest formats)))))
	     (equivalence-classes 
	      (match-lambda* (((type-1 . _) (type-2 . _))
			      (eq? type-1 type-2)))
	      (map (match-lambda 
		       ((name format)
			`(,@(property->matrix-type name) ,format)))
		   properties)))))))
 (define (property->index element-type property)
   (property-index ((element:->:property->matrix-type element-type) property)))
 (define (property->matrix element-type property)
   (array-type ((element:->:property->matrix-type element-type) property)))
 where
 (define-accessors (array-type minimal-size property-index))
 (define element:->:property->matrix-type
    (applicable-hash
     ("vertex" 
      (applicable-hash
       ;;property   array-type     minimal-size index
       ("x"       '(vertices       2            0    ))
       ("y"       '(vertices       2            1    ))
       ("z"       '(vertices       3            2    ))
       ("w"       '(vertices       4            3    ))
       ("nx"      '(normals        3            0    ))
       ("ny"      '(normals        3            1    ))
       ("nz"      '(normals        3            2    ))
       ("s"       '(texture-coords 1            0    ))
       ("t"       '(texture-coords 2            1    ))
       ("red"     '(colors         3            0    ))
       ("green"   '(colors         3            1    ))
       ("blue"    '(colors         3            2    ))
       ("alpha"   '(colors         4            3))))))
 )

;; given two formats (e.g. "char" and "ushort") return a smallest format
;; large enought to store all values that can be stored in any of those two
;; (in the above example the result would be "int")
(publish
 (define (broader-format a b)
   (let ((a-index (list-index (equals? a) order))
	 (b-index (list-index (equals? b) order)))
     (cond ((equal? a b) a)
	   ((and a-index b-index) (if (< a-index b-index) b a))
	   (a-index (broader-format a (next (base b))))
	   (b-index (broader-format (next (base a)) b))
	   (else (broader-format (next (base a)) (next (base b)))))))
 where
 (define order '("char" "short" "int" "long" "float" "double"))
 (define u-format (string-append "^u(("(string-join order ")|(")"))\\s*$"))
 (define (first-match regex string)
   (and-let* ((ms (string-match regex string)))
     (match:substring ms 1)))
 (define (base format)
   (or (first-match u-format format) (last #;in order)))
 (define (next format)
   (or (and-let* ((index (list-index (equals? format) order))
		  (next-index (1+ index))
		  ((< next-index (length order))))
	 (list-ref order next-index))
       (last #;in order)))
 )
