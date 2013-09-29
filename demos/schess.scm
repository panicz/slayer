#! /bin/sh
./slayer -d3d -i$0
exit
!#
(use-modules (slayer) (slayer image)
	     (widgets base) (widgets bitmap)
	     (oop goops) 
	     (extra common) (extra ref)
	     (schess elements))

(keydn 'esc quit)

;; DO ZROBIENIA:
;; - wczytywanie i przetwarzanie danych z pliku z regułami ~~
;; - ładowanie planszy ("initial board"), w szczególności
;;   wyświetlanie wszystkich rodzajów figur ~~
;; - zrobienie, żeby przy kliknięciu podświetlały się
;;   dopuszczalne pola

(define rgba
  (case-lambda 
    ((color)
     (decompose-color-to-rgba color))
    ((r g b a)
     (compose-color-from-rgba r g b a))))

(define* (highlighted image #:key (red 0) (green 0) (blue 0))
  (array->image
   (array-map (lambda(pixel)
		(match-let (((r g b a) (rgba pixel))
			    (+ (lambda (x y) (min 255 (+ x y)))))
		  (rgba (+ r red) (+ g green) (+ b blue) a)))
	      (image->array image))))

(define* (subtract-image subtrahend minuend #:key (tolerance 100))
  (array->image
   (array-map (lambda (x y)
		(let ((xrgba (rgba x))
		      (yrgba (rgba y)))
		  (let ((diffs (map abs (map - xrgba yrgba))))
		    (if (< (apply max diffs) 100)
			0 ;; make this pixel of checker transparent
			x))))  ;; or leave it as it is
	      (image->array subtrahend)
	      (image->array minuend))))

(define empty-field (load-image "art/chess/b.png"))

(define empty-field* (load-image "art/chess/w.png"))

(define-class <field> (<bitmap>)
  (%content #:init-value #f)
  (content #:allocation #:virtual
	   #:slot-ref (lambda (self)
			#[self '%content])
	   #:slot-set! (lambda (self object)
			 (set! #[self '%content] object)
			 (set! #[object 'parent] self)))
  (position #:init-keyword #:position #;(x y))
  (children #:allocation #:virtual
	    #:slot-ref (lambda (self)
			 (if #[self 'content] 
			     (list #[self 'content]) 
			     '()))
	    #:slot-set! (lambda (self value)
			  (if (list? value)
			      (for x in value
				   (set! #[self 'content] x))))))

(define-method (draw (field <field>))
  (next-method)
  (if #[field 'content]
      (draw #[field 'content])))

(define-class <checker> (<bitmap>)
  (type #:init-value #f #:init-keyword #:type))

(define-method (initialize (checker <checker>) init-args)
  (next-method)
  (set! #[checker 'left-mouse-down] ;; powinno być start-dragging
	(lambda (x y)
	  (and-let* ((parent #[checker 'parent])
		     ((is-a? parent <field>))
		     (grandpa #[parent 'parent])
		     ((is-a? grandpa <board>)))
	    ;; tutaj jakoś chcielibyśmy może sprawdzić, które ruchy
	    ;; są dozwolone, i odpowiednio je oznaczyć
	    (<< #[parent 'position])
	    (<< (current-board-state/rect grandpa))

	    (set! #[checker 'x] #[parent 'x])
	    (set! #[checker 'y] #[parent 'y])
	    (set! #[parent 'content] #f)
	    (set! #[checker 'parent] grandpa)
	    (push! #[grandpa 'above-fields] checker)
	    )))
  (set! #[checker 'left-mouse-up] ;; powinno być stop-dragging
	(lambda (x y)
	  (and-let* (((is-a? *nearby-widget* <field>))
		     (parent #[checker 'parent])
		     ((is-a? parent <board>)))
	    (set! #[parent 'above-fields] (delete checker
						  #[parent 'above-fields]))
	    (set! #[*nearby-widget* 'content] checker)
	    (set! #[checker 'x] 0)
	    (set! #[checker 'y] 0)
	    (set! #[checker 'parent] *nearby-widget*))))
  (set! #[checker 'drag]
	(lambda (x y dx dy)
	  (increase! #[checker 'x] dx)
	  (increase! #[checker 'y] dy))))

(define-class <board> (<widget>)
  (fields #:init-value #f)
  (above-fields #:init-value '())
  (children #:allocation #:virtual
	    #:slot-ref (lambda (self)
			 (append #[self 'above-fields] 
				 (flatten (array->list #[self 'fields]))))
	    #:slot-set! noop))

(define-method (initialize (board <board>) init-args)
  (next-method)
  (let-keywords init-args #t ((initial-state '((_)))
			      (images #[]))
    (match-let (((w h) (rect-size initial-state)))
      (set-screen-size! (* w (image-width empty-field)) 
			(* h (image-height empty-field)))
      (set! #[board 'w] (screen-width))
      (set! #[board 'h] (screen-height))
      (set! #[board 'fields] (make-array 2 h w))
      (for x in 0 .. (- w 1)
	   (for y in 0 .. (- h 1)
		(match-let* ((field (make <field> #:position `(,x ,y)
					  #:image (if (= 0 (modulo (+ x y) 2))
						      empty-field
						      empty-field*)))
			     (checker (take-from-rect initial-state x y))
			     ((w h) (image-size #[field 'image])))
		  (set! #[field 'parent] board)
		  (set! #[field 'x] (* x w))
		  (set! #[field 'y] (* y h))
		  (set! #[#[board 'fields] y x] field)
		  (and-let* ((image #[images checker])
			     (checker (make <checker> #:image image 
					    #:type checker)))
		    (add-child! field checker))
		  ))))))

(define-method (current-board-state/array (board <board>))
  (array-map (lambda(field)
	       (or #[field : 'content : 'type]
		   '_))
	     #[board 'fields]))

(define-method (current-board-state/rect (board <board>))
  (array->list (current-board-state/array board)))

(publish 
 (define (load-board-game game-description)
   (let ((description (with-input-from-file game-description read)))
     (match description
       (('define-game-rules name . description)
	(set-window-title! (string-append "SCHESS: " (->string name)))
	(let ((initial-board (first #[description 'initial-board:]))
	      (wildcards #[description 'wildcards:])
	      (moves #[description 'moves:])
	      (images (map (match-lambda 
			       ((figure name)
				`(,figure . ,(subtract-image (load-image name)
							     empty-field))))
			   #[description 'images:])))
	  (let ((wildcards (expand-wildcards wildcards))
		(width (rect-width initial-board))
		(height (rect-height initial-board)))
	    (specify ((wildcards wildcards))
		     (let ((allowed-moves (extract-moves moves width height)))
		       ;;(<< allowed-moves)
		       (make <board> #:initial-state initial-board
			     #:images images
			     ;;#:dimensions `(,width ,height)
		       #;(rules allowed-moves))))))))))
 where
 (define (expand-wildcards wildcards)
   (define (value term)
     (match (find (match-lambda ((name value)(equal? name term))) wildcards)
       ((name (values ...))
	(append-map value values))
       ((name atomic-value)
	(value atomic-value))
       (else
	(list term))))
   (map (match-lambda ((name definition) `(,name ,(value name)))) wildcards))

 (define (extract-moves movement-description board-width board-height)
   (let ((player-moves (make-hash-table)))
     (for (player . description) in movement-description
	  (match description
	    ((('transform player-name . details))
	     (<< description))
	    (((figures . moves) ...)
	     (let ((figure-moves (make-hash-table)))
	       (hash-set! player-moves player figure-moves)
	       (for (figure moves) in (zip figures moves)
		    (hash-set! figure-moves figure
			       (expand-moves #;for figure #;from moves #;at
						   board-width 
						   board-height)))))))
	  #;return player-moves))

 (define (expand-moves #;for figure #;from definitions 
			     #;at board-width #;and board-height)
   (append-map
    (match-lambda 
	((initial-state final-state . extra)
	 (let ((symmetries (or #[extra 'symmetries:]
			       '(identity)))
	       (conditions (or #[extra 'conditions:]
			       'none))) ;; na razie pana nie obsługujemy
	   (unique (zip 
		    (append-map (employ symmetries)
				(complements initial-state
					     board-width
					     board-height))
		    (append-map (employ symmetries)
				(complements final-state
					     board-width
					     board-height)))))))
    definitions))

 (define ((employ symmetries) state-description)
   (list state-description)) ;; na razie nic sobie nie robimy
 )

(define b (load-board-game "chess.scm"))

(keydn 's (lambda()(pretty-print (current-board-state/array b))))
(keydn 'e (lambda()(pretty-print (port-encoding (current-output-port)))))

(add-child! *stage* b)
