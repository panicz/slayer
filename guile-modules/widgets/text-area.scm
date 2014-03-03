(define-module (widgets text-area)
  #:use-module (oop goops)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (extra slayer)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (slayer font)
  #:use-module (widgets base)
  #:use-module (widgets image-clipper)
  #:export (<text-area>
	    )
  #:re-export (make)
  )

;; warto by to wyeksplikować, bo tutaj mamy mały PROBLEM!:
;; rzecz w tym, że chcielibyśmy być informowani o tym, że
;; stan widgetu się zmienił, żeby móc go narysować od nowa.

;; być może wymagałoby to ponownego przemyślenia całej infrastruktury
;; widgetowej. musielibyśmy założyć, że każdy widget albo ma skeszowany
;; obrazek, który jest używany do wyświetlania na ekranie (w ten sposób
;; dochodząc do tego, że przerenderowanie sceny to narysowanie jednego
;; obrazka!), albo kesz zwraca false i wówczas rysujemy od nowa
;; (według rekurencyjnej procedury)

;; wymagałoby to pewnego przebudowania SLAYERa tak, żeby OpenGL
;; mógł rysować do dowolnych buforów w pamięci, i dopiero stamtąd
;; przerzucał zawartość buforów na ekran.

;; na potrzeby SLAYERa 2.0 fajnie by było coś takiego zrobić, ale na razie
;; lepiej wybrać jakąś drogę na skróty (do czasu stworzenia grywalnej KUTASY)

;; w przypadku widgeta tekstowego projektant (czyli ja) musi zadbać o to,
;; żeby synchronizacja z keszem się odbywała we właściwych momentach
;; (wiadomo, że docelowo lepiej by było zwalić ten wysiłek na maszynę,
;; która sama miałaby się domyślać, co trzeba przerysować i jakie zmienne
;; spośród tych znajdujących się w obiekcie wpływają na jego wygląd)

;; w przypadku tego widgeta tekstowego byłoby super mieć coś takiego,
;; co potrafiłoby samo wywnioskować, które linie są widoczne, i wymuszać
;; przerysowanie jedynie w przypadku modyfikacji tych, które rzeczywiście
;; mają jakikolwiek wpływ na wygląd

;; tym niemniej na razie postaramy się zrobić to jak najprościej -- tzn.
;; nie przeszkadza nam pewna redundancja, jeżeli zwolni nas z myślenia
;; (i pozwolimy, żeby to rzeczywistość wyciągała na razie za nas wnioski)

;; konkretnie, idzie o to, że jak robimy
;; (set! #[this : 'lines : n] cośtam)
;; to żeby wywoływać jakiś sygnał w obiekcie "this"

(define-class <text-area> (<image-clipper>)
  (%space #:init-value #f)  ;; 
  (%cursor #:init-value #f) ;; cursor image
  (%background #:init-value #f)
  (%%image #:init-value #f)
  (%image
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (or #[self '%%image]
	 (let ((image (rectangle #[self 'width] #[self 'height] #x00000000)))
	   (with-video-output-to image (render self))
	   (set! #[self '%%image] image)
	   image)))
   #:slot-set! noop)
  (background-color #:init-value #f #:init-keyword #:background-color)
  (text-color #:init-value #xffffff #:init-keyword #:text-color)
  (font #:init-value *default-font* #:init-keyword #:font)
  (port #:init-value #f)
  (on-text-change #:init-value noop #:init-keyword #:on-text-change)
  (char-height
   #:allocation #:virtual
   #:slot-ref (lambda (self) (font-line-skip #[self 'font]))
   #:slot-set! noop)
  (height
   #:allocation #:virtual
   #:slot-ref (lambda (self)
		(* #[self 'char-height] 
		   (vector-length #[self 'lines])))
   #:slot-set! noop)
  (char-width
   #:allocation #:virtual
   #:slot-ref (lambda (self) (image-width #[self '%space]))
   #:slot-set! noop)
  (width 
   #:allocation #:virtual
   #:slot-ref (lambda (self)
		(* #[self 'char-width]
		   (apply max  (map string-length
				    (vector->list 
				     #[self 'lines])))))
   #:slot-set! noop)
  (special-keys #:init-thunk 
		(lambda()
		  (make-vector (vector-length *key-names*) noop)))
  (lines #:init-value '#(""))
  (max-lines #:init-value +inf.0 #:init-keyword #:max-lines)
  (on-max-lines-reached 
   #:init-value noop 
   #:init-keyword #:on-max-lines-reached)
  (%render-cache #:init-value #f)
  (%thread-results #:init-thunk make-hash-table))

(define-method (render (t <text-area>))
  (let* ((font #[t 'font])
	 (line-skip (font-line-skip font))
	 (lines #[t 'lines])
	 (cursor #[t '%cursor])
	 (space #[t '%space]))
    (if (or (not #[t '%render-cache])
	    (not (= (vector-length #[t '%render-cache])
		    (vector-length lines))))
	(set! #[t '%render-cache] (make-vector (vector-length lines) #f)))
    (when #[t 'background-color]
      (if (or (not #[t '%background])
	      (not (equal? (image-size #[t '%background])
			   `(,#[t 'width] ,#[t 'height]))))
	  (set! #[t '%background] 
		(rectangle #[t 'width] #[t 'height] #[t 'background-color])))
      (draw-image! #[t '%background]))
    (for n in 0 .. (1- (vector-length lines))
	 (let ((image (or #[t : '%render-cache : n]
			  (let ((fresh-image (render-text 
					      (string-append #[lines n] " ")
					      font
					      #[t 'text-color]
					      #[t 'background-color])))
			    (set! #[t : '%render-cache : n] fresh-image)
			    fresh-image))))
	   (draw-image! image 0 (* n line-skip))))
    (if (equal? (current-output-port) #[t 'port])
	(draw-image! cursor 
		     (* (image-width space) 
			(port-column #[t 'port]))
		     (* line-skip 
			(port-line #[t 'port]))))))

(define-method (last-sexp (t <text-area>))
  (and-let* ((lines (vector->list #[ t 'lines ]))
	     (line (port-line #[ t 'port ]))
	     (column (port-column #[ t 'port ]))
	     (text (string-join 
		    (append (take lines line)
			    (list (substring 
				   #[t : 'lines : line]
				   0 column)))
		    "\n"))
	     (starting-position (last-sexp-starting-position text)))
    (substring text starting-position)))

(define-method (move-cursor! (w <text-area>)
			     (right <integer>)
			     (down <integer>))
  (set! #[w '%cropped-image] #f)
  (set! #[w '%%image] #f)
  (set-port-line! #[ w 'port ] 
		  (max 0 (min (+ (port-line #[w 'port]) down) 
			      (- (vector-length #[w 'lines]) 1))))
  (set-port-column! 
   #[ w 'port ] 
   (max 0 (min 
	   (+ (port-column #[w 'port]) right)
	   (string-length #[w : 'lines : (port-line #[w 'port])])))))

(define-method (delete-char! (w <text-area>))
  (let ((p (slot-ref w 'port)))
    (delete-char! w (port-column p) (port-line p))))

(define-method (delete-char! (w <text-area>) 
			     (col <integer>) 
			     (line <integer>))
  (set! #[w '%cropped-image] #f)
  (set! #[w '%%image] #f)
  (let* ((s #[w : 'lines : line ])
	 (sl (string-length s))
	 (l (substring s 0 (max 0 (min sl (- col 1)))))
	 (r (substring s (min sl col))))
    (set! #[w : 'lines : line] (string-append l r))))

(define-method (break-line! (t <text-area>))
  (set! #[t '%cropped-image] #f)
  (set! #[t '%%image] #f)
  (let ((line #[#[t 'lines] (port-line #[t 'port])])
	(line-number (port-line #[ t 'port ]))
	(lines (vector->list #[ t 'lines ]))
	(column (port-column #[ t 'port ])))
    (if (>= (length lines) #[t 'max-lines])
	(#[t 'on-max-lines-reached] t)
    #;else
	(let ((left (substring line 0 column))
	      (right (substring line column)))
	  (set! #[ t 'lines ]
		(list->vector
		 (append (take lines line-number)
			 (list left right)
			 (drop lines (+ line-number 1)))))
	  (move-cursor! t (- (port-column #[t 'port])) 
			1)))))

(define-method (leave-typing-mode! (t <text-area>))
  (set! #[t '%cropped-image] #f)
  (set! #[t '%%image] #f)
  (set-current-output-port *stdout*)
  (set-direct-input-mode!))

(define-method (move-cursor-back! (t <text-area>))
  (set! #[t '%cropped-image] #f)
  (set! #[t '%%image] #f)
  (let ((line (port-line #[ t 'port ]))
	(column (port-column #[ t 'port ])))
    (if (and (= column 0)
	     (> line 0))
	(move-cursor! 
	 t 
	 (string-length #[#[t 'lines](1- line)]) 
	 -1)
    #;else
	(move-cursor! t -1 0))))

(define-method (move-cursor-forward! (t <text-area>))
  (set! #[t '%cropped-image] #f)
  (set! #[t '%%image] #f)
  (let ((line (port-line #[ t 'port ]))
	(column (port-column #[ t 'port ])))
    (if (and (= column (string-length 
			#[t : 'lines : line]))
	     (< (+ line 1) 
		(vector-length #[t 'lines])))
	(move-cursor! t (- (string-length #[t : 'lines : line])) 1)
    #;else
	(move-cursor! t 1 0))))

(define-method (move-cursor-to-the-end-of-line! (t <text-area>))
  (set! #[t '%cropped-image] #f)
  (set! #[t '%%image] #f)
  (let* ((port #[t 'port])
	 (line #[t : 'lines : (port-line port)]))
    (move-cursor! t (- (string-length line) 
		       (port-column port)) 
		  0)))

(define-method (move-cursor-to-the-beginning-of-line! (t <text-area>))
  (set! #[t '%%image] #f)
  (move-cursor! t (- (port-column #[t 'port])) 0))

(define-method (delete-previous-char! (t <text-area>))
  (set! #[t '%cropped-image] #f)
  (set! #[t '%%image] #f)
  (set! #[t '%render-cache] #f)
  (let ((p #[ t 'port ])
	(lines #[ t 'lines ]))
    (if (= (port-column p) 0)
	(let*-values (((pre post) (split-at (vector->list lines)
					    (port-line p))))
	  (if (> (length pre) 0)
	      (let ((this (last pre))
		    (pre (drop-right pre 1)))
		(match post 
		  ((next . rest)
		   (set! #[ t 'lines ] 
			 (list->vector 
			  (append pre 
				  `(,(string-append this next)) 
				  rest)))
		   (move-cursor! t (string-length #[lines (- (port-line p) 1)])
				 -1))))))
    #;else
	(begin 
	  (delete-char! t)
	  (move-cursor! t -1 0)))))

(define-method (delete-next-char! (t <text-area>))
  (set! #[t '%cropped-image] #f)
  (set! #[t '%%image] #f)
  (set! #[t '%render-cache] #f)
  (let* ((p #[ t 'port ])
	 (lines #[ t 'lines ]))
    (if (= (port-column p) 
	   (string-length #[ lines (port-line p) ]))
	(let*-values (((pre post) 
		       (split-at (vector->list lines)
				 (+ (port-line p) 1))))
	  (if (> (length pre) 0)
	      (let ((this (last pre))
		    (pre (drop-right pre 1)))
		(match post 
		  ((next . rest)
		   (set! #[t 'lines]
			 (list->vector
			  (append pre
				  `(,(string-append this next))
				  rest))))))))
    #;else
	(begin
	  (delete-char! t (+ (port-column p) 1) (port-line p)) 
	  (move-cursor! t 0 0)))))

(define-method (initialize (t <text-area>) args)
  (next-method)
  (let-keywords args #t ((text "hi! :)\n"))
    (let ((put-string (lambda(s)
			(set! #[t '%cropped-image] #f)
			(set! #[t '%%image] #f)
			(let ((p #[ t 'port ]))
			  (let ((row (port-line p))
				(col (port-column p)))
			    (set! #[t : '%render-cache : row] #f)
			    (let ((line #[t : 'lines : row ]))
			      (set! #[t : 'lines : row]
				    (string-append
				     (substring line 0 col)
				     s
				     (substring line col)))))))))
      (set! #[t '%cursor] (rectangle 2 #;(image-width #[t '%space]) 
					#[t 'char-height]
					#x20eeaa22))
      (set! #[t '%space] (render-text "_" #[t 'font]))
      (set! #[t 'lines] (list->vector (string-split text #\newline)))
      (set! #[t 'port] (make-soft-port
			(vector
			 (lambda(c) 
			   (put-string (list->string (list c))))
			 put-string
			 noop
			 #;(lambda()
			 (for-each display 
			 (vector->list #[t 'lines])))
			 #f
			 #f) "w"))
      (set! #[t 'left-click]
	    (lambda _
	      (set! #[t '%cropped-image] #f)
	      (set! #[t '%%image] #f)
	      (set-current-output-port #[ t 'port ])
	      (set-typing-special-procedure! 
	       (lambda(scancode)
		 (#[t : 'special-keys : scancode])))
	      (set-typing-input-mode!)))
      (let* ((set-key! (lambda (key action)
			 (set! #[#[t 'special-keys] #[*scancodes* key]]
			       (lambda()(action t))))))
	(set-key! "esc" leave-typing-mode!)
	(set-key! "return" break-line!)
	(set-key! "left"  move-cursor-back!)
	(set-key! "right" move-cursor-forward!)
	(set-key! "up" (lambda(t)(move-cursor! t 0 -1)))
	(set-key! "down" (lambda(t)(move-cursor! t 0 1)))
	(set-key! "end" move-cursor-to-the-end-of-line!)
	(set-key! "home" move-cursor-to-the-beginning-of-line!)
	(set-key! 
	 "f1" 
	 (lambda(t)
	   (call-with-new-thread 
	    (lambda()(display (eval-string (last-sexp t)) *stdout*)))))
	(set-key! "f2" (lambda(t)(display (last-sexp t) *stdout*)))
	(set-key! "backspace" delete-previous-char!)
	(set-key! "delete" delete-next-char!)
	))))
