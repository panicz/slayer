(define-module (widgets image-clipper)
  #:use-module (oop goops)
  #:use-module (extra ref)
  #:use-module (extra common)
  #:use-module (slayer)
  #:use-module (slayer image)
  #:use-module (widgets base)
  #:export (<image-clipper>
	    )
  #:re-export (make)
  )

;; an image-clipper is a widget that shows only a (cropped) part of an
;; image, but allows to decide which one

;;  < - - - - - - - - W - - - - - - - - >
;; +-------------------------------------+
;; |           ^                         | ^
;; |           T                         | :
;; |           v                         | :
;; |       +-----------------+           | :
;; |       |< - - w - - ^ - >|           | :
;; | < L > |            :    |           | :
;; |       |            h    |   < R >   | H
;; |       |            :    |     =     | :
;; |       | clipper    v    |   W-L-w   | :
;; |       +-----------------+           | :
;; |              ^                      | :
;; |              B = H-T-h              | :
;; |  image       v                      | v
;; +-------------------------------------+

(define-class <image-clipper> (<widget>)
  (%image #:init-value (rectangle 1 1 0) #:init-keyword #:image)
  (%cropped-image #:init-value #f)
  (%left #:init-value 0 #:init-keyword #:left)
  (%top #:init-value 0 #:init-keyword #:top)
  (image
   #:allocation #:virtual
   #:slot-ref (lambda (self) #[self '%image])
   #:slot-set!
   (lambda (self image)
     (set! #[self '%image] image)
     (set! #[self '%cropped-image] #f)))
  (left
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     #[self '%left])
   #:slot-set!
   (lambda (self left)
     (set! #[self '%left] left)
     (set! #[self '%cropped-image] #f)))
  (top
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     #[self '%top])
   #:slot-set!
   (lambda (self top)
     (set! #[self '%top] top)
     (set! #[self '%cropped-image] #f)))
  (right
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (- #[self 'w] #[self 'left] (image-width #[self 'image])))
   #:slot-set!
   (lambda (self right)
     (set! #[self 'left] (- #[self 'w] right (image-width #[self 'image])))))
  (bottom
   #:allocation #:virtual
   #:slot-ref
   (lambda (self)
     (- #[self 'h] #[self 'top] (image-height #[self 'image])))
   #:slot-set!
   (lambda (self bottom)
     (set! #[self 'top] (- #[self 'h] bottom (image-height #[self 'image]))))))

(define-method (initialize (c <image-clipper>) args)
  (next-method)
  (default-slot-values c args
    (drag (lambda (x y dx dy)
	    ;; zaimplementować stopa!
	    (set! #[c 'left] 
		  ((clamp 0 (- (image-width #[c 'image]) #[c 'w]))
		   (- #[c 'left] dx)))
	    (set! #[c 'top] 
		  ((clamp 0 (- (image-height #[c 'image]) #[c 'h])) 
		   (- #[c 'top] dy)))))))

(define-method (draw (c <image-clipper>))
  (unless #[c '%cropped-image]
    (set! #[c '%cropped-image] (crop-image #[c '%image] #[c 'left] #[c 'top]
					   #[c 'w] #[c 'h])))
  (draw-image! #[c '%cropped-image] #[c 'x] #[c 'y]))
