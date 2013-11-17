#!./slayer
!#
(use-modules (slayer) (slayer image) 
	     (srfi srfi-1) (srfi srfi-2))
(keydn 'esc quit)

(define (list-directory path)
  (let ((dir (opendir path)))
    (unfold eof-object? (lambda(f)(string-append path f)) 
	    (lambda x (readdir dir)) (readdir dir))))

(define (file? f) (eq? 'regular (stat:type (stat f))))

(define *directory* (if (defined? '$1) $1 "/usr/share/pixmaps/"))
(define *image-names* (filter file? (list-directory *directory*)))
(define *current-image* #f)
(define *image-index* 0)

(if (<= (length *image-names*) 0)
    (begin (display "no images found\n") (quit)))

(set-display-procedure! (lambda () (draw-image! *current-image* 0 0)))

(define (show-image! i)
  (set! *image-index* (modulo i (length *image-names*)))
  (and-let* ((image-name (list-ref *image-names* *image-index*))
	     (image (load-image image-name)))
    (set! *current-image* image)
    (apply set-screen-size! (image-size *current-image*))
    (set-window-title! image-name)))

(show-image! 0)

(keydn 'left (lambda () (show-image! (+ *image-index* 1))))
(keydn 'right (lambda () (show-image! (- *image-index* 1))))
