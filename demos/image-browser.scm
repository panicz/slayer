(use-modules (slayer) (slayer image) 
	     (ice-9 popen) (ice-9 rdelim) (ice-9 pretty-print)
	     (srfi srfi-1) (srfi srfi-2))
(keydn 'esc quit)

(define (list-directory directory)
  (let ((dir (opendir directory)))
    (unfold eof-object? (lambda(f)(string-append directory f)) 
	    (lambda x (readdir dir)) (readdir dir))))

(define (file? f) (eq? 'regular (stat:type (stat f))))

(define *image-names* (filter file? (list-directory "/usr/share/pixmaps/")))
(define *number-of-images* (length *image-names*))
(define *current-image* #f)
(define *image-index* 0)

(if (<= *number-of-images* 0)
    (die "no images found"))

(set-display-procedure! (lambda () (draw-image! *current-image* 0 0)))

(define (show-image! i)
  (set! *image-index* (modulo i *number-of-images*))
  (and-let* ((image-name (list-ref *image-names* *image-index*))
	     (image (load-image image-name)))
    (set! *current-image* image)
    (apply set-screen-size! (image-size *current-image*))
    (set-window-title! image-name)))

(show-image! 0)

(keydn 'left (lambda () (show-image! (+ *image-index* 1))))
(keydn 'right (lambda () (show-image! (- *image-index* 1))))
