(use-modules (slayer) (slayer image) (extra common))
(keydn 'esc quit)

(define *image-names* (shell "ls ../demos/art/*.png"))
(define *number-of-images* (length *image-names*))
(define *current-image* #f)
(define *image-index* 0)

(if (<= *number-of-images* 0)
    (die "no images found"))

(set-display-procedure! (lambda () (draw-image! *current-image* 0 0)))

(define (show-image! i)
  (set! *image-index* (modulo i *number-of-images*))
  (let ((image-name (list-ref *image-names* *image-index*)))
    (set! *current-image* (load-image image-name))
    (apply set-screen-size! (image-size *current-image*))
    (set-caption! image-name)))

(show-image! 0)

(keydn 'left (lambda () (show-image! (+ *image-index* 1))))
(keydn 'right (lambda () (show-image! (- *image-index* 1))))
