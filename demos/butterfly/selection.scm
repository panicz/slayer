(define-module (butterfly selection)
  ;;#:use-module (ice-9 nice-9)
  ;;#:use-module (butterfly cursor)
  #:export (current-selection))

(define current-selection (make-parameter '()))
