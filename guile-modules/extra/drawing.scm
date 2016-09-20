(define-module (extra drawing)
  #:use-module (extra drawing primitives)
  #:use-module (extra drawing operations)
  #:re-export (shape?
	       bitmap?
	       space?
	       caption?
	       drawing?
	       group?
	       group
	       draw!
	       displaced
	       extents
	       dimensions
	       width
	       height
	       overlay
	       beside
	       below
	       above))

