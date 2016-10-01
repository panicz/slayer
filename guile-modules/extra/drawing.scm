(define-module (extra drawing)
  #:use-module (extra drawing primitives)
  #:use-module (extra drawing operations)
  #:use-module (extra drawing parameters)
  #:re-export (shape?
	       bitmap?
	       space?
	       space
	       caption?
	       caption
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
	       above
	       current-background-color
	       current-font-size
	       current-text-color
	       current-font
	       current-text-background-color
	       current-font-slant
	       current-font-weight
	       ))

