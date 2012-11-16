(define-module (extra slayer)
  #:export (;; slayer.c
	    set-exit-procedure!
	    ;; video.c
	    set-caption!
	    clear-screen
	    wipe-screen
	    flip-screen
	    set-display-procedure!
	    ;; image.c
	    rectangle
	    load-image
	    draw-image
	    image-width
	    image-height
	    image-size
	    image->array
	    array->image
	    ;; font.c
	    load-font
	    render-text
	    set-font-style!
	    font-line-skip
	    ;; input.c
	    handle-input
	    grab-input
	    keydn keyup mousemove
	    input-mode
	    key-bindings
	    mousemove-binding
	    get-ticks
	    generate-userevent
	    register-userevent
	    ))
