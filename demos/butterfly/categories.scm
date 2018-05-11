(define-module (butterfly categories)
  #:use-module (extra registry)
  #:use-module (grand scheme)
  #:use-module (butterfly cursor)
  #:use-module (slayer)
  #:export (edited-expression-category
	    editor-state-category))

(define edited-expression-category
  (registry-category
   #:value string?
   #:cursor natural?
   #:selection (maybe (lambda (x)
			(and-let* (((a . b) x)
				   ((number? a))
				   ((number? b))))))))



(define edited-expression (edited-expression-category))

((registry-entry? edited-expression-category) edited-expression)


(define editor-state-category
  (registry-category
   #:document list?
   #:cursor (default cursor? 0)
   #:selection (default (lambda (x) (and (list? x) (every cursor? x))) '())
   #:dragged (maybe cursor?)
   ;;#:edited-expression (maybe (registry-entry? edited-expression-category))
   #:window-size (default (lambda (x)
			    (and-let* (((width height) x)
				       ((integer? width))
				       ((integer? height))
				       ((is width >= 0))
				       ((is height >= 0)))))
		   (screen-size))))
