(define-module (butterfly categories)
  #:use-module (extra registry)
  #:use-module (grand scheme)
  #:use-module (butterfly cursor)
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

(define editor-state-category
  (registry-category
   #:document list?
   #:cursor (default cursor? 0)
   #:selection (default (lambda (x) (and (list? x) (every cursor? x))) '())
   #:edited-expression (maybe (registry-entry? edited-expression-category))))
