(define-module (libs ode)
  #:export (force! torque!))
(load-extension "libguile-ode" "ode_init")

(define* (force! body force #:key (local #f) (at #f) (relative #f))
  (if local
      (if relative
	  (if at 
	      (add-body-local-force-at-relative-position! body force at)
	      (add-body-local-force! body force))
	  (if at
	      (add-body-local-force-at-position! body force at)
	      (add-body-local-force! body force)))
      (if relative
	  (if at 
	      (add-body-force-at-relative-position! body force at)
	      (add-body-force! body force))
	  (if at
	      (add-body-force-at-position! body force at)
	      (add-body-force! body force)))))

(define* (torque! body torque #:key (local #f))
  (if local 
      (add-body-local-torque! body torque)
      (add-body-torque! body torque)))
