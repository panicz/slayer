#include <SDL/SDL.h>
#include "extend.h"
#include "file.h"

SCM stage;

void widgets_init(Uint16 w, Uint16 h) {
  file_eval("widgets.scm");
  evalf("(define *stage* (make <widget> #:w %i #:h %i))", w, h);
  eval("(define *active-widget* *stage*)");
 
  stage = eval("*stage*");
  
}
