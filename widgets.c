#include <SDL/SDL.h>
#include "extend.h"
#include "file.h"
#include "utils.h"

SCM stage;

extern char const _binary_scm_widgets_scm_start[];
//extern char const _binary_scm_widgets_scm_end[];
//extern size_t const _binary_scm_widgets_scm_size;

void widgets_init(Uint16 w, Uint16 h) {
  //file_eval("scm/widgets.scm");

  eval(_binary_scm_widgets_scm_start);
  evalf("(define *stage* (make <widget> #:w %i #:h %i))", w, h);
  eval("(define *active-widget* *stage*)");
 
  stage = eval("*stage*");
  
}
