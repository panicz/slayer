#include <SDL/SDL.h>
#include "extend.h"
#include "file.h"
#include "utils.h"
#include "video.h"

SCM stage;
SCM input_widget;
SCM active_widget;
SCM nearby_widget;

extern char const _binary_scm_defs_scm_start[];
extern char const _binary_scm_widgets_scm_start[];
extern char const _binary_scm_3d_scm_start[];

extern char const _binary_scm_defs_scm_end[];
extern char const _binary_scm_widgets_scm_end[];
extern char const _binary_scm_3d_scm_end[];

extern size_t const _binary_scm_defs_scm_size;
extern size_t const _binary_scm_widgets_scm_size;
extern size_t const _binary_scm_3d_scm_size;

void widgets_init(Uint16 w, Uint16 h) {

  //OUT("widgets.scm: %d", ()-strlen(_binary_scm_widgets_scm_start));
  char *code;
  size_t size;

#define EVAL(name)				\
  size = _binary_ ## name ## _end - _binary_ ## name ## _start; \
  code = malloc(size+1); \
  memcpy(code, _binary_ ## name ## _start, size); \
  code[size] = 0; \
  eval(code); \
  free(code);

  EVAL(scm_defs_scm);

  EVAL(scm_widgets_scm);
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    EVAL(scm_3d_scm);
  }
#endif
#undef EVAL

  evalf("(define *stage* (make <widget> #:w %i #:h %i))", w, h); 
  stage = eval("*stage*");

  eval("(define *input-widget* #f)");
  eval("(define *active-widget* *stage*)");
  eval("(define *nearby-widget* #f)");

}
