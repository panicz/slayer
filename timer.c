#include <SDL/SDL.h>
#include <SDL/SDL_thread.h>
#include "extend.h"

SCM get_ticks() {
  return scm_from_uint32(SDL_GetTicks());
}

static void export_functions() {
  scm_c_define_gsubr("get-ticks", 0, 0, 0, get_ticks);
}

void timer_init() {
  export_functions();

}
