#include <SDL/SDL_image.h>
#include "video.h"
#include "extend.h"
#include "utils.h"
#include "widgets.h"

SDL_Surface *screen;

static SCM set_caption(SCM title) {
  char *str = scm_to_locale_string(title);
  SDL_WM_SetCaption(str, NULL);
  free(str);
  return title;
}

static SCM clear_screen() {
  SDL_FillRect(screen, NULL, 0);
  SDL_Flip(screen);
  return SCM_UNSPECIFIED;
}

static void export_functions() {
  scm_c_define_gsubr("set-caption", 1, 0, 0, set_caption); 
  scm_c_define_gsubr("clear-screen", 0, 0, 0, clear_screen);
}



void video_refresh_screen() {
  eval("(draw *stage*)");
}

void video_init(Uint16 w, Uint16 h) {
  SDL_Init(SDL_INIT_VIDEO);
  screen = SDL_SetVideoMode(w, h, 0, SDL_HWSURFACE);
  export_functions();
}
