#include <SDL/SDL_image.h>
#include "video.h"
#include "extend.h"
#include "utils.h"
#include "widgets.h"

#ifdef USE_OPENGL
#include "3d.h"
int video_mode = 0;
#endif

SDL_Surface *screen;

static SCM 
set_caption(SCM title) {
  char *str = scm_to_locale_string(title);
  SDL_WM_SetCaption(str, NULL);
  free(str);
  return title;
}

static SCM 
clear_screen() {
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    SDL_GL_SwapBuffers();
  } 
  else {
#endif
    SDL_FillRect(screen, NULL, 0);
    SDL_Flip(screen);
#ifdef USE_OPENGL
  }
#endif
  return SCM_UNSPECIFIED;
}

static SCM 
wipe_screen() {
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  }
  else {
#endif
    SDL_FillRect(screen, NULL, 0);
#ifdef USE_OPENGL
  }
#endif
  return SCM_UNSPECIFIED;
}

static SCM
flip_screen() {
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    SDL_GL_SwapBuffers();
  }
  else {
#endif
    SDL_Flip(screen);
#ifdef USE_OPENGL
  }
#endif
  return SCM_UNSPECIFIED;
}

static SCM display_procedure = (SCM) scm_noop;
static SCM
set_display_procedure_x(SCM procedure) {
  if(is_scm_procedure(procedure)) {
    display_procedure = procedure;
  }
  else {
    WARN("trying to set a non-procedure as the display procedure!");
  }
  return SCM_UNSPECIFIED;
}


static void 
export_symbols() {
  scm_c_define_gsubr("set-caption!", 1, 0, 0, 
		     (scm_t_subr) set_caption); 
  scm_c_define_gsubr("clear-screen", 0, 0, 0, 
		     (scm_t_subr) clear_screen);
  scm_c_define_gsubr("wipe-screen", 0, 0, 0, 
		     (scm_t_subr) wipe_screen);
  scm_c_define_gsubr("flip-screen", 0, 0, 0, 
		     (scm_t_subr) flip_screen);
  scm_c_define_gsubr("set-display-procedure!", 1, 0, 0, 
		     (scm_t_subr) set_display_procedure_x);
  
}

void
video_refresh_screen() {
  wipe_screen();
  scm_call_0(display_procedure);
  flip_screen();
}

void
video_init(Uint16 w, Uint16 h, int mode) {

  TRY_SDL(SDL_Init(SDL_INIT_VIDEO));

  screen = SDL_SetVideoMode(w, h, 0, mode);

  if(mode & SDL_OPENGL) {
#ifndef USE_OPENGL
    FATAL("slayer compiled without opengl support");
#else
    video_mode = mode;
    init_3d(w, h);
#endif
  }

  export_symbols();
}
