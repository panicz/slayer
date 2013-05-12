#include "slayer.h"
#include "video.h"
#include "extend.h"
#include "utils.h"
#include "symbols.h"

#ifdef USE_OPENGL
int video_mode = 0;
#endif

SDL_Surface *screen;

static SCM 
clear_screen() {
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    SDL_GL_SwapBuffers();
    return SCM_UNSPECIFIED;
  } 
#endif
  SDL_FillRect(screen, NULL, 0);
  SDL_Flip(screen);
  return SCM_UNSPECIFIED;
}

static SCM 
wipe_screen() {
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    return SCM_UNSPECIFIED;
  }
#endif
  SDL_FillRect(screen, NULL, 0);
  return SCM_UNSPECIFIED;
}

static SCM
flip_screen() {
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    SDL_GL_SwapBuffers();
    return SCM_UNSPECIFIED;
  }
#endif
  SDL_Flip(screen);
  return SCM_UNSPECIFIED;
}

static SCM display_procedure;
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

static inline SCM
screen_width() {
  return scm_from_int(screen->w);
}

static inline SCM
screen_height() {
  return scm_from_int(screen->h);
}

static SCM
screen_size() {
  return scm_list_2(screen_width(), screen_height());
}

static void 
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);

  EXPORT_PROCEDURE("screen-width", 0, 0, 0, screen_width); 
  EXPORT_PROCEDURE("screen-height", 0, 0, 0, screen_height); 
  EXPORT_PROCEDURE("screen-size", 0, 0, 0, screen_size); 
  EXPORT_PROCEDURE("clear-screen", 0, 0, 0, clear_screen);
  EXPORT_PROCEDURE("wipe-screen", 0, 0, 0, wipe_screen);
  EXPORT_PROCEDURE("flip-screen", 0, 0, 0, flip_screen);
  EXPORT_PROCEDURE("set-display-procedure!", 1, 0, 0, 
		   set_display_procedure_x);

#undef EXPORT_PROCEDURE

#ifdef USE_OPENGL
  eval("(cond-expand-provide (current-module) '(slayer-3d-available))");
  if(video_mode & SDL_OPENGL) {
    eval("(cond-expand-provide (current-module) '(slayer-3d))");
  }
#endif

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
  display_procedure = noop;

  scm_c_define_module("slayer", export_symbols, NULL);
}