#include "slayer.h"
#include "video.h"
#include "extend.h"
#include "utils.h"
#include "symbols.h"

int video_mode = 0;
SDL_Surface *screen = NULL;

static SCM 
clear_screen_x() {
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    SDL_GL_SwapBuffers();
    return SCM_UNSPECIFIED;
  } 
#endif // USE_OPENGL
  SDL_FillRect(screen, NULL, 0);
  SDL_Flip(screen);
  return SCM_UNSPECIFIED;
}

static SCM 
wipe_screen_x() {
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    return SCM_UNSPECIFIED;
  }
#endif // USE_OPENGL
  SDL_FillRect(screen, NULL, 0);
  return SCM_UNSPECIFIED;
}

static SCM
flip_screen_x() {
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    SDL_GL_SwapBuffers();
    return SCM_UNSPECIFIED;
  }
#endif // USE_OPENGL
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

static SCM 
set_screen_size_x(SCM W, SCM H) {
  int w = scm_to_int(W);
  int h = scm_to_int(H);
  screen = SDL_SetVideoMode(w, h, screen->format->BitsPerPixel, video_mode);
#if HAVE_SDL_SETMOUSERANGE
  SDL_SetMouseRange((Uint16) w, (Uint16) h);
#endif // HAVE_SDL_SETMOUSERANGE
  return SCM_UNSPECIFIED;
}

static SCM
set_screen_width_x(SCM W) {
  int w = scm_to_int(W);
  int h = screen ? screen->h : 1;
  screen = SDL_SetVideoMode(w, h, screen->format->BitsPerPixel, video_mode);
#if HAVE_SDL_SETMOUSERANGE
  SDL_SetMouseRange((Uint16) w, (Uint16) h);
#endif // HAVE_SDL_SETMOUSERANGE
  return SCM_UNSPECIFIED;
  
}

static SCM
set_screen_height_x(SCM H) {
  int w = screen ? screen->w : 1;
  int h = scm_to_int(H);
  screen = SDL_SetVideoMode(w, h, screen->format->BitsPerPixel, video_mode);
#if HAVE_SDL_SETMOUSERANGE
  SDL_SetMouseRange((Uint16) w, (Uint16) h);
#endif // HAVE_SDL_SETMOUSERANGE
  return SCM_UNSPECIFIED;
}

static SCM
set_screen_color_depth_x(SCM bits_per_pixel) {
  int w = screen ? screen->w : 1;
  int h = screen ? screen->h : 1;
  screen = SDL_SetVideoMode(w, h, scm_to_int(bits_per_pixel), video_mode);
  return SCM_UNSPECIFIED;
}

static SCM
screen_color_depth() {
  return scm_from_int(screen->format->BitsPerPixel);
}

static void 
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);

  EXPORT_PROCEDURE("set-screen-width!", 1, 0, 0, set_screen_width_x);
  EXPORT_PROCEDURE("set-screen-height!", 1, 0, 0, set_screen_height_x);
  EXPORT_PROCEDURE("set-screen-size!", 2, 0, 0, set_screen_size_x);
  EXPORT_PROCEDURE("set-screen-color-depth!", 1, 0, 0, set_screen_color_depth_x);
  EXPORT_PROCEDURE("screen-color-depth", 0, 0, 0, screen_color_depth);
  EXPORT_PROCEDURE("screen-width", 0, 0, 0, screen_width);
  EXPORT_PROCEDURE("screen-height", 0, 0, 0, screen_height);
  EXPORT_PROCEDURE("screen-size", 0, 0, 0, screen_size);
  EXPORT_PROCEDURE("clear-screen!", 0, 0, 0, clear_screen_x);
  EXPORT_PROCEDURE("wipe-screen!", 0, 0, 0, wipe_screen_x);
  EXPORT_PROCEDURE("flip-screen!", 0, 0, 0, flip_screen_x);
  EXPORT_PROCEDURE("set-display-procedure!", 1, 0, 0, 
		   set_display_procedure_x);

#undef EXPORT_PROCEDURE

#ifdef USE_OPENGL
  cond_expand_provide("slayer-3d-available");
  if(video_mode & SDL_OPENGL) {
    cond_expand_provide("slayer-3d");
  }
#endif // USE_OPENGL

}

void
flip_surface_vertically(SDL_Surface *s) {
  int bpp = s->format->BytesPerPixel;
  SDL_Surface *copy = sdl_surface(s->w, s->h, bpp);
  int line_size = s->w * bpp;
  Uint8 *s_pixels = s->pixels;
  Uint8 *copy_pixels = copy->pixels;
  int i;
  SDL_LockSurface(s);
  SDL_LockSurface(copy);

  for(i = 0; i < s->h; ++i) {
    memcpy((copy_pixels + (s->h - i - 1)*line_size),
	   (s_pixels + i*line_size), line_size);
  }
  memcpy(s_pixels, copy_pixels, line_size*s->h);

  SDL_UnlockSurface(copy);
  SDL_UnlockSurface(s);

  SDL_FreeSurface(copy);
}

void
video_refresh_screen() {
  wipe_screen_x();
  scm_call_0(display_procedure);
  flip_screen_x();
}

void
video_init(Uint16 w, Uint16 h, int mode) {

  TRY_SDL(SDL_Init(SDL_INIT_VIDEO));

  screen = SDL_SetVideoMode(w, h, 0, mode);
  video_mode = mode;

  if(mode & SDL_OPENGL) {
#ifndef USE_OPENGL
    FATAL("slayer compiled without OpenGL support");
#else // !USE_OPENGL
    init_3d();
#endif // !USE_OPENGL
  }

  display_procedure = noop;

  scm_c_define_module("slayer", export_symbols, NULL);
}
