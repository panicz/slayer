#include <SDL/SDL_image.h>
#include "video.h"
#include "extend.h"
#include "utils.h"
#include "widgets.h"

#ifdef USE_OPENGL
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

    glBegin(GL_POINTS);
    glVertex2i(1,1);
    glEnd();

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

    glBegin(GL_POINTS);
    glVertex2i(1,1);
    glEnd();
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

    glBegin(GL_POINTS);
    glVertex2i(0,0);
    glEnd();

  }
  else {
#endif
    SDL_Flip(screen);
#ifdef USE_OPENGL
  }
#endif
  return SCM_UNSPECIFIED;
}

static void 
export_symbols() {
  scm_c_define_gsubr("set-caption!", 1, 0, 0, set_caption); 
  scm_c_define_gsubr("clear-screen", 0, 0, 0, clear_screen);
  scm_c_define_gsubr("wipe-screen", 0, 0, 0, wipe_screen);
  scm_c_define_gsubr("flip-screen", 0, 0, 0, flip_screen);
#ifdef USE_OPENGL
  scm_c_define("*use-opengl*", SCM_BOOL_T);
#endif
}

void
video_refresh_screen() {
  wipe_screen();
  eval("(draw *stage*)");
  flip_screen();
}

void
video_init(Uint16 w, Uint16 h, int mode) {

  TRY_SDL(SDL_Init(SDL_INIT_VIDEO));

  if(mode & SDL_OPENGL) {
#ifndef USE_OPENGL
    FATAL("slayer compiled without opengl support");
#else
    video_mode = mode;
    SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_TRUE);
    glClearColor(0,0,0,0);
    glShadeModel(GL_SMOOTH);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glDisable(GL_COLOR_MATERIAL);
    glViewport(0, 0, (GLsizei) w, (GLsizei) h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glWindowPos2i(0, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
#endif
  }
  screen = SDL_SetVideoMode(w, h, 0, mode);
  export_symbols();
}
