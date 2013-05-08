#ifndef SLAYER_H
#define SLAYER_H

#ifdef HAVE_CONFIG_H
#  include "config.h"

#  if defined HAVE_LIBGL && defined HAVE_LIBGLU 
#    define USE_OPENGL
#  endif

#  ifdef HAVE_LIBSDL_MIXER
#    define USE_SDL_MIXER
#  endif

#endif

#include <SDL/SDL.h>
#include "utils.h"
#include "extend.h"

#define TRY_SDL(f)						       \
  if((f) == -1) {						       \
    OUT("%s/%s,%d: '%s' failed: %s", __FILE__, __FUNCTION__, __LINE__, \
	STR(f),SDL_GetError());					       \
  }

// AUDIO
#ifdef USE_SDL_MIXER
extern void audio_init();
extern void audio_finish();
#endif

// INPUT
extern void input_init();
extern SCM input_handle_events();

// FILE
int file_exists(const char *filename);
int file_empty(const char *filename);
int file_writable(const char *filename);
int file_readable(const char *filename);

int file_create(const char *filename);
int file_write(const char *filename, const char *string);
SCM file_eval(const char *filename);

// IMAGE
extern scm_t_bits image_tag;
extern void image_init();
extern SCM rectangle(SCM w, SCM h, SCM color, SCM BytesPerPixel);

// FONT
extern scm_t_bits font_tag;
void font_init();

// 3D
#ifdef USE_OPENGL
extern void init_3d();
#endif

#endif // SLAYER_H
