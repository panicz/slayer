#ifndef SLAYER_H
#define SLAYER_H

#ifdef HAVE_CONFIG_H
#include "config.h"
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
DECLARE void audio_init();
DECLARE void audio_finish();
#endif

// INPUT
DECLARE void input_init();
DECLARE SCM input_handle_events();

// FILE
DECLARE int file_exists(const char *filename);
DECLARE int file_empty(const char *filename);
DECLARE int file_writable(const char *filename);
DECLARE int file_readable(const char *filename);

DECLARE int file_create(const char *filename);
DECLARE int file_write(const char *filename, const char *string);
DECLARE SCM file_eval(const char *filename);

// IMAGE
extern scm_t_bits image_tag;
DECLARE void image_init();
DECLARE SCM rectangle(SCM w, SCM h, SCM color, SCM BytesPerPixel);

// FONT
extern scm_t_bits font_tag;
DECLARE void font_init();

// 3D
#ifdef USE_OPENGL
DECLARE void init_3d();
#endif

#endif // SLAYER_H
