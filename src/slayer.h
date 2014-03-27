#ifndef SLAYER_H
#define SLAYER_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#define COPYRIGHT "Copyright (C) 2013 Maciek Godek <godek.maciek@gmail.com>\n" \
  "\nLicense GPLv3+: GNU GPL 3 or later <http://gnu.org/licenses/gpl.html>.\n" \
  "This is free software: you are free to change and redistribute it.\n" \
  "There is NO WARRANTY, to the extent permitted by law."

#include <SDL/SDL.h>
#include "utils.h"
#include "extend.h"

#define TRY_SDL(f)						       \
  if((f) == -1) {						       \
    OUT("%s/%s,%d: '%s' failed: %s", __FILE__, __FUNCTION__, __LINE__, \
	_TOSTRING(f),SDL_GetError());				       \
  }

// SLAYER
#define SLAYER_SUFFIX ".scm"
DECLARE void *
remember_to_release(void *resource, void (*release)(void *));

#define REMEMBER_TO_RELEASE(resource, release)				\
  remember_to_release((void *) resource, (void (*)(void *)) release)

#define REMEMBER_TO_FREE(resource)		\
  remember_to_release(resource, free)


// AUDIO
#ifdef USE_SDL_MIXER
DECLARE void audio_init();
DECLARE void audio_finish();
#endif

// INPUT
DECLARE void input_init();
DECLARE SCM input_handle_events();
DECLARE SCM register_userevent(SCM handler);
DECLARE SCM generate_userevent(SCM code, SCM data1, SCM data2);

// TIMER
DECLARE void timer_init();

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
