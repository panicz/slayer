#ifndef VIDEO_H
#define VIDEO_H

#include "extend.h"
#include <SDL/SDL.h>

#ifdef USE_OPENGL

#  include <GL/gl.h>
#  include <GL/glu.h>
#  include <GL/glext.h>

#  include "3d/extensions.m4.h"

#if SDL_BYTEORDER == SDL_BIG_ENDIAN

typedef enum COLOR_SHIFT {
  COLOR_SHIFT_BLUE = 24,
  COLOR_SHIFT_GREEN = 16,
  COLOR_SHIFT_RED = 8,
  COLOR_SHIFT_ALPHA = 0
} COLOR_SHIFT;

#else // SDL_LIL_ENDIAN

typedef enum COLOR_SHIFT {
  COLOR_SHIFT_BLUE = 0,
  COLOR_SHIFT_GREEN = 8,
  COLOR_SHIFT_RED = 16,
  COLOR_SHIFT_ALPHA = 24
} COLOR_SHIFT;

#endif // SDL_BYTEORDER

typedef enum COLOR_MASK {
  COLOR_MASK_RED = (0xff << COLOR_SHIFT_RED),
  COLOR_MASK_BLUE = (0xff << COLOR_SHIFT_BLUE),
  COLOR_MASK_GREEN = (0xff << COLOR_SHIFT_GREEN),
  COLOR_MASK_ALPHA = (0xff << COLOR_SHIFT_ALPHA)
} COLOR_MASK;

DECLARE void glWindowPos2i (GLint x, GLint y);

#define WITH_VERTEX_ARRAY(size, type, stride, pointer, action)	\
  glEnableClientState(GL_VERTEX_ARRAY);				\
  glVertexPointer(size, type, stride, pointer);			\
  action;							\
  glDisableClientState(GL_VERTEX_ARRAY);

#define WITH_TEXTURE_COORDS_ARRAY(size, type, stride, pointer, action)	\
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);				\
  glTexCoordPointer(size, type, stride, pointer);			\
  action;								\
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);

#endif // USE_OPENGL

#define WITH_LOCKED_SURFACE(surface, action)	\
  if(SDL_MUSTLOCK(surface)) {			\
    TRY_SDL(SDL_LockSurface(surface));		\
  }						\
  action;					\
  if(SDL_MUSTLOCK(surface)) {			\
    SDL_UnlockSurface(surface);			\
  }

#if defined(USE_OPENGL) && !defined(NDEBUG)
#define CHECK_OPENGL_ERROR(header)				\
  {								\
    GLenum error = glGetError();				\
    if(error != GL_NO_ERROR) {					\
      const unsigned char *message = gluErrorString(error);	\
      WARN(header "OpenGL error: %s", message);			\
    }								\
  }
#else // !defined(USE_OPENGL) || defined(NDEBUG)
#define CHECK_OPENGL_ERROR(message)
#endif // !defined(USE_OPENGL) || defined(NDEBUG)

#define RECKLESSLY(operation) operation

#ifndef NDEBUG
#define CAUTIOUSLY(operation)			\
  operation;					\
  CHECK_OPENGL_ERROR(# operation ": ")
#else // !NDEBUG
// THIS IS CRAZY!
#define CAUTIOUSLY RECKLESSLY
#endif // !NDEBUG

extern SDL_Surface *screen;

extern SCM current_video_output_fluid;
DECLARE SCM current_video_output();

DECLARE void video_refresh_screen();
DECLARE void video_init(Uint16 w, Uint16 h, int mode);

extern int video_mode;

static inline SDL_Surface *
sdl_surface(int w, int h, int BytesPerPixel) {
  return SDL_CreateRGBSurface(SDL_HWSURFACE | SDL_SRCALPHA,
			      w, h, 8*BytesPerPixel,
			      COLOR_MASK_RED,
			      COLOR_MASK_GREEN,
			      COLOR_MASK_BLUE,
			      COLOR_MASK_ALPHA);
}

DECLARE void flip_surface_vertically(SDL_Surface *);

static inline SDL_Rect 
sdl_rect(Sint16 x, Sint16 y, Uint16 w, Uint16 h) {
  SDL_Rect rect;
  rect.x = x;
  rect.y = y;
  rect.w = w;
  rect.h = h;
  return rect;
}

static inline SDL_Color 
sdl_color(Uint32 rgba) {
  SDL_Color c;
  c.b = 0xff & (rgba >> COLOR_SHIFT_BLUE);
  c.g = 0xff & (rgba >> COLOR_SHIFT_GREEN);
  c.r = 0xff & (rgba >> COLOR_SHIFT_RED);
  c.unused = 0xff & (rgba >> COLOR_SHIFT_ALPHA);
  return c;
}

static inline Uint32
rgba_color(SDL_Color c) {
  return (c.b << COLOR_SHIFT_BLUE)
    | (c.g << COLOR_SHIFT_GREEN)
    | (c.r << COLOR_SHIFT_RED)
    | (c.unused << COLOR_SHIFT_ALPHA);
}

#endif // VIDEO_H
