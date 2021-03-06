#ifndef VIDEO_H
#define VIDEO_H

#include "extend.h"
#include <SDL/SDL.h>

#ifdef USE_OPENGL

#  include <GL/gl.h>
#  include <GL/glu.h>
#  include <GL/glext.h>

#  include "3d/extensions.m4.h"

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

#if defined(USE_OPENGL) && !defined(NDEBUG)
#define CHECK_OPENGL_ERROR(header)				\
  {								\
    GLenum error = glGetError();				\
    if(error != GL_NO_ERROR) {					\
      const unsigned char *message = gluErrorString(error);	\
      WARN(header "OpenGL error: %s", message);			\
    }								\
  }
#else
#define CHECK_OPENGL_ERROR(message)
#endif

#define RECKLESSLY(operation) operation

#ifndef NDEBUG
#define CAUTIOUSLY(operation)			\
  operation;					\
  CHECK_OPENGL_ERROR(# operation ": ")
#else
// THIS IS CRAZY!
#define CAUTIOUSLY RECKLESSLY
#endif


extern SDL_Surface *screen;

extern SCM current_video_output_fluid;
DECLARE SCM current_video_output();

DECLARE void video_refresh_screen();
DECLARE void video_init(Uint16 w, Uint16 h, int mode);

extern int video_mode;

static inline SDL_Surface *
sdl_surface(int w, int h, int BytesPerPixel) {
  Uint32 r, g, b, a;
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
  r = 0xff000000;
  g = 0x00ff0000;
  b = 0x0000ff00;
  a = 0x000000ff;
#else
  r = 0x000000ff;
  g = 0x0000ff00;
  b = 0x00ff0000;
  a = 0xff000000;
#endif
  return SDL_CreateRGBSurface(SDL_HWSURFACE | SDL_SRCALPHA,
			      w, h, 8*BytesPerPixel, r, g, b, a);
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
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
  *((Uint32 *) &c) = rgba;  
#else
  c.b = 0xff & (rgba);
  c.g = 0xff & (rgba >> 8);
  c.r = 0xff & (rgba >> 16);
  c.unused = 0xff & (rgba >> 24);
#endif
  return c;
}

static inline Uint32
rgba_color(SDL_Color c) {
  Uint32 rgba;
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
  rgba = *((Uint32 *) &c);  
#else
  rgba = c.b | (c.g << 8) | (c.r << 16) | (c.unused << 24);
#endif
  return rgba;
}

#endif // VIDEO_H
