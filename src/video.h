#ifndef VIDEO_H
#define VIDEO_H

#include "extend.h"
#include <SDL/SDL.h>

#ifdef USE_OPENGL

/**
(define-template (OPENGL_EXTENSION NAME FUNCTION TYPE) "
#  ifndef HAVE_<NAME>
#    ifdef HAVE_<NAME>_ARB
#      define <FUNCTION> <FUNCTION>ARB
#    else 
#      ifdef HAVE_<NAME>_EXT
#        define <FUNCTION> <FUNCTION>EXT
#      else
#        define NO_<NAME>
#      endif
#    endif 
#  endif

#  ifdef NO_<NAME>
DECLARE void (*<FUNCTION>)<TYPE>;
#  endif
")
**/

/**
(OPENGL_EXTENSION GL_GEN_FRAMEBUFFERS glGenFramebuffers "(GLsizei, GLuint *)")
**/
#  ifndef HAVE_GL_GEN_FRAMEBUFFERS
#    ifdef HAVE_GL_GEN_FRAMEBUFFERS_ARB
#      define glGenFramebuffers glGenFramebuffersARB
#    else 
#      ifdef HAVE_GL_GEN_FRAMEBUFFERS_EXT
#        define glGenFramebuffers glGenFramebuffersEXT
#      else
#        define NO_GL_GEN_FRAMEBUFFERS
#      endif
#    endif 
#  endif

#  ifdef NO_GL_GEN_FRAMEBUFFERS
DECLARE void (*glGenFramebuffers)(GLsizei, GLuint *);
#  endif

/**
(OPENGL_EXTENSION GL_GEN_RENDERBUFFERS glGenRenderbuffers "(GLsizei, GLuint *)")
**/
#  ifndef HAVE_GL_GEN_RENDERBUFFERS
#    ifdef HAVE_GL_GEN_RENDERBUFFERS_ARB
#      define glGenRenderbuffers glGenRenderbuffersARB
#    else 
#      ifdef HAVE_GL_GEN_RENDERBUFFERS_EXT
#        define glGenRenderbuffers glGenRenderbuffersEXT
#      else
#        define NO_GL_GEN_RENDERBUFFERS
#      endif
#    endif 
#  endif

#  ifdef NO_GL_GEN_RENDERBUFFERS
DECLARE void (*glGenRenderbuffers)(GLsizei, GLuint *);
#  endif

#  if defined(NO_GL_GEN_FRAMEBUFFERS) && HAVE_GL_EXTENSION_WRANGLER
#    include <GL/glew.h>
#  else
#    include <GL/gl.h>
#    include <GL/glu.h>
#    include <GL/glext.h>
#  endif

DECLARE void glWindowPos2i (GLint x, GLint y);

#endif // USE_OPENGL

extern SDL_Surface *screen;

extern SCM current_screen_fluid;
DECLARE SCM current_screen();

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
