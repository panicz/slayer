#ifndef VIDEO_H
#define VIDEO_H

#include "extend.h"
#include <SDL/SDL.h>

#ifdef USE_OPENGL
#include <GL/gl.h>
#include <GL/glu.h>
#endif

extern SDL_Surface *screen;

extern void video_refresh_screen();
extern void video_init(Uint16 w, Uint16 h, int mode);

#ifdef USE_OPENGL
extern int video_mode;
#endif


#endif // VIDEO_H
