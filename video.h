#ifndef VIDEO_H
#define VIDEO_H

#include "extend.h"
#include <SDL/SDL.h>
extern SDL_Surface *screen;

extern void video_refresh_screen();
extern void video_init(Uint16 w, Uint16 h);


#endif // VIDEO_H
