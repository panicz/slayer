#ifndef SLAYER_H
#define SLAYER_H
#include <SDL/SDL.h>

static inline SDL_Rect sdl_rect(Sint16 x, Sint16 y, Uint16 w, Uint16 h) {
  SDL_Rect rect;
  rect.x = x;
  rect.y = y;
  rect.w = w;
  rect.h = h;
  return rect;
}


static inline SDL_Color sdl_color(Uint32 rgba) {
  SDL_Color c;
#if SDL_BYTEORDER == SDL_BIG_ENDIAN
  *((Uint32 *) &c) = rgba;  
#else
  c.r = 0xff & (rgba);
  c.g = 0xff & (rgba >> 8);
  c.b = 0xff & (rgba >> 16);
  c.unused = 0xff & (rgba >> 24);
#endif
  return c;
}


#endif // SLAYER_H
