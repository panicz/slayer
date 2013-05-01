#ifndef SLAYER_H
#define SLAYER_H
#include <SDL/SDL.h>
#include "utils.h"

static inline SDL_Surface *sdl_surface(int w, int h, 
				       int BytesPerPixel) {
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
  c.b = 0xff & (rgba);
  c.g = 0xff & (rgba >> 8);
  c.r = 0xff & (rgba >> 16);
  c.unused = 0xff & (rgba >> 24);
#endif
  return c;
}

static inline void print_sdl_surface(SDL_Surface *surface) {
  OUT("struct SDL_Surface <%p> {", surface);
  Uint32 flags = surface->flags;
  OUT_("\tUint32 flags = 0");
#define FLAG(flag) if(flags & flag) OUT_("\n\t\t     | " # flag);
  FLAG(SDL_SWSURFACE);
  FLAG(SDL_HWSURFACE);
  FLAG(SDL_ASYNCBLIT);
  FLAG(SDL_ANYFORMAT);
  FLAG(SDL_HWPALETTE);
  FLAG(SDL_DOUBLEBUF);
  FLAG(SDL_FULLSCREEN);
  FLAG(SDL_OPENGL);
  FLAG(SDL_OPENGLBLIT);
  FLAG(SDL_RESIZABLE);
  FLAG(SDL_HWACCEL);
  FLAG(SDL_SRCCOLORKEY);
  FLAG(SDL_RLEACCEL);
  FLAG(SDL_SRCALPHA);
  FLAG(SDL_PREALLOC);
#undef FLAG
  OUT(";");

  if(surface->format) {
    OUT("\tSDL_PixelFormat *format <%p> = {", surface->format);
    if(surface->format->palette) {
      OUT("\t\tSDL_Palette *palette <%p> = {", surface->format->palette);
      OUT("\t\t\tint ncolors = %d;", surface->format->palette->ncolors);
      int i;
      OUT("\t\t\tSDL_Color *colors = {");
      for (i = 0; i < surface->format->palette->ncolors; ++i) {
	//Uint8 r, g, b, a;
	//SDL_Color c = surface->format->palette->colors[i];
	//OUT("\t\t\t\t%d: red = 0x%02x, green = 0x%02x, blue = 0x%02x, alpha = 0x%02x", i, c.r, c.g, c.b, c.unused);
      }
      OUT("\t\t\t\t...");
      OUT("\t\t\t}");
      //      OUT("\t\t\t...");
      OUT("\t\t}");
    } else {
      OUT("\t\tSDL_Palette *palette = NULL;");
    }
    OUT("\t\tUint8 BitsPerPixel = %d;", surface->format->BitsPerPixel);
    OUT("\t\tUint8 BytesPerPixel = %d;", surface->format->BytesPerPixel);
    OUT("\t\tUint8 Rloss = 0x%x, Gloss = 0x%x, Bloss = 0x%x, Aloss = 0x%x;", 
	surface->format->Rloss, surface->format->Gloss, 
	surface->format->Bloss, surface->format->Aloss);
    OUT("\t\tUint8 Rshift = 0x%x, Gshift = 0x%x, Bshift = 0x%x, Ashift = 0x%x;", 
	surface->format->Rshift, surface->format->Gshift, 
	surface->format->Bshift, surface->format->Ashift);
    OUT("\t\tUint32 Rmask = 0x%x, Gmask = 0x%x, Bmask = 0x%x, Amask = 0x%x;", 
	surface->format->Rmask, surface->format->Gmask, 
	surface->format->Bmask, surface->format->Amask);
    OUT("\t\tUint32 colorkey = 0x%x;", surface->format->colorkey);
    OUT("\t\tUint8 alpha = 0x%x;", surface->format->alpha);
    OUT("\t}");
  }
  else {
    OUT("\tSDL_PixelFormat *format = NULL;");
  }
  OUT("\tint w = %d, h = %d;", surface->w, surface->h);
  OUT("\tUint16 pitch = %d;", surface->pitch);
  OUT("\tSDL_Rect clip_rect = { x = %d, y = %d, w = %d, h = %d };", 
      surface->clip_rect.x, surface->clip_rect.y, surface->clip_rect.w, surface->clip_rect.h);
  OUT("\tint refcount = %d;", surface->refcount);
  OUT("}");
}


#endif // SLAYER_H
