#ifndef SLAYER_H
#define SLAYER_H
#include <SDL/SDL.h>
#include "utils.h"

#define TRY_SDL(f)						       \
  if((f) == -1) {						       \
    OUT("%s/%s,%d: '%s' failed: %s", __FILE__, __FUNCTION__, __LINE__, \
	STR(f),SDL_GetError());					       \
  }


#endif // SLAYER_H
