#ifndef INPUT_H
#define INPUT_H

#include "extend.h"
#include <SDL/SDL.h>

#define SDL_NBUTTONS 6

extern SCM keydown[SDLK_LAST + SDL_NBUTTONS];
extern SCM keyup[SDLK_LAST + SDL_NBUTTONS];
extern SCM mousemove;

extern SCM (*event_handler[SDL_NUMEVENTS])(SDL_Event *);

extern SCM scancodes; // hash table containing keynames and their scancodes
extern SCM key_names;

/** initialize input subsystem */
void input_init();

/** use the input subsystem to handle events */
extern SCM input_handle_events();

void bind_key(SCM *keytab, SDLKey key, SCM function);
extern SCM bind_keydown(SCM key, SCM function);
extern SCM bind_keyup(SCM key, SCM function);
extern SCM bind_mousemove(SCM function);
extern SCM input_grab(SCM onoff);

enum input_modes {
  DIRECT_MODE = 0,
  TYPING_MODE = 1
};

extern enum input_modes input_mode;


#endif /* INPUT_H */
