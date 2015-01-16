#ifndef INPUT_H
#define INPUT_H

#define SDL_NBUTTONS 12
//#define PROVIDE_KEY_BINDINGS_ACCESSORS

// in miliseconds
#define MAX_PROCESS_HANDLING_TIME 1000

enum input_modes {
  DIRECT_MODE = 0,
  TYPING_MODE = 1
};

enum key_bindings_indices {
  KEY_BINDINGS_UP = 0,
  KEY_BINDINGS_DOWN = 1,
  KEY_BINDINGS_MOUSEMOVE = 2
};

#define DisableUNICODE() SDL_EnableUNICODE(0)
#define DisableKeyRepeat() SDL_EnableKeyRepeat(0, 0)
#define EnableUNICODE() SDL_EnableUNICODE(1)
#define EnableDefaultKeyRepeat()					\
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL)

#endif // INPUT_H
