#ifndef USEREVENT_H
#define USEREVENT_H

enum userevents {
  SDL_USEREVENT_TIMER = 0,
  //  SDL_USEREVENT_TYPING = 1,

  MAX_USEREVENTS = 256
};

extern SCM (*userevent[MAX_USEREVENTS])(SDL_Event *);

#endif // USEREVENT_H
