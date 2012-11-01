#ifndef USEREVENT_H
#define USEREVENT_H

enum userevents {

  MAX_USEREVENTS = 256
};

extern SCM (*userevent[MAX_USEREVENTS])(SDL_Event *);

#endif // USEREVENT_H
