#include <SDL/SDL.h>
#include <SDL/SDL_thread.h>
#include "extend.h"
#include "userevent.h"

typedef struct {
  int period;
} timer_thread_data;

static int timer_thread(timer_thread_data *timer) {
  SDL_Event timer_event;
  timer_event.type = SDL_USEREVENT;
  timer_event.user.code = SDL_USEREVENT_TIMER;
  timer_event.user.data1 = timer;
  timer_event.user.data2 = NULL;
  Uint32 period = timer->period;

  while(1) {
    SDL_Delay(period);
    SDL_PeepEvents(&timer_event, 1, SDL_ADDEVENT, 0xffffffff);
  }
}

SCM get_ticks() {
  return scm_from_uint32(SDL_GetTicks());
}

static void export_functions() {
  scm_c_define_gsubr("get-ticks", 0, 0, 0, get_ticks);
  
}

void timer_init() {
  export_functions();

}
