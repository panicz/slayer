#include <SDL/SDL.h>
#include <SDL/SDL_thread.h>
#include "extend.h"
#include "userevent.h"
#include "timer.h"
#include "utils.h"

typedef struct {
  int period;
  SCM actions;
} timer_thread_data;


static int timer_thread(timer_thread_data *timer) {
  SDL_Event timer_event;
  Uint32 period = timer->period;

  while(1) {
    timer_event.type = SDL_USEREVENT;
    timer_event.user.code = SDL_USEREVENT_TIMER;
    timer_event.user.data1 = timer;
    timer_event.user.data2 = NULL;

    SDL_Delay(period);
    SDL_PeepEvents(&timer_event, 1, SDL_ADDEVENT, 0xffffffff);
  }

}

SCM get_ticks() {
  return scm_from_uint32(SDL_GetTicks());
}

SCM make_timer(SCM period, SCM actions) {
  timer_thread_data *timer = (timer_thread_data *) malloc(sizeof(timer_thread_data));
  timer->period = scm_to_int(period);

  hold_scm(actions);
  timer->actions = actions;

  SDL_CreateThread((int (*)(void *)) timer_thread, timer);
  return SCM_UNSPECIFIED;
}

static SCM timer_event(SDL_Event *e) {
  timer_thread_data *timer = (timer_thread_data *) e->user.data1;

  SCM actions = timer->actions;


  while(SCM_CONSP(actions)) {
    if(is_scm_procedure(SCM_CAR(actions))) {
      scm_call_0(SCM_CAR(actions));
    }
    actions = SCM_CDR(actions);
  }
  return SCM_UNSPECIFIED;
}

static void export_functions() {
  scm_c_define_gsubr("get-ticks", 0, 0, 0, get_ticks);
  scm_c_define_gsubr("make-timer", 1, 0, 1, make_timer);
}

void timer_init() {
  userevent[SDL_USEREVENT_TIMER] = timer_event;
  export_functions();
}
