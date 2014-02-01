#include "slayer.h"
#include "utils.h"
#include "symbols.h"
#include <SDL/SDL.h>
#include <SDL/SDL_timer.h>

static SCM 
get_ticks() {
  return scm_from_uint32(SDL_GetTicks());
}

typedef struct timer_data_t {
  SDL_Event event;
  Uint32 interval;
  SDL_TimerID id;
} timer_data_t;

static struct list *timers = NULL;

static Uint32
run_timer(Uint32 interval, timer_data_t *data) {
  SDL_PeepEvents(&data->event, 1, SDL_ADDEVENT, 0xffffffff);
  return data->interval;
}

static SCM
add_timer_x(SCM interval, SCM handler) {
  ASSERT_SCM_TYPE(integer, interval, 1);
  ASSERT_SCM_TYPE(procedure, handler, 2);

  timer_data_t *data = malloc(sizeof(timer_data_t));
  data->event.type = SDL_USEREVENT;
  data->event.user.code = scm_to_int(register_userevent(handler));
  data->event.user.data1 = data->event.user.data2 = NULL;
  data->interval = scm_to_int(interval);
  data->id = SDL_AddTimer(data->interval, 
			  (SDL_NewTimerCallback) run_timer, 
			  (void *) data);
  timers = cons((void *) data, timers);
  scm_remember_upto_here_2(interval, handler);
  return scm_from_int(data->event.user.code);
}

static SCM
set_timer_period_x(SCM timer_id, SCM period) {
  ASSERT_SCM_TYPE(integer, timer_id, 1);
  ASSERT_SCM_TYPE(integer, period, 2);
  int code = scm_to_int(timer_id);
  struct list *p;
  for(p = timers; p; p = p->next) {
    timer_data_t *timer = (timer_data_t *) p->data;
    if(timer->event.user.code == code) {
      timer->interval = scm_to_int(period);
      return period;
    }
  }
  WARN("timer #%d not found", code);
  scm_remember_upto_here_2(timer_id, period);
  return SCM_BOOL_F;
}

static SCM
remove_timer_x(SCM timer) {
  ASSERT_SCM_TYPE(integer, timer, 1);
  WARN("the procedure should unregister the userevent handler, which is "
       "currently not supported");
  int code = scm_to_int(timer);
  struct list *p, *prev = NULL;
  for(p = timers; p; prev = p, p = p->next) {
    timer_data_t *data = (timer_data_t *) p->data;
    if(data->event.user.code == code) {
      if(prev) {
	prev->next = p->next;
      }
      else {
	timers = p->next;
      }
      SDL_bool timer_removed = SDL_RemoveTimer(data->id);
      free(data);
      free(p);
      return timer_removed ? SCM_BOOL_T : SCM_BOOL_F;      
    }
  }
  scm_remember_upto_here_1(timer);
  return SCM_BOOL_F;
}

static void 
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);\
  scm_c_export(name,NULL)

  EXPORT_PROCEDURE("add-timer!", 2, 2, 0, add_timer_x);
  EXPORT_PROCEDURE("remove-timer!", 1, 0, 0, remove_timer_x);
  EXPORT_PROCEDURE("get-ticks", 0, 0, 0, get_ticks);
  EXPORT_PROCEDURE("set-timer-period!", 2, 0, 0, set_timer_period_x);

#undef EXPORT_PROCEDURE
}

void
timer_init() {
  TRY_SDL(SDL_InitSubSystem(SDL_INIT_TIMER));
  scm_c_define_module("slayer", export_symbols, NULL);
}
