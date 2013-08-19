#include "slayer.h"
#include "utils.h"
#include "symbols.h"
#include "video.h"
#include <SDL/SDL.h>
#include <SDL/SDL_timer.h>

#define SDL_NBUTTONS 12

enum input_modes {
  DIRECT_MODE = 0,
  TYPING_MODE = 1
};

static enum input_modes input_mode;

static SCM (*event_handler[SDL_NUMEVENTS])(SDL_Event *);
static SCM *userevent_handlers = NULL;
static int next_userevent = 0;

// procedures that are triggered when appropreate keys are pressed
static SCM keydown[SDLK_LAST + SDL_NBUTTONS];
static SCM keyup[SDLK_LAST + SDL_NBUTTONS];
static SCM mousemove;

// a map from key names (SCM string) to their scan codes (SCM integers)
static SCM scancodes;
// a vector containing 
static SCM key_names;
// a map from modifier symbols to their values
static SCM modifier_codes;

static inline void
init_modifier_codes() {
  modifier_codes = gc_protected(scm_c_make_hash_table(16));
#define INIT_MOD(name, NAME)				\
  scm_hash_set_x(modifier_codes, s_##name, scm_from_int(KMOD_##NAME))

#define INIT_MOD_LR(name, NAME)			\
  INIT_MOD(l##name, L##NAME);			\
  INIT_MOD(r##name, R##NAME);			\
  INIT_MOD(name, NAME)

  INIT_MOD_LR(shift, SHIFT);
  INIT_MOD_LR(ctrl, CTRL);
  INIT_MOD_LR(alt, ALT);
  INIT_MOD_LR(meta, META);
  INIT_MOD(num, NUM);
  INIT_MOD(caps, CAPS);
  INIT_MOD(mode, MODE);
  
#undef INIT_MOD_LR
#undef INIT_MOD
}

static SCM 
set_direct_input_mode_x() {
  SDL_EnableUNICODE(0); // actually DisableUNICODE
  SDL_EnableKeyRepeat(0, 0); //actually DisableKeyRepeat
  input_mode = DIRECT_MODE;
  return SCM_UNSPECIFIED;
}

static SCM
set_typing_input_mode_x() {
  SDL_EnableUNICODE(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
  input_mode = TYPING_MODE;
  return SCM_UNSPECIFIED;
}

static SCM 
get_input_mode(SCM mode) {
  return input_mode == TYPING_MODE ? s_typing : s_direct;
}

static SCM 
unsupported_event(SDL_Event *e) {
  WARN_UPTO(5, "unsupported event %i", e->type);
  return SCM_UNSPECIFIED;
}

static SCM 
activeevent_handler(SDL_Event *e) {
  WARN_UPTO(3, "SDL_ACTIVEEVENT (%i) not supported", e->type);
  return SCM_UNSPECIFIED;
}

static SCM resize_procedure;
static SCM 
set_resize_procedure_x(SCM procedure) {
  if(is_scm_procedure(procedure)) {
    resize_procedure = procedure;
  }
  else {
    WARN("trying to set a non-procedure as the resize procedure!");
  }
  return SCM_UNSPECIFIED;
}

static SCM 
videoresize_handler(SDL_Event *e) {
  scm_call_2(resize_procedure, scm_from_int(e->resize.w), 
	     scm_from_int(e->resize.h));
  WARN_ONCE("FOR SCREEN RESIZE ON WINDOWS/OPENGL, REFER TO %s",
	    "http://www.bytehazard.com/code/sdlres.html");
  return SCM_UNSPECIFIED;
}

static SCM 
keydown_handler(SDL_Event *e) {
  SCM handler = keydown[e->key.keysym.sym];
  /*
  WARN_UPTO(12, "%s (%d) has been pressed", 
	    scm_to_locale_string(scm_c_vector_ref(key_names,
						  e->key.keysym.sym)),
	    e->key.keysym.sym);
  */
  if(is_scm_procedure(handler)) {
    return scm_call_0(handler);
  }
  return SCM_UNSPECIFIED;
}

static SCM 
keyup_handler(SDL_Event *e) {
  SCM handler = keyup[e->key.keysym.sym];
  if(is_scm_procedure(handler))
    return scm_call_0(handler);
  return SCM_UNSPECIFIED;
}

static SCM 
mousemotion_handler(SDL_Event *e) {
  if(is_scm_procedure(mousemove))
    return scm_call_4(mousemove, 
		      scm_from_uint16(e->motion.x),
		      scm_from_uint16(e->motion.y),
		      scm_from_int16(e->motion.xrel),
		      scm_from_int16(e->motion.yrel));
  return SCM_UNSPECIFIED;
}

static SCM 
mousepressed_handler(SDL_Event *e) {
  SCM handler = keydown[SDLK_LAST + e->button.button];
  if(is_scm_procedure(handler))
    return scm_call_2(handler, 		      
		      scm_from_uint16(e->button.x),
		      scm_from_uint16(e->button.y));
  return SCM_UNSPECIFIED;
}

static SCM 
mousereleased_handler(SDL_Event *e) {
  SCM handler = keyup[SDLK_LAST + e->button.button];
  if(is_scm_procedure(handler))
    return scm_call_2(handler, 		      
		      scm_from_uint16(e->button.x),
		      scm_from_uint16(e->button.y));
  return SCM_UNSPECIFIED;
}

static SCM
register_userevent_x(SCM handler) {
  userevent_handlers = realloc(userevent_handlers, next_userevent+1);
  assert(userevent_handlers);
  userevent_handlers[next_userevent] = handler;
  hold_scm(handler);
  return scm_from_int(next_userevent++);
}

static SCM
generate_userevent_x(SCM code, SCM data1, SCM data2) {
  SDL_Event event;

  event.type = SDL_USEREVENT;
  event.user.code = (code == SCM_UNDEFINED) ? -1 : scm_to_int(code);
  event.user.data1 = (data1 == SCM_UNDEFINED) ? NULL : (void *) data1;
  event.user.data2 = (data2 == SCM_UNDEFINED) ? NULL : (void *) data2;

  SDL_PeepEvents(&event, 1, SDL_ADDEVENT, 0xffffffff);
  return SCM_UNSPECIFIED;
}

typedef struct timer_data_t {
  SDL_Event event;
  Uint32 interval;
  SDL_TimerID id;
  //SCM before_event_proc;
  //SCM after_event_proc;
} timer_data_t;

static struct list *timers = NULL;

static Uint32
run_timer(Uint32 interval, timer_data_t *data) {
  //if(data->before_event_proc) {
  //  scm_call_0(data->before_event_proc);
  //}
  SDL_PeepEvents(&data->event, 1, SDL_ADDEVENT, 0xffffffff);
  //if(data->after_event_proc) {
  //  scm_call_0(data->after_event_proc);
  //}
  return data->interval;
}

static SCM
add_timer_x(SCM interval, SCM handler) {//, SCM before_event, SCM after_event) {
  timer_data_t *data = malloc(sizeof(timer_data_t));
  data->event.type = SDL_USEREVENT;
  data->event.user.code = scm_to_int(register_userevent_x(handler));
  data->event.user.data1 = data->event.user.data2 = NULL;
  //data->before_event_proc = is_scm_procedure(before_event) 
  //  ? gc_protected(before_event) : NULL;
  //data->after_event_proc = is_scm_procedure(after_event) 
  //  ? gc_protected(after_event) : NULL;
  data->interval = scm_to_int(interval);
  data->id = SDL_AddTimer(data->interval, 
			  (SDL_NewTimerCallback) run_timer, 
			  (void *) data);
  timers = cons((void *) data, timers);
  return scm_from_int(data->event.user.code);
}

static SCM
remove_timer_x(SCM timer) {
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
      //if(data->before_event_proc) {
      //  scm_gc_unprotect_object(data->before_event_proc);
      //}
      //if(data->after_event_proc) {
      //  scm_gc_unprotect_object(data->after_event_proc);
      //}
      free(data);
      free(p);
      return timer_removed ? SCM_BOOL_T : SCM_BOOL_F;      
    }
  }
  return SCM_BOOL_F;
}

static SCM 
userevent_handler(SDL_Event *e) {
  if (-1 < e->user.code && e->user.code < next_userevent) {
    if (e->user.data2)
      return scm_call_2(userevent_handlers[e->user.code],
			e->user.data1, e->user.data2);
    if (e->user.data1)
      return scm_call_1(userevent_handlers[e->user.code],
			e->user.data1);
    return scm_call_0(userevent_handlers[e->user.code]);
  }
  WARN_UPTO(10, "unregistered callback: %d", e->user.code);
  return SCM_UNSPECIFIED;
}

static SCM 
quit_handler(SDL_Event *e) {
  exit(0);
  return SCM_UNSPECIFIED;
}

static SCM 
get_ticks() {
  return scm_from_uint32(SDL_GetTicks());
}

#include "scancode.c" // contains the definition of scancode table
//struct scancode {
//  const char *keyname;
//  SDLKey value;
//};
//static struct scancode keymap[];

static inline void 
build_keymap() {

  int i, max = 0;
  
  scancodes = scm_c_make_hash_table(SDLK_LAST + SDL_NBUTTONS);
  scm_gc_protect_object(scancodes);
  // exported in export_symbols
    
  for(i = 0; i < NELEMS(keymap); ++i) { 
    scm_hash_set_x(scancodes, 
		   scm_from_locale_string(keymap[i].keyname), 
		   scm_from_int((int) keymap[i].value));
    if(keymap[i].value > max)
      max = keymap[i].value;
  }

  key_names = scm_c_make_vector(max+1, SCM_BOOL_F);
  scm_gc_protect_object(key_names);
  // exported in export_symbols
  
  for(i = 0; i < NELEMS(keymap); ++i) {
    scm_c_vector_set_x(key_names, keymap[i].value, 
		       scm_from_locale_string(keymap[i].keyname));
  }

}

static inline int 
get_scancode(SCM key) {
  if(!(scm_is_string(key) || scm_is_symbol(key)))
    return -1;

  SCM keycode = scm_hash_ref(scancodes, 
			     (scm_is_symbol(key)
			      ? scm_symbol_to_string(key)
			      : key), 
			     SCM_UNSPECIFIED);
  if(!scm_is_integer(keycode))
    return -1;
  return scm_to_int(keycode);
}

static inline int
get_modifier_code(SCM name) {
  if(!(scm_is_symbol(name)))
    return -1;

  SCM code = scm_hash_ref(modifier_codes, name, SCM_UNSPECIFIED);
  if(!scm_is_integer(code))
    return -1;
  return scm_to_int(code);
}

static inline SCM
modifier_pressed_p(SCM name) {
  SDLMod state = SDL_GetModState();
  int code = get_modifier_code(name);
  if(state & code)
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

static void 
bind_key(SCM *keytab, SDLKey key, SCM function) {
  if(!is_scm_procedure(function))
    return;

  if(keytab[key] != SCM_UNSPECIFIED) {
    release_scm(keytab[key]);
  }

  keytab[key] = function;
  hold_scm(keytab[key]);
}

static SCM 
bind_keydown(SCM key, SCM function) {
  int scancode = get_scancode(key);
  if(scancode > -1)
    bind_key(keydown, scancode, function);
  return SCM_UNSPECIFIED;
}

static SCM 
bind_keyup(SCM key, SCM function) {
  int scancode = get_scancode(key);
  if(scancode > -1)
    bind_key(keyup, scancode, function);
  return SCM_UNSPECIFIED;
}

static SCM 
bind_mousemove(SCM function) {
  WARN("function should check for arity of its argument");
  bind_key(&mousemove, 0, function); 
  return SCM_UNSPECIFIED;
}

static SCM 
grab_input_x(SCM on) {
  if(on != SCM_UNDEFINED) {
    if(indeed(on)) {
      SDL_WM_GrabInput(SDL_GRAB_ON);
      SDL_ShowCursor(SDL_DISABLE);
    } else if(isnt(on)) {
      SDL_WM_GrabInput(SDL_GRAB_OFF);
      SDL_ShowCursor(SDL_ENABLE);
    }
  }
  return (SDL_WM_GrabInput(SDL_GRAB_QUERY) == SDL_GRAB_ON) 
    ? SCM_BOOL_T
    : SCM_BOOL_F;
}

static SCM
key_bindings(SCM type) {
  SCM vector;
  SCM *bindings;
  if(equal(type, s_pressed)) {
    bindings = keydown;
  }
  else if(equal(type, s_released)) {
    bindings = keyup;
  } else {
    WARN("Invalid argument");
    return SCM_UNSPECIFIED;
  }
  vector = scm_c_make_vector(NELEMS(keydown), SCM_UNDEFINED);
  int i;
  for(i = 0; i < NELEMS(keydown); ++i) {
    scm_c_vector_set_x(vector, i, bindings[i]);
  }
  return vector;
}

static SCM 
mousemove_binding(SCM type) {
  return mousemove;
}

static SCM typing_special;
static SCM
set_typing_special_procedure_x(SCM proc) {
  ASSERT_SCM_TYPE(procedure, proc, 1);
  typing_special = proc;
  return SCM_UNSPECIFIED;
}

static void 
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc);\
  scm_c_export(name,NULL)

#define EXPORT_OBJECT(name, c_name) \
  scm_c_define(name, c_name); \
  scm_c_export(name, NULL)

  EXPORT_PROCEDURE("handle-input", 0, 0, 0, input_handle_events);
  EXPORT_PROCEDURE("grab-input!", 0, 1, 0, grab_input_x);
  EXPORT_PROCEDURE("keydn", 2, 0, 0, bind_keydown);
  EXPORT_PROCEDURE("keyup", 2, 0, 0, bind_keyup);
  EXPORT_PROCEDURE("mousemove", 1, 0, 0, bind_mousemove);  
  EXPORT_PROCEDURE("input-mode", 0, 0, 0, get_input_mode);
  EXPORT_PROCEDURE("set-typing-input-mode!", 0, 0, 0, set_typing_input_mode_x);
  EXPORT_PROCEDURE("set-direct-input-mode!", 0, 0, 0, set_direct_input_mode_x);
  EXPORT_PROCEDURE("key-bindings", 1, 0, 0, key_bindings);
  EXPORT_PROCEDURE("mousemove-binding", 0, 0, 0, mousemove_binding);
  EXPORT_PROCEDURE("get-ticks", 0, 0, 0, get_ticks);
  EXPORT_PROCEDURE("generate-userevent!", 0, 3, 0, generate_userevent_x);
  EXPORT_PROCEDURE("register-userevent!", 1, 0, 0, register_userevent_x);
  EXPORT_PROCEDURE("add-timer!", 2, 2, 0, add_timer_x);
  EXPORT_PROCEDURE("remove-timer!", 1, 0, 0, remove_timer_x);
  EXPORT_PROCEDURE("set-resize-procedure!", 1, 0, 0, set_resize_procedure_x);
  EXPORT_PROCEDURE("set-typing-special-procedure!", 1, 0, 0, 
		   set_typing_special_procedure_x);

  EXPORT_PROCEDURE("modifier-pressed?", 1, 0, 0, modifier_pressed_p);

  EXPORT_OBJECT("*scancodes*", scancodes);
  EXPORT_OBJECT("*key-names*", key_names);  

#undef EXPORT_PROCEDURE
#undef EXPORT_OBJECT
}

void 
input_init() {
  int i;

  for(i = 0; i < SDLK_LAST + SDL_NBUTTONS; ++i) {
    keydown[i] = keyup[i] = SCM_UNSPECIFIED;
  }
  mousemove = SCM_UNSPECIFIED;

  for(i = 0; i < SDL_NUMEVENTS; ++i) 
    event_handler[i] = unsupported_event;
  event_handler[SDL_ACTIVEEVENT] = activeevent_handler;
  event_handler[SDL_VIDEORESIZE] = videoresize_handler;
  event_handler[SDL_KEYDOWN] = keydown_handler;
  event_handler[SDL_KEYUP] = keyup_handler;
  event_handler[SDL_MOUSEBUTTONDOWN] = mousepressed_handler;
  event_handler[SDL_MOUSEBUTTONUP] = mousereleased_handler;
  event_handler[SDL_MOUSEMOTION] = mousemotion_handler;
  event_handler[SDL_QUIT] = quit_handler;
  event_handler[SDL_USEREVENT] = userevent_handler;

  resize_procedure = noop;

  build_keymap();

  init_modifier_codes();
  
  scm_c_define_module("slayer", export_symbols, NULL);

  set_direct_input_mode_x();
  
  TRY_SDL(SDL_InitSubSystem(SDL_INIT_TIMER));
}

void (*handle_events)(SDL_Event *e);

SCM 
input_handle_events() {
  SDL_Event event;
  SCM c;

  if(SDL_WaitEvent(NULL)) {
    int i = 0;
    while(SDL_PollEvent(&event)) {
      ++i;
      if(input_mode == DIRECT_MODE) {
	(*event_handler[event.type])(&event);
      } else if(input_mode == TYPING_MODE) {
	switch(event.type) {
	case SDL_KEYUP:
	  break;

	case SDL_KEYDOWN:

	  if(isgraph(event.key.keysym.unicode)
	     || event.key.keysym.unicode  == ' ') {
	    c = scm_integer_to_char(scm_from_int16(event.key.keysym.unicode));
	    scm_write_char(c, scm_current_output_port());
	    scm_force_output(scm_current_output_port());
	  } 
	  else {
	    scm_call_1(typing_special, 
		       scm_from_uint16((Uint16) event.key.keysym.sym));
	    
	  }
	  //putchar(event.key.keysym.unicode);
	  //fflush(stdout);
	  break;
	case SDL_QUIT:
	  quit_handler(&event);
	  break;
	default:
	  (*event_handler[event.type])(&event);
	}
      } else {
	assert(!"NAH, THAT'S IMPOSSIBLE...");
      }
    }
  }  
  return SCM_UNSPECIFIED;
}
