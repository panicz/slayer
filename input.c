#include "input.h"
#include "utils.h"

SCM keydown[SDLK_LAST + SDL_NBUTTONS];
SCM keyup[SDLK_LAST + SDL_NBUTTONS];
SCM mousemove;

enum input_modes input_mode;
SCM scancodes;
SCM key_names;

static void input_mode_direct() {
  SDL_EnableUNICODE(0); // actually DisableUNICODE
  SDL_EnableKeyRepeat(0, 0); //actually DisableKeyRepeat
  input_mode = DIRECT_MODE;
}

static void input_mode_typing() {
  SDL_EnableUNICODE(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
  input_mode = TYPING_MODE;
}

static SCM s_typing;
static SCM s_direct;
static SCM s_unknown;
static SCM s_mouse;
static SCM s_input;
static SCM s_active;
static SCM s_gain;
static SCM s_loss;
static SCM s_pressed;
static SCM s_released;
static SCM s_keyboard;
static SCM s_mousemotion;
static SCM s_mousebutton;
static SCM s_left;
static SCM s_right;
static SCM s_middle;


void init_static_symbols() {
#define INIT_SYMBOL(var, val) \
  var = symbol(val);\
  hold_scm(var);

  INIT_SYMBOL(s_typing, "typing");
  INIT_SYMBOL(s_direct, "direct");
  INIT_SYMBOL(s_unknown, "unknown");
  INIT_SYMBOL(s_mouse, "mouse");
  INIT_SYMBOL(s_input, "input");
  INIT_SYMBOL(s_active, "active");
  INIT_SYMBOL(s_gain, "gain");
  INIT_SYMBOL(s_loss, "loss");
  INIT_SYMBOL(s_pressed, "pressed");
  INIT_SYMBOL(s_released, "released");
  INIT_SYMBOL(s_keyboard, "keyboard");
  INIT_SYMBOL(s_mousemotion, "mousemotion");
  INIT_SYMBOL(s_mousebutton, "mousebutton");
  INIT_SYMBOL(s_left, "left");
  INIT_SYMBOL(s_right, "right");
  INIT_SYMBOL(s_middle, "middle");

#undef INIT_SYMBOL
}

static SCM input_mode_x(SCM mode) {
  if (equal(mode, s_typing)) {
    input_mode_typing();
  }
  else if (equal(mode, s_direct)) {
    input_mode_direct();
  }
  return input_mode == TYPING_MODE ? s_typing : s_direct;
}


SCM scm_from_sdl_event(SDL_Event *event) {
  SCM state = s_unknown;

  switch(event->type) {
  case SDL_ACTIVEEVENT:  // SDL_ActiveEvent
    switch(event->active.state) {
    case SDL_APPMOUSEFOCUS:
      state = s_mouse;
      break;
    case SDL_APPINPUTFOCUS:
      state = s_input;
      break;
    case SDL_APPACTIVE:
      state = s_active;
      break;
    }
    return scm_list_3(s_active,
		      event->active.gain ? s_gain : s_loss,
		      state);
  case SDL_KEYDOWN: // SDL_KeyboardEvent
  case SDL_KEYUP:
    switch(event->key.state) {
    case SDL_PRESSED:
      state = s_pressed;
      break;
    case SDL_RELEASED:
      state = s_released;
      break;
    }
    return scm_list_n(s_keyboard,
		      state,
		      scm_from_uint8(event->key.keysym.scancode),
		      scm_c_vector_ref(key_names, event->key.keysym.sym),
		      scm_from_uint32(event->key.keysym.mod),
		      event->key.keysym.unicode ? scm_integer_to_char(scm_from_uint16(event->key.keysym.unicode)) : SCM_BOOL_F,
		      SCM_UNDEFINED);
  case SDL_MOUSEMOTION: // SDL_MouseMotionEvent
    return scm_list_n(s_mousemotion,
		      scm_from_uint8(event->motion.state),
		      scm_from_uint16(event->motion.x),
		      scm_from_uint16(event->motion.y),
		      scm_from_int16(event->motion.xrel),
		      scm_from_int16(event->motion.yrel),
		      SCM_UNDEFINED);

  case SDL_MOUSEBUTTONDOWN: // SDL_MouseButtonEvent
  case SDL_MOUSEBUTTONUP: 
    switch(event->button.state) {
    case SDL_PRESSED:
      state = s_pressed;
      break;
    case SDL_RELEASED:
      state = s_released;
      break;
    }
    //button = scm_from_int(mouseb    }
    return scm_list_n(s_mousebutton,
		      scm_c_vector_ref(key_names, 
				       SDLK_LAST + event->button.button),
		      state,
		      scm_from_uint16(event->button.x),
		      scm_from_uint16(event->button.y),
		      SCM_UNDEFINED);

  case SDL_JOYAXISMOTION: // SDL_JoyAxisEvent
  case SDL_JOYBALLMOTION: // SDL_JoyBallEvent
  case SDL_JOYHATMOTION: // SDL_JoyHatEvent
  case SDL_JOYBUTTONDOWN: // SDL_JoyButtonEvent
  case SDL_JOYBUTTONUP:
  case SDL_QUIT: // SDL_QuitEvent
  case SDL_SYSWMEVENT: // SDL_SysWMEvent
  case SDL_VIDEORESIZE: // SDL_ResizeEvent
  case SDL_VIDEOEXPOSE: // SDL_ExposeEvent
  case SDL_USEREVENT: // SDL_UserEvent
  default:
    return SCM_BOOL_F;
  }
}

static inline SCM unsupported_event(SDL_Event *e) {
  WARN("Warning: unsupported event %i", e->type);
  return SCM_UNSPECIFIED;
}

static inline SCM keydown_handler(SDL_Event *e) {
  SCM handler = keydown[e->key.keysym.sym];
  if(is_scm_procedure(handler))
    return scm_apply_0(handler, scm_from_sdl_event(e));
  return SCM_UNSPECIFIED;
}

static inline SCM keyup_handler(SDL_Event *e) {
  SCM handler = keyup[e->key.keysym.sym];
  if(is_scm_procedure(handler))
    return scm_apply_0(handler, scm_from_sdl_event(e));
  return SCM_UNSPECIFIED;
}

static inline SCM mousemotion_handler(SDL_Event *e) {
  if(is_scm_procedure(mousemove))
    return scm_apply_0(mousemove, scm_from_sdl_event(e));
  return SCM_UNSPECIFIED;
}

static inline SCM mousepressed_handler(SDL_Event *e) {
  SCM handler = keydown[SDLK_LAST + e->button.button];
  if(is_scm_procedure(handler))
    return scm_apply_0(handler, scm_from_sdl_event(e));
  return SCM_UNSPECIFIED;
}

static inline SCM mousereleased_handler(SDL_Event *e) {
  SCM handler = keyup[SDLK_LAST + e->button.button];
  if(is_scm_procedure(handler))
    return scm_apply_0(handler, scm_from_sdl_event(e));
  return SCM_UNSPECIFIED;
}

static inline SCM quit_handler(SDL_Event *e) {
  SDL_Quit();
  exit(0);
  return SCM_UNSPECIFIED;
}

#include "scancode.c" // contains the definition of scancode table

static void build_keymap() {

  char command[128];
  int i, max = 0;
  
  sprintf(command, "(make-hash-table %i)", SDLK_LAST + SDL_NBUTTONS);
  //  fprintf(stderr, "%lli\n", (int) NELEMS(keymap));
  //fprintf(stderr, command);
  scancodes = scm_c_eval_string(command);

  scm_gc_protect_object(scancodes);

  scm_c_define("*scancodes*", scancodes);
  
  
  for(i = 0; i < NELEMS(keymap); ++i) { //*(keymap[i].keyname)
    scm_hash_set_x(scancodes, 
		   scm_from_locale_string(keymap[i].keyname), 
		   scm_from_int((int) keymap[i].value));
    if(keymap[i].value > max)
      max = keymap[i].value;
  }

  key_names = scm_c_make_vector(max+1, 
				scm_from_locale_string("unknown-key"));
  scm_gc_protect_object(key_names);
  scm_c_define("*key-names*", key_names);
  
  for(i = 0; i < NELEMS(keymap); ++i) {
    scm_c_vector_set_x(key_names, keymap[i].value, 
		       scm_from_locale_string(keymap[i].keyname));
  }

}

static inline int get_scancode(SCM key) {
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

void bind_key(SCM *keytab, SDLKey key, SCM function) {
  if(!is_scm_procedure(function))
    return;

  if(keytab[key] != SCM_UNSPECIFIED) {
    release_scm(keytab[key]);
  }

  keytab[key] = function;
  hold_scm(keytab[key]);
}

SCM bind_keydown(SCM key, SCM function) {
  int scancode = get_scancode(key);
  if(scancode > -1)
    bind_key(keydown, scancode, function);
  return SCM_UNSPECIFIED;
}

SCM bind_keyup(SCM key, SCM function) {
  int scancode = get_scancode(key);
  if(scancode > -1)
    bind_key(keyup, scancode, function);
  return SCM_UNSPECIFIED;
}


SCM bind_mousemove(SCM function) {
  WARN("function should check for arity of its argument");
  bind_key(&mousemove, 0, function); 
  return SCM_UNSPECIFIED;
}

SCM input_grab(SCM on) {
  if(on != SCM_UNDEFINED) {
    if(indeed(on)) {
      SDL_WM_GrabInput(SDL_GRAB_ON);
      SDL_ShowCursor(SDL_DISABLE);
    } else if(not(on)) {
      SDL_WM_GrabInput(SDL_GRAB_OFF);
      SDL_ShowCursor(SDL_ENABLE);
    }
  }
  return (SDL_WM_GrabInput(SDL_GRAB_QUERY) == SDL_GRAB_ON) 
    ? SCM_BOOL_T
    : SCM_BOOL_F;
}

static void export_functions() {
  scm_c_define_gsubr("handle-input", 0, 0, 0, input_handle_events);
  scm_c_define_gsubr("grab-input", 0, 1, 0, input_grab);
  scm_c_define_gsubr("keydn", 2, 0, 0, bind_keydown);
  scm_c_define_gsubr("keyup", 2, 0, 0, bind_keyup);
  scm_c_define_gsubr("mousemove", 1, 0, 0, bind_mousemove);  
  scm_c_define_gsubr("input-mode", 0, 1, 0, input_mode_x);  
}


void input_init() {
  int i;
  init_static_symbols();
  for(i = 0; i < SDLK_LAST + SDL_NBUTTONS; ++i) {
    keydown[i] = keyup[i] = SCM_UNSPECIFIED;
  }
  mousemove = SCM_UNSPECIFIED;

  for(i = 0; i < SDL_NUMEVENTS; ++i) 
    event_handler[i] = unsupported_event;
  event_handler[SDL_KEYDOWN] = keydown_handler;
  event_handler[SDL_KEYUP] = keyup_handler;
  event_handler[SDL_MOUSEBUTTONDOWN] = mousepressed_handler;
  event_handler[SDL_MOUSEBUTTONUP] = mousereleased_handler;
  event_handler[SDL_MOUSEMOTION] = mousemotion_handler;
  event_handler[SDL_QUIT] = quit_handler;

  build_keymap();
  export_functions();

  //  scm_c_define_gsubr("keydown", 2, 0, 0, (SCM(*)()) bind_keydown);
  //  scm_c_define_gsubr("keyup", 2, 0, 0, (SCM(*)()) bind_keyup);

  input_mode_direct();
}

// goose_internal_api powinno definiować kilka trybów
// obsługi zdarzeń
void (*handle_events)(SDL_Event *e);

// pytanie: czy tworząc konsolę chcielibyśmy móc używać kilku rodzajów powłoki?
// czy dałoby się to zrobić tak, żeby móc linkować programy napisane pod bibliotekę
// curses z naszą konsolą?
SCM input_handle_events() {
  SDL_Event event;

  /*
  while(SDL_PollEvent(&event))
    (*event_handler[event.type])(&event);
  */
  
  if(SDL_WaitEvent(NULL)) {
    int i = 0;
    while(SDL_PollEvent(&event)) {
      ++i;
      if(input_mode == DIRECT_MODE) {
        (*event_handler[event.type])(&event);
      } else if(input_mode == TYPING_MODE) {
	switch(event.type) {
	case SDL_KEYDOWN:
	  putchar(event.key.keysym.unicode);
	  fflush(stdout);
	  break;
	case SDL_QUIT:
	  quit_handler(&event);
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


