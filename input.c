#include "input.h"
#include "utils.h"
#include "symbols.h"

SCM (*event_handler[SDL_NUMEVENTS])(SDL_Event *);

SCM keydown[SDLK_LAST + SDL_NBUTTONS];
SCM keyup[SDLK_LAST + SDL_NBUTTONS];
SCM mousemove;

enum input_modes input_mode;
SCM scancodes;
SCM key_names;

static void 
input_mode_direct() {
  SDL_EnableUNICODE(0); // actually DisableUNICODE
  SDL_EnableKeyRepeat(0, 0); //actually DisableKeyRepeat
  input_mode = DIRECT_MODE;
}

static void 
input_mode_typing() {
  SDL_EnableUNICODE(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
  input_mode = TYPING_MODE;
}

static SCM 
input_mode_x(SCM mode) {
  if (equal(mode, s_typing)) {
    input_mode_typing();
  }
  else if (equal(mode, s_direct)) {
    input_mode_direct();
  }
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

static SCM 
keydown_handler(SDL_Event *e) {
  SCM handler = keydown[e->key.keysym.sym];
  if(is_scm_procedure(handler))
    return scm_call_0(handler);
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
userevent_handler(SDL_Event *e) {
  WARN_ONCE("not (quite) implemented");
  return SCM_UNSPECIFIED;
}

static SCM 
quit_handler(SDL_Event *e) {
  SDL_Quit();
  exit(0);
  return SCM_UNSPECIFIED;
}

static SCM 
get_ticks() {
  return scm_from_uint32(SDL_GetTicks());
}

static SCM
generate_userevent(SCM code) {
  SDL_Event event;

  event.type = SDL_USEREVENT;
  event.user.code = (code == SCM_UNDEFINED) ? 0 : scm_to_int(code);
  event.user.data1 = NULL;
  event.user.data2 = NULL;

  SDL_PeepEvents(&event, 1, SDL_ADDEVENT, 0xffffffff);
  return SCM_UNSPECIFIED;
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
  
  scancodes = evalf("(make-hash-table %i)", SDLK_LAST + SDL_NBUTTONS); 

  scm_gc_protect_object(scancodes);

  scm_c_define("*scancodes*", scancodes);
    
  for(i = 0; i < NELEMS(keymap); ++i) { 
    scm_hash_set_x(scancodes, 
		   scm_from_locale_string(keymap[i].keyname), 
		   scm_from_int((int) keymap[i].value));
    if(keymap[i].value > max)
      max = keymap[i].value;
  }

  key_names = scm_c_make_vector(max+1, SCM_BOOL_F);
  scm_gc_protect_object(key_names);
  scm_c_define("*key-names*", key_names);
  
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

void 
bind_key(SCM *keytab, SDLKey key, SCM function) {
  if(!is_scm_procedure(function))
    return;

  if(keytab[key] != SCM_UNSPECIFIED) {
    release_scm(keytab[key]);
  }

  keytab[key] = function;
  hold_scm(keytab[key]);
}

SCM 
bind_keydown(SCM key, SCM function) {
  int scancode = get_scancode(key);
  if(scancode > -1)
    bind_key(keydown, scancode, function);
  return SCM_UNSPECIFIED;
}

SCM 
bind_keyup(SCM key, SCM function) {
  int scancode = get_scancode(key);
  if(scancode > -1)
    bind_key(keyup, scancode, function);
  return SCM_UNSPECIFIED;
}


SCM 
bind_mousemove(SCM function) {
  WARN("function should check for arity of its argument");
  bind_key(&mousemove, 0, function); 
  return SCM_UNSPECIFIED;
}

SCM 
input_grab(SCM on) {
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

static 
SCM key_bindings(SCM type) {
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


static inline void 
export_functions() {
  scm_c_define_gsubr("handle-input", 0, 0, 0, input_handle_events);
  scm_c_define_gsubr("grab-input", 0, 1, 0, input_grab);
  scm_c_define_gsubr("keydn", 2, 0, 0, bind_keydown);
  scm_c_define_gsubr("keyup", 2, 0, 0, bind_keyup);
  scm_c_define_gsubr("mousemove", 1, 0, 0, bind_mousemove);  
  scm_c_define_gsubr("input-mode", 0, 1, 0, input_mode_x);
  scm_c_define_gsubr("key-bindings", 1, 0, 0, key_bindings);
  scm_c_define_gsubr("mousemove-binding", 0, 0, 0, mousemove_binding);
  scm_c_define_gsubr("get-ticks", 0, 0, 0, get_ticks);
  scm_c_define_gsubr("generate-userevent", 0, 1, 0, generate_userevent);
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
  event_handler[SDL_KEYDOWN] = keydown_handler;
  event_handler[SDL_KEYUP] = keyup_handler;
  event_handler[SDL_MOUSEBUTTONDOWN] = mousepressed_handler;
  event_handler[SDL_MOUSEBUTTONUP] = mousereleased_handler;
  event_handler[SDL_MOUSEMOTION] = mousemotion_handler;
  event_handler[SDL_QUIT] = quit_handler;
  event_handler[SDL_USEREVENT] = userevent_handler;

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
SCM 
input_handle_events() {
  SDL_Event event;
  SCM c, input_widget;

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
	case SDL_KEYUP:
	  break;

	case SDL_KEYDOWN:

	  if(isgraph(event.key.keysym.unicode) || event.key.keysym.unicode  == ' ') {
	    c = scm_integer_to_char(scm_from_int16(event.key.keysym.unicode));
	    scm_write_char(c, scm_current_output_port());
	    scm_force_output(scm_current_output_port());
	  } else {
	    input_widget = eval("*input-widget*");
	    if(indeed(input_widget)) {
	      SCM special_keys = eval("(slot-ref *input-widget* 'special-keys)");
	      scm_apply_0(scm_c_vector_ref(special_keys, event.key.keysym.sym), SCM_EOL);
	    }
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


