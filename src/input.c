#include "slayer.h"
#include "utils.h"
#include "symbols.h"
#include "video.h"
#include <SDL/SDL.h>
#include "input.h"

static enum input_modes input_mode;

static SCM (*event_handler[SDL_NUMEVENTS])(SDL_Event *);
static SCM *userevent_handlers = NULL;
static int next_userevent = 0;

// procedures that are triggered when appropreate keys are pressed
static SCM keydown;
static SCM keyup;

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
  DisableUNICODE();
  DisableKeyRepeat();
  input_mode = DIRECT_MODE;
  return SCM_UNSPECIFIED;
}

static SCM
set_typing_input_mode_x() {
  EnableUNICODE();
  EnableDefaultKeyRepeat();
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
  SCM handler = SCM_SIMPLE_VECTOR_REF(keydown, e->key.keysym.sym);

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
  //SCM handler = keyup[e->key.keysym.sym];
  SCM handler = SCM_SIMPLE_VECTOR_REF(keyup, e->key.keysym.sym);
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
  SCM handler = SCM_SIMPLE_VECTOR_REF(keydown, SDLK_LAST + e->button.button);
  if(is_scm_procedure(handler))
    return scm_call_2(handler, 		      
		      scm_from_uint16(e->button.x),
		      scm_from_uint16(e->button.y));
  return SCM_UNSPECIFIED;
}

static SCM 
mousereleased_handler(SDL_Event *e) {
  //SCM handler = keyup[SDLK_LAST + e->button.button];
  SCM handler = SCM_SIMPLE_VECTOR_REF(keyup, SDLK_LAST + e->button.button);
  if(is_scm_procedure(handler))
    return scm_call_2(handler, 		      
		      scm_from_uint16(e->button.x),
		      scm_from_uint16(e->button.y));
  return SCM_UNSPECIFIED;
}

static SCM
mouse_position() {
  int x, y;
  (void) SDL_GetMouseState(&x, &y);
  return scm_list_2(scm_from_int(x), scm_from_int(y));
}

static SCM
set_mouse_position_x(SCM x, SCM y) {
  SDL_WarpMouse(scm_to_uint16(x), scm_to_uint16(y));
  return SCM_UNSPECIFIED;  
}

SCM
register_userevent(SCM handler) {
  userevent_handlers = realloc(userevent_handlers, next_userevent+1);
  assert(userevent_handlers);
  userevent_handlers[next_userevent] = handler;
  hold_scm(handler);
  return scm_from_int(next_userevent++);
}

SCM
generate_userevent(SCM code, SCM data1, SCM data2) {
  SDL_Event event;

  event.type = SDL_USEREVENT;
  event.user.code = (code == SCM_UNDEFINED) ? -1 : scm_to_int(code);
  event.user.data1 = (data1 == SCM_UNDEFINED) ? NULL : (void *) data1;
  event.user.data2 = (data2 == SCM_UNDEFINED) ? NULL : (void *) data2;

  SDL_PeepEvents(&event, 1, SDL_ADDEVENT, 0xffffffff);
  return SCM_UNSPECIFIED;
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

struct scancode_t {
  const char *keyname;
  SDLKey value;
};

static struct scancode_t keymap[] = {
#include "scancode.c" 
};

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
  if(!(scm_is_string(key) 
       || scm_is_symbol(key) 
       || scm_is_integer(key))) {
    return -1;
  }

  SCM keycode = scm_hash_ref(scancodes, 
			     (scm_is_symbol(key)
			      ? scm_symbol_to_string(key)
			      : (scm_is_integer(key) 
				 ? scm_number_to_string(key, scm_from_int(10))
				 : key)),
			     SCM_UNSPECIFIED);

  if(!scm_is_integer(keycode)) {
    return -1;
  }
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

#ifdef PROVIDE_KEY_BINDINGS_ACCESSORS

static inline int
is_key_bindings(SCM var) {
  if(!scm_is_simple_vector(var)
     || (SCM_SIMPLE_VECTOR_LENGTH(var) != 3)) { 
    return 0; 
  }
  SCM up = SCM_SIMPLE_VECTOR_REF(var, KEY_BINDINGS_UP);
  SCM dn = SCM_SIMPLE_VECTOR_REF(var, KEY_BINDINGS_DOWN);
  SCM move = SCM_SIMPLE_VECTOR_REF(var, KEY_BINDINGS_MOUSEMOVE);
  
  if(!scm_is_simple_vector(up)
     || !scm_is_simple_vector(dn)
     || !scm_is_procedure(move)
     || (SCM_SIMPLE_VECTOR_LENGTH(up) != (SDLK_LAST + SDL_NBUTTONS))
     || (SCM_SIMPLE_VECTOR_LENGTH(dn) != (SDLK_LAST + SDL_NBUTTONS))) {
    return 0;
  }
  int i;
  for(i = 0; i <= SDLK_LAST; ++i) {
    if(!scm_is_thunk(SCM_SIMPLE_VECTOR_REF(up, i)) 
       || !scm_is_thunk(SCM_SIMPLE_VECTOR_REF(dn, i))) {
      return 0;
    }
  }
  for(i = SDLK_LAST+1; i < (SDLK_LAST + SDL_NBUTTONS); ++i) {
    if(!scm_is_procedure(SCM_SIMPLE_VECTOR_REF(up, i)) 
       || !scm_is_procedure(SCM_SIMPLE_VECTOR_REF(dn, i))) {
      return 0;
    }
  }
  return 1;
}

static SCM
key_bindings_p(SCM var) {
  return is_key_bindings(var) ? SCM_BOOL_T : SCM_BOOL_F;
}

#define DEF_KEY_BINDINGS_VECTOR_GETTER(which, WHICH)			\
  static SCM key_bindings_##which(SCM bindings) {			\
    if(!GIVEN(bindings)) {						\
      SCM copy = scm_c_make_vector(SDLK_LAST + SDL_NBUTTONS, SCM_UNDEFINED); \
      int i;								\
      for (i = 0; i < (SDLK_LAST + SDL_NBUTTONS); ++i) {		\
	SCM_SIMPLE_VECTOR_SET(copy, i, SCM_SIMPLE_VECTOR_REF(key##which, i)); \
      }									\
      return copy;							\
    }									\
    return SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_##WHICH);	\
  }

DEF_KEY_BINDINGS_VECTOR_GETTER(up, UP)
DEF_KEY_BINDINGS_VECTOR_GETTER(down, DOWN)

#undef DEF_KEY_BINDINGS_VECTOR_GETTER

static SCM key_bindings_mousemove(SCM bindings) {
  if(!GIVEN(bindings)) {
    return mousemove;
  }
  return SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_MOUSEMOVE);
}

#endif // PROVIDE_KEY_BINDINGS_ACCESSORS

static SCM key_bindings; // a fluid that allows to specify, which set
// of bindings should be modified by keydn, keyup and mousemove
// (it's called KEY-BINDINGS on the Scheme side)

static inline SCM
bind_key(SCM keytab, SDLKey key, SCM function) {
  if(!is_scm_procedure(function)) {
    return SCM_SIMPLE_VECTOR_REF(keytab, key);
  }
  SCM_SIMPLE_VECTOR_SET(keytab, key, function);
  return SCM_UNSPECIFIED;
}

static SCM 
bind_keydown(SCM key, SCM function) {
  int scancode = get_scancode(key);
  SCM bindings = scm_fluid_ref(key_bindings);
  SCM tab = indeed(bindings)
    ? SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_DOWN)
    : keydown;

  if(scancode > -1)
    return bind_key(tab, scancode, function);
  return SCM_UNSPECIFIED;
}

static SCM 
bind_keyup(SCM key, SCM function) {
  int scancode = get_scancode(key);
  SCM bindings = scm_fluid_ref(key_bindings);
  SCM tab = indeed(bindings)
    ? SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_UP)
    : keyup;

  if(scancode > -1)
    return bind_key(tab, scancode, function);
  return SCM_UNSPECIFIED;
}

static SCM 
bind_mousemove(SCM function) {
  WARN_ONCE("function should check for arity of its argument");
  SCM bindings = scm_fluid_ref(key_bindings);
  if(indeed(bindings)) {
    if(!is_scm_procedure(function)) {
      return SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_MOUSEMOVE);
    }
    SCM_SIMPLE_VECTOR_SET(bindings, KEY_BINDINGS_MOUSEMOVE, function);
  } else {
    if(!is_scm_procedure(function)) {
      return mousemove;
    }
    if(!scm_is_eq(mousemove, noop)) {
      scm_gc_unprotect_object(mousemove);
    }
    mousemove = gc_protected(function);
  }
  return SCM_UNSPECIFIED;
}

static SCM
fresh_key_bindings() {
  SCM up = scm_c_make_vector(SDLK_LAST + SDL_NBUTTONS, noop);
  SCM dn = scm_c_make_vector(SDLK_LAST + SDL_NBUTTONS, noop);
  SCM bindings = scm_c_make_vector(3, SCM_UNDEFINED);
  SCM_SIMPLE_VECTOR_SET(bindings, KEY_BINDINGS_UP, up);
  SCM_SIMPLE_VECTOR_SET(bindings, KEY_BINDINGS_DOWN, dn);
  SCM_SIMPLE_VECTOR_SET(bindings, KEY_BINDINGS_MOUSEMOVE, noop);
  return bindings;
}

static SCM
current_key_bindings() {
  SCM bindings = fresh_key_bindings();
  SCM up = SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_UP);
  SCM down = SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_DOWN);
  int i;
  for(i = 0; i < (SDLK_LAST + SDL_NBUTTONS); ++i) {
    SCM_SIMPLE_VECTOR_SET(up, i, SCM_SIMPLE_VECTOR_REF(keyup, i));
    SCM_SIMPLE_VECTOR_SET(down, i, SCM_SIMPLE_VECTOR_REF(keydown, i));
  }
  SCM_SIMPLE_VECTOR_SET(bindings, KEY_BINDINGS_MOUSEMOVE, mousemove);
  return bindings;
}

static SCM
set_key_bindings_x(SCM bindings) {
  SCM up = SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_UP);
  SCM down = SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_DOWN);
  int i;
  for(i = 0; i < (SDLK_LAST + SDL_NBUTTONS); ++i) {
    SCM_SIMPLE_VECTOR_SET(keyup, i, SCM_SIMPLE_VECTOR_REF(up, i));
    SCM_SIMPLE_VECTOR_SET(keydown, i, SCM_SIMPLE_VECTOR_REF(down, i));
  }
  bind_mousemove(SCM_SIMPLE_VECTOR_REF(bindings, KEY_BINDINGS_MOUSEMOVE));
  return SCM_UNSPECIFIED;
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
  EXPORT_PROCEDURE("keydn", 1, 1, 0, bind_keydown);
  EXPORT_PROCEDURE("keyup", 1, 1, 0, bind_keyup);
  EXPORT_PROCEDURE("mousemove", 0, 1, 0, bind_mousemove); 
  EXPORT_PROCEDURE("mouse-position", 0, 0, 0, mouse_position);
  EXPORT_PROCEDURE("set-mouse-position!", 2, 0, 0, set_mouse_position_x);

  EXPORT_PROCEDURE("input-mode", 0, 0, 0, get_input_mode);
  EXPORT_PROCEDURE("set-typing-input-mode!", 0, 0, 0, set_typing_input_mode_x);
  EXPORT_PROCEDURE("set-direct-input-mode!", 0, 0, 0, set_direct_input_mode_x);
  EXPORT_PROCEDURE("current-key-bindings", 0, 0, 0, current_key_bindings);
  EXPORT_PROCEDURE("fresh-key-bindings", 0, 0, 0, fresh_key_bindings);

#ifdef PROVIDE_KEY_BINDINGS_ACCESSORS
  EXPORT_PROCEDURE("key-bindings?", 1, 0, 0, key_bindings_p);
  EXPORT_PROCEDURE("keyup-bindings", 0, 1, 0, key_bindings_up);
  EXPORT_PROCEDURE("keydn-bindings", 0, 1, 0, key_bindings_down);
  EXPORT_PROCEDURE("mousemove-binding", 0, 1, 0, key_bindings_mousemove);
#endif //PROVIDE_KEY_BINDINGS_ACCESSORS

  EXPORT_PROCEDURE("set-key-bindings!", 1, 0, 0, set_key_bindings_x);

  EXPORT_PROCEDURE("generate-userevent!", 0, 3, 0, generate_userevent);
  EXPORT_PROCEDURE("register-userevent!", 1, 0, 0, register_userevent);
  EXPORT_PROCEDURE("set-resize-procedure!", 1, 0, 0, set_resize_procedure_x);
  EXPORT_PROCEDURE("set-typing-special-procedure!", 1, 0, 0, 
		   set_typing_special_procedure_x);

  EXPORT_PROCEDURE("modifier-pressed?", 1, 0, 0, modifier_pressed_p);

  EXPORT_OBJECT("KEY-BINDINGS", key_bindings);
  EXPORT_OBJECT("*scancodes*", scancodes);
  EXPORT_OBJECT("*key-names*", key_names);  

#undef EXPORT_PROCEDURE
#undef EXPORT_OBJECT
}

#define COMPRESS_MOTION_EVENT(src, dest)	\
  dest.motion.x = src.motion.x;			\
  dest.motion.y = src.motion.y;			\
  dest.motion.xrel += src.motion.xrel;		\
  dest.motion.yrel += src.motion.yrel;		\
  src.type = SDL_USEREVENT;			\
  src.user.code = -1

static int
compress_mouse_moves(SDL_Event *e) {
  static SDL_Event queue[16];
  static int left = 0;
  static int base = 0;
  int i = 0, j;

 yield:
  if(left) {
    --left;
    while(queue[base].type == SDL_USEREVENT && queue[base].user.code == -1) {
      ++base;
    }
    *e = queue[base];
    ++base;
    return 1;
  }
  else {
    base = 0;
  }

  left = SDL_PeepEvents(queue, NELEMS(queue), SDL_GETEVENT, SDL_ALLEVENTS);
  if(!left) {
    return 0;
  }

  while(i < left && queue[base+i].type != SDL_MOUSEMOTION) {
    ++i;
  }
  
  for(j = i + 1; j < left; ++j) {
    if(queue[base+j].type == SDL_MOUSEMOTION) {
      if(queue[base+j].motion.state != queue[base+i].motion.state) {
	i = j;
      }
      else {
	--left;
	COMPRESS_MOTION_EVENT(queue[base+j], queue[base+i]);
      }
    }
    else {
      while(j < left && queue[base+j].type != SDL_MOUSEMOTION) {
	++j;
      }
      i = j;
    }
  }
  goto yield;
}

#undef COMPRESS_MOTION_EVENT

int (*getting_events)(SDL_Event *e) = compress_mouse_moves; //SDL_PollEvent;

void 
input_init() {
  int i;

  keydown = gc_protected(scm_c_make_vector(SDLK_LAST + SDL_NBUTTONS, noop));
  
  keyup = gc_protected(scm_c_make_vector(SDLK_LAST + SDL_NBUTTONS, noop));

  mousemove = noop;

  key_bindings = gc_protected(scm_make_fluid());

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
}

SCM 
input_handle_events() {
  SDL_Event event;
  SCM c;

  if(SDL_WaitEvent(NULL)) {
    while(getting_events(&event)) {
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
