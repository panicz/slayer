#include <SDL/SDL.h>
#include "extend.h"
#include "video.h"
#include "image.h"
#include "input.h"
#include "file.h"
#include "font.h"
#include "widgets.h"
#include "timer.h"
#include "utils.h"


/*
  There is a set of widgets/modules, written either in C or
  in guile scheme, that can be loaded at runtime.
  With each module, certain actions and states are associated.
  Specifically, new processes can be spawn, input can be
  retrieved and output can update the audiovisual information.
  
  The main progam runs the visual interpreter for a specified
  configuration. The input/output model is event-driven, with
  the possibility of synchronisation using the timer module.

  With every widget, there is an editor associated.

  The default widgets are: radio buttons, text field 
  and checkbox, which corespond to certain data types
  ('enum', string/number/symbol, boolean).

  There is also a button (that generates events), text
  area (whih can be used as a console) and canvas. By default, 
  each module emits various signals or messages (like the "debug" 
  signal that contains the information on what's going on)

  Every singal invokes a sequence of scheme thunk calls
  (event dispatchers) that were previously attached to it.

  Generally, the philosophy is that evrything that can be
  `clicked-in' can also be typed into the interpreter, and that
  everything that is clicked can be remembered in the log as a
  valid scheme code, and that the whole GUI is fully configurable
  at runtime.

  The order of implementation:
  first, a gui console should be implemented.
  afterwards, some buttons and simple widgets
  finally, windows, threads, canvases and so on (as loadable modules)

 */

//static widget *widgets[];
//static widget *active_widget;


SCM scm_catch_handler(void *data, SCM key, SCM args) {
  if(scm_is_eq(symbol("quit"), key)) {
    exit(0);
  }
  funcall_1("write", key);
  return SCM_UNSPECIFIED;
}

static void finish(int status, char *filename) {

  evalf("(save \"%s\")", filename);

  /*
  int i;
  char *source;
  char *keyname;
  assert(NELEMS(keydown) == NELEMS(keyup));
#define DUMP_SOURCE(keytab, i) \
  if(keytab[i] != SCM_UNSPECIFIED) { \
    source = as_c_string(scm_procedure_source(keytab[i])); \
    keyname = scm_to_locale_string(scm_c_vector_ref(key_names, i)); \
    LOG("(" # keytab " '%s %s)", keyname, source);	    \
    free(keyname); \
    free(source); \
  }


  for(i = 0; i < NELEMS(keydown); ++i) {
    DUMP_SOURCE(keydown, i);
    DUMP_SOURCE(keyup, i);
  }
#undef DUMP_SOURCE
  */
}



static void init(char *specs, Uint16 w, Uint16 h) {

  video_init(w, h);
  image_init();
  input_init();
  font_init();
  timer_init();
  widgets_init(w, h);

  // if the file doesn't exist, create it, filling it with the
  // basic definitions

  if(!file_exists(specs)) {
    if(!file_create(specs)) {
      FATAL("Unable to create spec file ``%s''", specs);
    }
  }

  if(file_empty(specs)) {
    if(!file_write(specs, "(keydn 'esc (function (type state code name mod unicode) (quit)))")) {
      FATAL("Unable to write to spec file ``%s''", specs);
    }
  }
  
  file_eval(specs);
  on_exit((void (*)(int, void *)) finish, specs);  
}

typedef struct {
  char *filename;
  Uint16 w;
  Uint16 h;
} init_t;

static void *io(init_t *arg) {
  init(arg->filename, arg->w, arg->h);

  while(1) {
    input_handle_events();
    video_refresh_screen();
  }
  return NULL;
}

#define SLAYER_SUFFIX ".spec"
 
int main(int argc, char *argv[]) {

#ifdef NDEBUG
  setenv("GUILE_WARN_DEPRECATED", "no", 1);
#else
  setenv("GUILE_WARN_DEPRECATED", "detailed", 1);
#endif

  init_t arg;
  arg.filename = malloc(strlen(argv[0]) + strlen(SLAYER_SUFFIX));

  sprintf(arg.filename, "%s" SLAYER_SUFFIX, argv[0]);
  arg.w = 640;
  arg.h = 480;

  scm_with_guile((void *(*)(void *))&io, &arg);
  free(arg.filename);
  return 0;
}
