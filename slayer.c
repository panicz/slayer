#include <unistd.h>
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

#ifdef USE_OPENGL
#include "draw3d.h"
#endif

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
  if (scm_is_eq(symbol("quit"), key)) {
    exit(0);
  }
  funcall_1("write", key);
  return SCM_UNSPECIFIED;
}

static void finish(int status, char *filename) {
  evalf("(save \"%s\")", filename);
}

typedef struct {
  char *infile;
  char *outfile;
  Uint16 w;
  Uint16 h;
  int video_mode;
} init_t;


static void init(init_t *arg) {

  video_init(arg->w, arg->h, arg->video_mode);
#ifdef USE_OPENGL
  if(video_mode & SDL_OPENGL) {
    LOG(draw3d_init());
  }
#endif
  image_init();
  input_init();
  font_init();
  timer_init();
  LOGTIME(widgets_init(arg->w, arg->h));

  // if the file doesn't exist, create it, filling it with the
  // basic definitions

  if (!file_exists(arg->infile)) {
    if (!file_create(arg->infile)) {
      FATAL("Unable to create spec file ``%s''", arg->infile);
    }
  }

  if (file_empty(arg->infile)) {
    if (!file_write(arg->infile, "(keydn 'esc (function (type state code name mod unicode) (quit)))")) {
      FATAL("Unable to write to spec file ``%s''", arg->infile);
    }
  }

  LOGTIME(file_eval(arg->infile));
  on_exit((void (*)(int, void *)) finish, arg->outfile);  
}

static void *io(init_t *arg) {

  init(arg);
  
  while (1) {
    input_handle_events();
    video_refresh_screen();
  }
  return NULL;
}

#define SLAYER_SUFFIX ".scm"

int main(int argc, char *argv[]) {

  init_t arg = {
    .infile = NULL,
    .outfile = NULL,
    .w = 0,
    .h = 0,
    .video_mode = SDL_HWSURFACE | SDL_DOUBLEBUF
  };
  
  int opt;
  while ((opt = getopt(argc, argv, "i:o:w:h:re:")) != -1) {
    switch (opt) {
    case 'i':
      arg.infile = malloc(strlen(optarg) + 1);
      if (arg.infile) {
	sprintf(arg.infile, "%s", optarg);
      }
      break;
    case 'o':
      arg.outfile = malloc(strlen(optarg) + 1);
      if (arg.outfile) {
	sprintf(arg.outfile, "%s", optarg);
      }
      break;
    case 'w':
      arg.w = atoi(optarg);
      break;
    case 'h':
      arg.h = atoi(optarg);
      break;
    case 'e':
      if(!strcmp(optarg, "3d")) {
	arg.video_mode |= SDL_OPENGL;
      } else {
	WARN("unknown extension: %s", optarg);
      }
      break;
    case 'r':
      arg.video_mode |= SDL_RESIZABLE;
      break;
    }
  }
 
#ifdef NDEBUG
  setenv("GUILE_WARN_DEPRECATED", "no", 1);
#else
  setenv("GUILE_WARN_DEPRECATED", "detailed", 1);
#endif

  if (!arg.infile) {
    arg.infile = 
      malloc(strlen(argv[0]) + strlen(SLAYER_SUFFIX));
    sprintf(arg.infile, "%s" SLAYER_SUFFIX, argv[0]);
  }

  if (!arg.outfile) {
    arg.outfile = 
      malloc(strlen(argv[0]) + strlen(SLAYER_SUFFIX));
    sprintf(arg.outfile, "/dev/null");//"%s" SLAYER_SUFFIX, argv[0]);
  }

  if (arg.w == 0) {
    arg.w = 640;
  }

  if (arg.h == 0) {
    arg.h = 480;
  }

  OUT("infile = %s, outfile = %s, w = %d, h = %d", arg.infile, arg.outfile, arg.w, arg.h);

  scm_with_guile((void *(*)(void *))&io, &arg);

  free(arg.outfile);
  free(arg.infile);

  return 0;
}
