#include <unistd.h>
#include <SDL/SDL.h>
#include "extend.h"
#include "video.h"
#include "audio.h"
#include "image.h"
#include "input.h"
#include "file.h"
#include "font.h"
#include "utils.h"
#include "symbols.h"

#ifdef USE_OPENGL
#include "3d.h"
#endif

SCM 
scm_catch_handler(void *data, SCM key, SCM args) {
  if (scm_is_eq(s_quit, key)) {
    exit(0);
  }
  scm_call_1(eval("write"), key);
  return SCM_UNSPECIFIED;
}

static SCM exit_procedure;
static SCM 
set_exit_procedure_x(SCM procedure) {
  if(is_scm_procedure(procedure)) {
    exit_procedure = procedure;
  }
  else {
    WARN("trying to set a non-procedure as the exit procedure!");
  }
  return SCM_UNSPECIFIED;
}

static void 
finish(int status, char *filename) {
  scm_call_1(exit_procedure, scm_from_locale_string(filename));

  scm_gc();
  audio_finish();
  input_finish();
  video_finish();

  SDL_Quit();
}

typedef struct {
  char *infile;
  char *outfile;
  Uint16 w;
  Uint16 h;
  int video_mode;
} init_t;

static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc)	    \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);
  
  EXPORT_PROCEDURE("set-exit-procedure!", 1, 0, 0, 
		   set_exit_procedure_x);
#undef EXPORT_PROCEDURE
}

static void 
init(init_t *arg) {
  symbols_init();

  exit_procedure = noop;
  scm_c_define_module("slayer", export_symbols, NULL);
  video_init(arg->w, arg->h, arg->video_mode);
  input_init();
  audio_init();

  // these calls should be moved to separate libraries
  image_init();
  font_init();

  // if the file doesn't exist, create it, filling it with the
  // basic definitions

  if (!file_exists(arg->infile)) {
    if (!file_create(arg->infile)) {
      FATAL("Unable to create spec file ``%s''", arg->infile);
    }
  }

  if (file_empty(arg->infile)) {
    if (!file_write(arg->infile, 
		    "(use-modules (slayer))\n"
		    "(keydn 'esc quit)\n")) {
      FATAL("Unable to write to spec file ``%s''", arg->infile);
    }
  }

  LOGTIME(file_eval(arg->infile));
  on_exit((void (*)(int, void *)) finish, arg->outfile);
}

static void *
io(init_t *arg) {
  /* To see a world in a grain of sand,
   * And a heaven in a wild flower,
   * Hold infinity in the palm of your hand,
   * And eternity in an hour.
   */
  init(arg);
  while (1) {
    input_handle_events();
    video_refresh_screen();
  }
  return NULL;
}

#define SLAYER_SUFFIX ".scm"

int 
main(int argc, char *argv[]) {

  init_t arg = {
    .infile = NULL,
    .outfile = NULL,
    .w = 0,
    .h = 0,
    .video_mode = SDL_HWSURFACE | SDL_DOUBLEBUF
  };
  
  int opt;
  while ((opt = getopt(argc, argv, "i:o:w:h:rfe:")) != -1) {
    switch (opt) {
    case 'i': // input file
      arg.infile = malloc(strlen(optarg) + 1);
      if (arg.infile) {
	sprintf(arg.infile, "%s", optarg);
      }
      break;
    case 'o': // output file
      arg.outfile = malloc(strlen(optarg) + 1);
      if (arg.outfile) {
	sprintf(arg.outfile, "%s", optarg);
      }
      break;
    case 'w': // screen width
      arg.w = atoi(optarg);
      break;
    case 'h': // screen height
      arg.h = atoi(optarg);
      break;
    case 'e': // extensions (3d, net)
      if(!strcmp(optarg, "3d")) {
	arg.video_mode |= SDL_OPENGL;
      } else {
	WARN("unknown extension: %s", optarg);
      }
      break;
    case 'r': // resizable
      arg.video_mode |= SDL_RESIZABLE;
      break;
    case 'f':
      arg.video_mode |= SDL_FULLSCREEN;
      break;
      
    default:
      break;
    }
  }
 
#ifdef NDEBUG
  setenv("GUILE_WARN_DEPRECATED", "no", 1);
#else
  setenv("GUILE_WARN_DEPRECATED", "detailed", 1);
#endif
  setenv("GUILE_LOAD_PATH", ".:./libs", 1);
  setenv("LTDL_LIBRARY_PATH", ".:./libs", 1);
  setenv("LC_ALL", "C.UTF8", 1); // discard locale

  if (!arg.infile) {
    arg.infile = 
      malloc(strlen(argv[0]) + strlen(SLAYER_SUFFIX) + 1);
    sprintf(arg.infile, "%s" SLAYER_SUFFIX, argv[0]);
  }

  if (!arg.outfile) {
    arg.outfile = 
      malloc(strlen(argv[0]) + strlen(SLAYER_SUFFIX) + 1);
    sprintf(arg.outfile, "/dev/null");//"%s" SLAYER_SUFFIX, argv[0]);
  }

  if (arg.w == 0) {
    arg.w = 640;
  }

  if (arg.h == 0) {
    arg.h = 480;
  }

  OUT("infile = %s, outfile = %s, w = %d, h = %d", arg.infile, arg.outfile, 
      arg.w, arg.h);

  scm_with_guile((void *(*)(void *))&io, &arg);

  free(arg.outfile);
  free(arg.infile);

  return 0;
}
