#include <unistd.h>
#include <SDL/SDL.h>
#include <getopt.h>
#include "slayer.h"
#include "video.h"
#include "symbols.h"

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

typedef struct {
  char *infile;
  char *outfile;
  Uint16 w;
  Uint16 h;
  int video_mode;
  int sound;
} arg_t;

static void 
finish(int status, arg_t *arg) {
  scm_call_1(exit_procedure, scm_from_locale_string(arg->outfile));

  scm_gc();
#ifdef USE_SDL_MIXER
  if(arg->sound) {
    audio_finish();
  }
#endif

  SDL_Quit();
}

static SCM 
set_caption_x(SCM title) {
  char *str = scm_to_locale_string(title);
  SDL_WM_SetCaption(str, NULL);
  free(str);
  return title;
}

static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc)	    \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);
  
  EXPORT_PROCEDURE("set-exit-procedure!", 1, 0, 0, 
		   set_exit_procedure_x);

  EXPORT_PROCEDURE("set-caption!", 1, 0, 0, set_caption_x); 

#undef EXPORT_PROCEDURE
}

static void
fake_audio(void *unused) {
#define FAKE_PROCEDURE(name)					    \
  scm_c_define_gsubr(name, 0, 0, 1,(scm_t_subr) scm_noop);	    \
  scm_c_export(name, NULL);
  
  FAKE_PROCEDURE("load-sound");
  FAKE_PROCEDURE("play-sound!");
  FAKE_PROCEDURE("load-music");
  FAKE_PROCEDURE("play-music!");
  FAKE_PROCEDURE("pause-music!");
  FAKE_PROCEDURE("resume-music!");

#undef EXPORT_PROCEDURE
}

static void 
init(arg_t *arg) {
  symbols_init();

  exit_procedure = noop;
  scm_c_define_module("slayer", export_symbols, NULL);
  video_init(arg->w, arg->h, arg->video_mode);
  input_init();
#ifdef USE_SDL_MIXER
  if(arg->sound) {
    audio_init();
  } else {
#endif
    scm_c_define_module("slayer", fake_audio, NULL);
#ifdef USE_SDL_MIXER
  }
#endif

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
  on_exit((void (*)(int, void *)) finish, (void *) arg);
}

static void *
io(arg_t *arg) {
  /* To see a world in a grain of sand,
   * And a heaven in a wild flower,
   * Hold infinity in the palm of your hand,
   * And eternity in an hour.
   *                         -- William Blake
   */
  init(arg);
  unsigned int step = 0;
  while (1) {
    input_handle_events();
    video_refresh_screen();
    if((step++ % 64) == 0) {
      scm_gc();
    }
  }
  return NULL;
}

static void
show_version() {
  printf("%s\n", PACKAGE_STRING);
  printf("%s\n", COPYRIGHT);
}


static void
show_usage(const char *program, const char *file_name) {
#define TABS "\t\t\t\t" //"                             "
  printf("Usage: %s [OPTION] ...\n", program);
  printf("Run specified program in SLAYER environment. "
	 "Unless -i option given, it\n");
  printf("attempts to run a program named %s%s"
	 " from current working directory.\n\n", file_name, SLAYER_SUFFIX);
  printf("Available options:\n");
  printf("  -i FILE,   --input FILE\texecute FILE (guile scheme source)\n");

#if defined(USE_OPENGL) || defined(USE_SDL_MIXER)
  printf("  -e EXT, --extension EXT\tload extension EXT."
	 " Currently available\n");
  printf(TABS "extensions are:\n");
#ifdef USE_OPENGL
  printf(TABS "  * 3d      use OpenGL    (%s by default)\n",
#ifdef ENABLE_DEFAULT_3D
	 "enabled"
#else
	 "disabled"
#endif // DEFAULT 3D
	 );
#endif // USE OPENGL

#ifdef USE_SDL_MIXER
  printf(TABS "  * sound   use SDL_mixer (%s by default)\n",
	 "enabled");
#endif  // MIXER
  printf("  -d EXT,  --disable EXT\tdisable extension EXT\n");
#endif  // EXTENSIONS
  printf("  -w N,        --width N\tset initial window width to N\n");
  printf("  -h N,       --height N\tset initial window height to N\n");
  printf("  -r,        --resizable\tallow user to resize the window\n");
  printf(TABS "by dragging its edge\n");
  printf("  --help\t\t\tdisplay this help and exit\n");
  printf("  --version\t\t\tdisplay version information and exit\n");
  printf("\n");
  printf("Report bugs to: %s\n", PACKAGE_BUGREPORT);
  printf("For more information, type ``info slayer''\n");
#undef TABS
}

static void
process_command_line_options(int argc, 
			     char *argv[], 
			     arg_t *arg, 
			     const char *exec_file_name) {
  int option_index;
  static struct option long_options[] = {
    {"help",      no_argument,       0,  0 },
    {"version",   no_argument,       0,  0 },
    {"input",     required_argument, 0, 'i'},
    {"output",    required_argument, 0, 'o'},
    {"extension", required_argument, 0, 'e'},
    {"disable",   required_argument, 0, 'd'},
    {"nosound",   no_argument,       0,  0 },
    {"resizable", no_argument,       0, 'r'},
    {"width",     required_argument, 0, 'w'},
    {"height",    required_argument, 0, 'h'},
    {0,           0,                 0,  0 }
  };
  
  int opt;
  while ((opt = getopt_long(argc, argv, "i:o:w:h:rfe:d:",
			    long_options, &option_index)) != -1) {
    switch (opt) {
    case 0:
      if(0) {
      }
#ifdef USE_SDL_MIXER
      else if(!strcmp(long_options[option_index].name, "nosound")) {
	arg->sound = 0;
      }
#endif
      else if(!strcmp(long_options[option_index].name, "help")) {
	show_usage(argv[0], exec_file_name);
	exit(0);
      }
      else if(!strcmp(long_options[option_index].name, "version")) {
	show_version();
	exit(0);
      }
      else {
	printf("Unrecognised option: %s\n", long_options[option_index].name);
	show_usage(argv[0], exec_file_name);
	exit(-1);
      }
      break;
    case 'i': // input file
      arg->infile = malloc(strlen(optarg) + 1);
      if (arg->infile) {
	sprintf(arg->infile, "%s", optarg);
      }
      break;
    case 'o': // output file
      arg->outfile = malloc(strlen(optarg) + 1);
      if (arg->outfile) {
	sprintf(arg->outfile, "%s", optarg);
      }
      break;
    case 'w': // screen width
      arg->w = atoi(optarg);
      break;
    case 'h': // screen height
      arg->h = atoi(optarg);
      break;
    case 'e': // enable extensions
      if(0) {
      }
#ifdef USE_OPENGL
      else if(!strcmp(optarg, "3d")) {
	arg->video_mode |= SDL_OPENGL;
      }
#endif
      else {
	WARN("unknown extension: %s", optarg);
      }
      break;
    case 'd': // disable extensions
      if(0) {
      }
#ifdef USE_OPENGL
      else if(!strcmp(optarg, "3d")) {
	arg->video_mode &= ~SDL_OPENGL;
      }
#endif
#ifdef USE_SDL_MIXER
      else if(!strcmp(optarg, "sound")) {
	arg->sound = 0;
      }
#endif
      else {
	WARN("unknown extension: %s", optarg);
      }
      break;
    case 'r': // resizable
      arg->video_mode |= SDL_RESIZABLE;
      break;
    case 'f':
      arg->video_mode |= SDL_FULLSCREEN;
      break;
      
    default:
      break;
    }
  }
}

int 
main(int argc, char *argv[]) {

  arg_t arg = {
    .infile = NULL,
    .outfile = NULL,
    .w = 0,
    .h = 0,
    .video_mode = SDL_HWSURFACE | SDL_DOUBLEBUF,
    .sound = 1
  };

  // get the name of current file, skipping any slashes
  char *filename = argv[0];

  int i;
  for(i = 0; argv[0][i]; ++i) {
    if(argv[0][i] == '/') {
      filename = &argv[0][i+1];
    }
  }

#ifdef ENABLE_DEFAULT_3D
  arg.video_mode |= SDL_OPENGL;
#endif

  process_command_line_options(argc, argv, &arg, filename);

#ifdef NDEBUG
  setenv("GUILE_WARN_DEPRECATED", "no", 1);
#else
  setenv("GUILE_WARN_DEPRECATED", "detailed", 1);
#endif
  setenv("GUILE_LOAD_PATH", ".:./libs:../guile-modules", 1);
  setenv("LTDL_LIBRARY_PATH", ".:./libs", 1);
  setenv("LC_ALL", "C.UTF8", 1); // discard locale

  if (!arg.infile) {
    arg.infile = 
      malloc(strlen(filename) + strlen(SLAYER_SUFFIX) + 1);
    sprintf(arg.infile, "%s" SLAYER_SUFFIX, filename);
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
