#include <unistd.h>
#include <SDL/SDL.h>
#include <getopt.h>
#include "slayer.h"
#include "video.h"
#include "symbols.h"
#include "utils.h"
#include <stdbool.h>

#if !HAVE_VASPRINTF
int vasprintf(char **strp, const char *fmt, va_list ap) {
  int size = vsnprintf(*strp = NULL, 0, fmt, ap);
  if((size < 0) || ((*strp = malloc(size+1)) == NULL))
    return -1;
  
  return vsprintf(*strp, fmt, ap);
}
#endif // !HAVE_VASPRINTF

#if !HAVE_ASPRINTF
int asprintf(char **strp, const char *fmt, ...) {
  int retval;
  va_list ap;
  va_start(ap, fmt);
  retval = vasprintf(strp, fmt, ap);
  va_end(ap);
  return retval;
}
#endif // !HAVE_ASPRINTF

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
  char **args; // the remaining args from command line
  int arg_num; // and their number
} arg_t;

static SCM 
set_window_title_x(SCM title) {
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
  EXPORT_PROCEDURE("set-window-title!", 1, 0, 0, set_window_title_x);
#undef EXPORT_PROCEDURE
}

#define RESOURCE(pair) ((struct list *) pair)->data
#define RELEASE_PROC(pair) ((struct list *) pair)->next

static struct list *resources = NULL;

void *
remember_to_release(void *resource, void (*release)(void *)) {
  if(resource) {
    resources = cons(cons(resource, (void *) release), resources);
  }
  else {
    WARN("%p passed as a resource to release, ignoring", resource);
  }
  return resource;
}

static void
release_resources() {
  while(resources) {
    struct list *next = resources->next;
    void *resource = RESOURCE(resources->data);
    void (*release)(void *) = (void(*)(void *)) RELEASE_PROC(resources->data);
    free(resources->data);
    (*release)(resource);
    free(resources);
    resources = next;
  }
}

#undef RELEASE_PROC
#undef RESOURCE

static void 
finish(arg_t *arg) {
  scm_call_1(exit_procedure, scm_from_locale_string(arg->outfile));
  scm_gc();
#ifdef USE_SDL_MIXER
  if(arg->sound) {
    audio_finish();
  }
#endif // USE_SDL_MIXER
  SDL_WM_GrabInput(SDL_GRAB_OFF);
  SDL_ShowCursor(SDL_ENABLE);
  SDL_Quit();
  release_resources();
}

static void 
setup_port_encodings() {
  SCM utf8 = scm_from_utf8_string("UTF-8");
  scm_set_port_encoding_x(scm_current_output_port(), utf8);
  scm_set_port_encoding_x(scm_current_input_port(), utf8);
  scm_set_port_encoding_x(scm_current_error_port(), utf8);
  eval("(fluid-set! %default-port-encoding \"UTF-8\")");
}

static void
bind_labels_to_args(char *args[], int arg_num) {
  // we want the command line arguments to be available
  // in the $* variable, and each individual argument
  // -- in $0, $1, $2, ...
  int i;
  SCM list = SCM_EOL;
  for (i = arg_num - 1; i >= 0; --i) {
    char *label;
    SCM arg = scm_from_locale_string(args[i]);
    list = scm_cons(arg, list);
    TRY(asprintf(&label, "$%d", i));
    scm_c_define(label, arg);
    free(label);
  }
  scm_c_define("$*", list);
}

static void 
init(arg_t *arg) {
  // we need to store the argument persistently in order
  // to be able to bind it in the local _finish function below
  static arg_t *_arg = NULL;
  symbols_init();
  setup_port_encodings();
  bind_labels_to_args(arg->args, arg->arg_num);

  exit_procedure = noop;
  scm_c_define_module("slayer", export_symbols, NULL);
  video_init(arg->w, arg->h, arg->video_mode);

  input_init();
  timer_init();

  scm_c_use_module("slayer");

#ifdef USE_OPENGL
  if(arg->video_mode & SDL_OPENGL) {
    scm_c_use_module("slayer 3d");
  }
#endif // USE_OPENGL

#ifdef USE_SDL_MIXER
  if(arg->sound) {
    audio_init();
    scm_c_use_module("slayer audio");
  }
#endif // USE_SDL_MIXER

#ifdef ENABLE_VECTOR_GRAPHICS
  drawing_init();
#endif // ENABLE_VECTOR_GRAPHICS
  
  // these calls should be moved to separate libraries
  image_init();
  scm_c_use_module("slayer image");
  font_init();
  scm_c_use_module("slayer font");

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
  _arg = arg;
  void _finish() {
    finish(_arg);
  }
  atexit(_finish);
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
  printf("built with Guile %d.%d.%d, SDL %d.%d.%d\n", 
	 SCM_MAJOR_VERSION, SCM_MINOR_VERSION, SCM_MICRO_VERSION,
	 SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL);

  printf("%s\n", COPYRIGHT);
}

static void
show_usage(const char *program, const char *file_name) {
#define TABS "\t\t\t\t" //"                             "
  printf("Usage: %s [OPTIONS] [PROGRAM]\n", program);
  printf("Run specified program in SLAYER environment. "
	 "Unless PROGRAM is given, it\n");
  printf("attempts to run a program named %s%s"
	 " from current working directory.\n\n", file_name, SLAYER_SUFFIX);
  printf("Available options:\n");
  printf("  PROGRAM,               \texecute PROGRAM (guile scheme source)\n");

#if defined(USE_OPENGL) || defined(USE_SDL_MIXER)
  printf("  -e EXT, --extension EXT\tload extension EXT."
	 " Currently available\n");
  printf(TABS "extensions are:\n");
#ifdef USE_OPENGL
  printf(TABS "  * 3d      use OpenGL    (%s by default)\n",
#ifdef ENABLE_DEFAULT_3D
	 "enabled"
#else // !ENABLE_DEFAULT_3D
	 "disabled"
#endif // !ENABLE_DEFAULT_3D
	 );
#endif // USE OPENGL

#ifdef USE_SDL_MIXER
  printf(TABS "  * sound   use SDL_mixer (%s by default)\n",
	 "enabled");
#endif  // USE_SDL_MIXER
  printf("  -d EXT,  --disable EXT\tdisable extension EXT\n");
#endif  // USE_OPENGL || USE_SDL_MIXER
  printf("  -w N,        --width N\tset initial window width to N\n");
  printf("  -h N,       --height N\tset initial window height to N\n");
  printf("  -r,        --resizable\tallow user to resize the window\n");
  printf(TABS "by dragging its edge\n");
  printf("  -f,       --fullscreen\trun SLAYER in fullscreen mode\n");
  printf("  --help\t\t\tdisplay this help and exit\n");
  printf("  --version\t\t\tdisplay version information and exit\n");
  printf("\n");
  printf("Report bugs to: %s\n", PACKAGE_BUGREPORT);
  printf("For more information, type ``info slayer''\n");
#undef TABS
}

static arg_t *
process_command_line_options(int argc, 
			     char *argv[], 
			     arg_t *arg, 
			     const char *exec_file_name) {
  int option_index;
  static struct option long_options[] = {
    {"help",      no_argument,       0,  0 },
    {"version",   no_argument,       0,  0 },
    {"force-recompile", no_argument, 0, 'R'},
    {"output",    required_argument, 0, 'o'},
    {"extension", required_argument, 0, 'e'},
    {"disable",   required_argument, 0, 'd'},
    {"nosound",   no_argument,       0,  0 },
    {"resizable", no_argument,       0, 'r'},
    {"fullscreen",no_argument,       0, 'f'},
    {"width",     required_argument, 0, 'w'},
    {"height",    required_argument, 0, 'h'},
    {0,           0,                 0,  0 }
  };
  
  int opt;
  while ((opt = getopt_long(argc, argv, "o:w:h:rfe:d:R",
			    long_options, &option_index)) != -1) {
    switch (opt) {
    case 0:
      if(0) {}
#ifdef USE_SDL_MIXER
      else if(!strcmp(long_options[option_index].name, "nosound")) {
	arg->sound = 0;
      }
#endif // USE_SDL_MIXER
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
    case 'R': // force recompile
      putenv("GUILE_AUTO_COMPILE=fresh");
      break;
    case 'o': // output file
      TRY(asprintf(&arg->outfile, "%s", optarg));
      break;
    case 'w': // screen width
      arg->w = atoi(optarg);
      break;
    case 'h': // screen height
      arg->h = atoi(optarg);
      break;
    case 'e': // enable extensions
      if(0) {}
#ifdef USE_OPENGL
      else if(!strcmp(optarg, "3d")) {
	arg->video_mode |= SDL_OPENGL;
      }
#endif // USE_OPENGL
      else {
	WARN("unknown extension: %s", optarg);
      }
      break;
    case 'd': // disable extensions
      if(0) {}
#ifdef USE_OPENGL
      else if(!strcmp(optarg, "3d")) {
	arg->video_mode &= ~SDL_OPENGL;
      }
#endif // USE_OPENGL
#ifdef USE_SDL_MIXER
      else if(!strcmp(optarg, "sound")) {
	arg->sound = 0;
      }
#endif // USE_SDL_MIXER
      else {
	WARN("unknown extension: %s", optarg);
      }
      break;
    case 'r': // resizable
      arg->video_mode |= SDL_RESIZABLE;
      break;
    case 'f': // full screen
      arg->video_mode |= SDL_FULLSCREEN;
      break;
      
    default:
      break;
    }
  }
  if (optind < argc) {
    TRY(asprintf(&arg->infile, "%s", argv[optind]));
    arg->arg_num = argc - optind;
    arg->args = &argv[optind];
  }
  return arg;
}

static inline char *
substring(const char *string, int start, int end) {
  int n = end - start;
  char *copy = malloc(n+1);
  strncpy(copy, &string[start], n);
  copy[n] = 0;
  return copy;
}

#ifdef __MINGW32__

static inline char *
directory(const char *path) {
  int i, end = strlen(path);
  for (i = end-1; i > 0; --i) {
    if ((path[i] == '/') || (path[i] == '\\')) {
      end = i;
      break;
    }
  }
  return substring(path, 0, end);
}

static inline bool
is_absolute_path(const char *path) {
  size_t i, l = strlen(path);
  for (i = 0; i < l; ++i) {
    if (path[i] == ':') {
      return true;
    }
  }
  return false;
}

#endif // __MINGW32__

char *
base(char *filename) {
  int i, start = 0, end = strlen(filename);
  // discard info about directory
  for(i = 0; filename[i]; ++i) {
    if(filename[i] == '/' || filename[i] == '\\') {
      start = i+1;
    }
  }
  // remove possible extensions
  for(i = end - 1; i > 0; --i) {
    switch(filename[i]) {
    case '.':
      end = i;
      /* FALLTHROUGH */
    case '/':
    case '\\':
    case ':':
      goto end;
    }
  }
  end:
  return substring(filename, start, end); 
}

int 
main(int argc, char *argv[]) {

  static arg_t arg = {
    .infile = NULL,
    .outfile = NULL,
    .w = 0,
    .h = 0,
    .video_mode = SDL_HWSURFACE | SDL_DOUBLEBUF,
    .sound = 1,
    .args = NULL,
    .arg_num = 0
  };

  // get the name of current file, skipping any slashes
  char *filename = (char *) REMEMBER_TO_FREE(base(argv[0]));

#ifdef ENABLE_DEFAULT_3D
  arg.video_mode |= SDL_OPENGL;
#endif // ENABLE_DEFAULT_3D

  arg = *process_command_line_options(argc, argv, &arg, filename);

#ifdef NDEBUG
  putenv("GUILE_WARN_DEPRECATED=no");
#else // !NDEBUG
  putenv("GUILE_WARN_DEPRECATED=detailed");
#endif // !NDEBUG

  putenv("LC_ALL=C.UTF8"); // discard locale

  if (!arg.infile) {
    arg.infile = malloc(strlen(filename) + strlen(SLAYER_SUFFIX) + 1);
    sprintf(arg.infile, "%s" SLAYER_SUFFIX, filename);
  }
  REMEMBER_TO_FREE(arg.infile);

  if (!arg.outfile) {
    arg.outfile = malloc(strlen(argv[0]) + strlen(SLAYER_SUFFIX) + 1);
    sprintf(arg.outfile, "/dev/null");//"%s" SLAYER_SUFFIX, argv[0]);
  }
  REMEMBER_TO_FREE(arg.outfile);

#ifdef __MINGW32__
  if (is_absolute_path(arg.infile)) {
    char *infile_directory = directory(arg.infile);
    SetCurrentDirectory(infile_directory);
    free(infile_directory);
  }

# if defined(XDG_CACHE_HOME) || defined(GULE_LOAD_PATH) \
  || defined(GUILE_LOAD_COMPILED_PATH)
#  error "local variable name conflicts with global macro definition"
# endif // XDG_CACHE_HOME || GUILE_LOAD_PATH || GUILE_LOAD_COMPILED_PATH

  char *slayer_directory;
  char *XDG_CACHE_HOME, *GUILE_LOAD_PATH, *GUILE_LOAD_COMPILED_PATH;
  if (is_absolute_path(argv[0])) {
    slayer_directory = (char *) REMEMBER_TO_FREE(directory(argv[0]));
  }
  else {
    slayer_directory = ".";
  }

  TRY(asprintf(&XDG_CACHE_HOME, "XDG_CACHE_HOME=%s/ccache", slayer_directory));
  REMEMBER_TO_FREE(XDG_CACHE_HOME);
  putenv(XDG_CACHE_HOME);

  TRY(asprintf(&GUILE_LOAD_PATH, "GUILE_LOAD_PATH=./;../;./guile-modules;"
	       "../guile-modules;%s;%s/guile-modules", 
	       slayer_directory, slayer_directory));
  REMEMBER_TO_FREE(GUILE_LOAD_PATH);
  putenv(GUILE_LOAD_PATH);

  TRY(asprintf(&GUILE_LOAD_COMPILED_PATH, "GUILE_LOAD_COMPILED_PATH="
	       "%s/ccache;./ccache;../ccache", slayer_directory));
  REMEMBER_TO_FREE(GUILE_LOAD_COMPILED_PATH);
  putenv(GUILE_LOAD_COMPILED_PATH);

#else // !__MINGW32__
  putenv("GUILE_LOAD_PATH=.:./scum:../guile-modules:./guile-modules");
  putenv("LTDL_LIBRARY_PATH=.:./scum");
#endif // !__MINGW32__

  if (arg.w == 0) {
    arg.w = 640;
  }

  if (arg.h == 0) {
    arg.h = 480;
  }

  OUT("infile = %s, outfile = %s, w = %d, h = %d", arg.infile, arg.outfile, 
      arg.w, arg.h);

  //check this out:
  //GC_allow_register_threads();

  scm_with_guile((void *(*)(void *))&io, &arg);

  return 0;
}
