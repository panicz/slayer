#include "slayer.h"
#include <SDL/SDL_audio.h>
#include <SDL/SDL_mixer.h>
#include "extend.h"
#include "utils.h"
#include "symbols.h"

scm_t_bits sound_tag;

static Mix_Music *now_playing = NULL;
static Mix_Chunk *used_channels[MIX_CHANNELS];

struct list {
  void *data;
  struct list *next;
};

static inline
struct list *cons(void *data, struct list *next) {
  struct list *cell = malloc(sizeof(struct list));
  cell->data = data;
  cell->next = next;
  return cell;
}

static struct list *purge_music = NULL;

enum {
  SOUND = 1,
  MUSIC = 2
};

#define SOUND_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)			\
  MDEF_CONDITIONAL_ASSIGN(SOUND, sound_tag, scm_var, Mix_Chunk *, c_var, d_val)

#define MUSIC_CONDITIONAL_ASSIGN(scm_var, c_var, d_val)			\
  MDEF_CONDITIONAL_ASSIGN(MUSIC, sound_tag, scm_var, Mix_Music *, c_var, d_val)


static void
music_finish() {
  now_playing = NULL;

  while(purge_music) {
    struct list *next = purge_music->next;
    Mix_Music *music = (Mix_Music *) purge_music->data;
    Mix_FreeMusic(music);
    free(purge_music);
    purge_music = next;
  }
  
}


static SCM
load_sound(SCM path) {
  SCM smob;
  char *filename = as_c_string(path);
  if(filename == NULL)
    return SCM_BOOL_F;
  Mix_Chunk *sound = Mix_LoadWAV(filename);
  free(filename);
  SCM_NEWSMOB(smob, sound_tag, sound);
  SCM_SET_SMOB_FLAGS(smob, SOUND);
  return smob;
}

static SCM
load_music(SCM path) {
  SCM smob;
  char *filename = as_c_string(path);
  if(filename == NULL)
    return SCM_BOOL_F;
  Mix_Music *music = Mix_LoadMUS(filename);
  free(filename);
  SCM_NEWSMOB(smob, sound_tag, music);
  SCM_SET_SMOB_FLAGS(smob, MUSIC);
  return smob;
}

static SCM
play_music_x(SCM x_music, SCM x_repeats) {
  MUSIC_CONDITIONAL_ASSIGN(x_music, music, SCM_BOOL_F);
  int repeats;
  if (x_repeats == SCM_UNDEFINED) {
    repeats = 0;
  }
  else {
    if (scm_is_integer(x_repeats)) {
      repeats = scm_to_int(x_repeats);
    }
    else {
      repeats = indeed(x_repeats) ? -1 : 0;
    }
  }
  now_playing = music;
  Mix_PlayMusic(music, repeats);
  Mix_HookMusicFinished(music_finish);
  return SCM_UNSPECIFIED;
}

static SCM 
pause_music_x() { 
  Mix_PauseMusic(); 
  return SCM_UNSPECIFIED;
}

static SCM 
resume_music_x() { 
  Mix_ResumeMusic(); 
  return SCM_UNSPECIFIED;
}


static void
export_symbols(void *unused) {
#define EXPORT_PROCEDURE(name, required, optional, rest, proc) \
  scm_c_define_gsubr(name,required,optional,rest,(scm_t_subr)proc); \
  scm_c_export(name,NULL);

  EXPORT_PROCEDURE("load-sound", 1, 0, 0, load_sound);
  EXPORT_PROCEDURE("load-music", 1, 0, 0, load_music);
  EXPORT_PROCEDURE("play-music!", 1, 1, 0, play_music_x);
  EXPORT_PROCEDURE("pause-music!", 0, 0, 0, pause_music_x);
  EXPORT_PROCEDURE("resume-music!", 0, 0, 0, resume_music_x);


#undef EXPORT_PROCEDURE
}

static size_t
free_sound(SCM sound_smob) {
  return 0;
  scm_assert_smob_type(sound_tag, sound_smob);  
  int type = SCM_SMOB_FLAGS(sound_smob);
  if(type == SOUND) {
    
  }
  else if(type == MUSIC) {
    Mix_Music *music = (Mix_Music *) SCM_SMOB_DATA(sound_smob);
    if(music != now_playing) {
      Mix_FreeMusic(music);
    }
    else {
      purge_music = cons((void *) music, purge_music);
    }
  }
  return 0;
}

void
audio_finish() {
  Mix_CloseAudio();
  Mix_Quit();
  SDL_QuitSubSystem(SDL_INIT_AUDIO);
}

void
audio_init() {
  TRY_SDL(SDL_InitSubSystem(SDL_INIT_AUDIO));

  //Mix_Init(MIX_INIT_OGG | MIX_INIT_MP3);

  Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 4096);

  int i;
  for(i = 0; i < NELEMS(used_channels); ++i) {
    used_channels[i] = NULL;
  }

  sound_tag = scm_make_smob_type("sound", sizeof(void *));
  scm_set_smob_free(sound_tag, free_sound);
  
  scm_c_define_module("slayer", export_symbols, NULL);
}
