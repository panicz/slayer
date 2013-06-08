#include "slayer.h"
#include <SDL/SDL_audio.h>
#include <SDL/SDL_mixer.h>
#include "extend.h"
#include "utils.h"
#include "symbols.h"

scm_t_bits sound_tag;

static Mix_Music *now_playing = NULL;
static Mix_Chunk **used_channels;
static int nchannels;

struct list {
  void *data;
  struct list *next;
};

static inline struct list *
cons(void *data, struct list *next) {
  struct list *cell = malloc(sizeof(struct list));
  cell->data = data;
  cell->next = next;
  return cell;
}

// these are needed to sync with garbage collector
static struct list *purge_music = NULL;
static struct list *purge_sounds = NULL;

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

static void
channel_finish(int channel) {
  Mix_Chunk *sound = used_channels[channel];
  used_channels[channel] = NULL;

  if(!purge_sounds)
    return;

  int i;
  for(i = 0; i < nchannels; ++i) {
    if(used_channels[i] == sound) { // the sound is still being played on some
      return;                       // channel, so it shall not be deleted
    }
  }
  
  struct list *p, *prev = NULL;
  for(p = purge_sounds; p; p = p->next) {
    if(((Mix_Chunk *) p->data) == sound) {
      if(prev) {
	prev->next = p->next;
      }
      else {
	purge_sounds = p->next;
      }
      Mix_FreeChunk(sound);
      free(p);
      return;
    }
    prev = p;
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
play_sound_x(SCM x_sound, SCM x_repeats) {
  SOUND_CONDITIONAL_ASSIGN(x_sound, sound, SCM_BOOL_F);
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
  int channel = Mix_PlayChannel(-1, sound, repeats);
  //OUT("playing sound on channel %d", channel);
  scm_remember_upto_here_2(x_sound, x_repeats);
  return scm_from_int(channel);
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
  EXPORT_PROCEDURE("play-sound!", 1, 1, 0, play_sound_x);
  EXPORT_PROCEDURE("load-music", 1, 0, 0, load_music);
  EXPORT_PROCEDURE("play-music!", 1, 1, 0, play_music_x);
  EXPORT_PROCEDURE("pause-music!", 0, 0, 0, pause_music_x);
  EXPORT_PROCEDURE("resume-music!", 0, 0, 0, resume_music_x);

#undef EXPORT_PROCEDURE
}

static void
cond_expand_provide(void *unused) {
  eval("(cond-expand-provide (current-module) '(slayer-audio))");
}

static size_t
free_sound(SCM sound_smob) {
  scm_assert_smob_type(sound_tag, sound_smob);  
  int type = SCM_SMOB_FLAGS(sound_smob);
  if(type == SOUND) {
    Mix_Chunk *sound = (Mix_Chunk *) SCM_SMOB_DATA(sound_smob);
    struct list *p;

    // we don't want to add sound to the queue if it's already there
    for(p = purge_sounds; p; p = p->next) {
      if(((Mix_Chunk *) p->data) == sound)
	return 0;
    }
    purge_sounds = cons((void *) sound, purge_sounds);
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
  SDL_QuitSubSystem(SDL_INIT_AUDIO);
  free(used_channels);
}

void
audio_init() {
  TRY_SDL(SDL_InitSubSystem(SDL_INIT_AUDIO));
  Mix_OpenAudio(MIX_DEFAULT_FREQUENCY, MIX_DEFAULT_FORMAT, 2, 1024);

  nchannels = Mix_AllocateChannels(MIX_CHANNELS);
  OUT("There are %i channels allocated", nchannels);
  used_channels = (Mix_Chunk **) calloc(nchannels, sizeof(Mix_Chunk *));

  int i;
  for(i = 0; i < nchannels; ++i) {
    used_channels[i] = NULL;
  }

  Mix_ChannelFinished(channel_finish);
  Mix_HookMusicFinished(music_finish);

  sound_tag = scm_make_smob_type("sound", sizeof(void *));
  scm_set_smob_free(sound_tag, free_sound);
  
  scm_c_define_module("slayer audio", export_symbols, NULL);
  scm_c_define_module("slayer", cond_expand_provide, NULL);
}
