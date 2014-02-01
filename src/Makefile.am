# flags set by pkg-config
AM_CFLAGS =  $(SDL_CFLAGS) $(GUILE_CFLAGS) $(OPENGL_CFLAGS) -Wall -Werror \
	$(AM_LDFLAGS)

AM_CXXFLAGS = $(AM_CFLAGS)

bin_PROGRAMS = slayer
slayer_SOURCES = extend.h file.c font.c image.c input.c slayer.c slayer.h \
	 symbols.c symbols.h timer.c utils.h video.c video.h 
noinst_HEADERS = scancode.c

if OPENGL
slayer_SOURCES += 3d.cc vmx.hh
endif

if SDL_MIXER
slayer_SOURCES += audio.c
endif

slayer_LDADD = $(SDL_LIBS) $(GUILE_LIBS) $(OPENGL_LIBS) $(SDL_MIXER_LIBS) \
	$(SDL_IMAGE_LIBS)
