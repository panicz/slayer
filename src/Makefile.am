# flags set by pkg-config
AM_CFLAGS = $(GUILE_CFLAGS) $(SDL_CFLAGS) $(OPENGL_CFLAGS)
AM_CXXFLAGS = $(AM_CFLAGS)
AM_LDFLAGS = $(GUILE_LIBS) $(SDL_LIBS) $(OPENGL_LIBS) $(SDL_MIXER_LIBS) \
	$(SDL_IMAGE_LIBS)

bin_PROGRAMS = slayer
slayer_SOURCES = extend.h file.c font.c image.c input.c slayer.c slayer.h \
	 symbols.c symbols.h utils.h video.c video.h 

if OPENGL
slayer_SOURCES += audio.c
endif

if SDL_MIXER
slayer_SOURCES += 3d.cc vmx.hh
endif
