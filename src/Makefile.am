# flags set by pkg-config
AM_CFLAGS = $(GUILE_CFLAGS) $(SDL_CFLAGS) $(OPENGL_CFLAGS)
AM_CXXFLAGS = $(AM_CFLAGS)
AM_LDFLAGS = $(GUILE_LIBS) $(SDL_LIBS) $(OPENGL_LIBS) $(SDL_MIXER_LIBS) \
	$(SDL_IMAGE_LIBS)

bin_PROGRAMS = slayer
slayer_SOURCES = 3d.cc audio.c extend.h file.c font.c image.c input.c \
	 slayer.c slayer.h symbols.c symbols.h utils.h video.c video.h vmx.hh

show:
	echo $(AM_CXXFLAGS)
