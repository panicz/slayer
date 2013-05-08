AM_CFLAGS = `pkg-config --cflags sdl guile-2.0 gl glu`
AM_CXXFLAGS = $(AM_CFLAGS)
AM_LDFLAGS = `pkg-config --libs sdl guile-2.0 gl glu SDL_image SDL_mixer`

bin_PROGRAMS = slayer
slayer_SOURCES = 3d.cc audio.c extend.h file.c font.c image.c input.c slayer.c slayer.h symbols.c symbols.h utils.h video.c video.h vmx.hh
