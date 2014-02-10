AUTOMAKE_OPTIONS = subdir-objects

# flags set by pkg-config
AM_CFLAGS =  $(SDL_CFLAGS) $(GUILE_CFLAGS) $(OPENGL_CFLAGS) -Wall -Werror \
	$(AM_LDFLAGS)

AM_CXXFLAGS = $(AM_CFLAGS)

bin_PROGRAMS = slayer
slayer_SOURCES = extend.h file.c font.c image.c input.c slayer.c slayer.h \
	 symbols.c symbols.h timer.c utils.h video.c video.h 
noinst_HEADERS = scancode.c

if OPENGL
slayer_SOURCES += 3d.cc 3d.hh 3d/lights.cc 3d/arrays.cc 3d/color.cc \
	3d/transforms.cc 3d/buffers.cc vmx.hh vmx/math.hh vmx/v2.hh vmx/v3.hh \
	vmx/qt.hh vmx/m3x3.hh vmx/v4.hh vmx/m4x4.hh
endif

if SDL_MIXER
slayer_SOURCES += audio.c
endif

slayer_LDADD = $(SDL_LIBS) $(GUILE_LIBS) $(OPENGL_LIBS) $(SDL_MIXER_LIBS) \
	$(SDL_IMAGE_LIBS)
