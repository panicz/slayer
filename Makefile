CC = gcc
CXX = g++
CFLAGS = -Wall `pkg-config --cflags sdl guile-2.0` -DUSE_OPENGL -lGL -lGLU # -DUSE_EVENT_HANDLER -pg 
LIBS = `pkg-config --libs sdl guile-2.0` -lSDL_image -lSDL_ttf -lc -lGL -lGLU # -lSDL_net -lSDL_mixer -pg
OBJCOPY = objcopy

ARCH := $(shell uname -m)

ifeq ($(ARCH),x86_64)
OBJ_BINARCH = i386:x86-64
OBJ_TARGET = elf64-x86-64
else 
OBJ_BINARCH = i386
OBJ_TARGET = elf32-i386
endif

all: slayer libraries

scm/defs.o:	scm/defs.scm
	$(OBJCOPY) -I binary -O $(OBJ_TARGET) -B $(OBJ_BINARCH) $< $@
scm/widgets.o:	scm/widgets.scm
	$(OBJCOPY) -I binary -O $(OBJ_TARGET) -B $(OBJ_BINARCH) $< $@
scm/3d-view.o:	scm/3d-view.scm
	$(OBJCOPY) -I binary -O $(OBJ_TARGET) -B $(OBJ_BINARCH) $< $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@
.cc.o:
	$(CXX) $(CFLAGS) -c $< -o $@

OBJECTS = input.o slayer.o symbols.o video.o file.o widgets.o image.o font.o scm/defs.o scm/widgets.o scm/3d-view.o 3d.o

slayer:	$(OBJECTS)
	$(CXX) $(CFLAGS) -o $@ $(OBJECTS) $(LIBS)
libraries:	
	cd libs && make

clean:
	rm -f slayer *.o *~ scm/*.o scm/*~ extra/*~ *.go scm/*.go extra/*.go libs/*.so

again:
	make clean
	make -j
