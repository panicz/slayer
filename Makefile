CC = gcc
CXX = g++
CFLAGS = -Wall `pkg-config --cflags sdl guile-2.0` -DUSE_OPENGL -lGL -lGLU # -DUSE_EVENT_HANDLER -pg 
LIBS = `pkg-config --libs sdl guile-2.0` -lSDL_image -lSDL_ttf -lc -lGL -lGLU -lSDL_mixer # -pg
OBJCOPY = objcopy

ARCH := $(shell uname -m)

ifeq ($(ARCH),x86_64)
OBJ_BINARCH = i386:x86-64
OBJ_TARGET = elf64-x86-64
else 
OBJ_BINARCH = i386
OBJ_TARGET = elf32-i386
endif

.PHONY:	clean
all: slayer libraries

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@
.cc.o:
	$(CXX) $(CFLAGS) -c $< -o $@

OBJECTS = input.o slayer.o symbols.o video.o file.o image.o font.o 3d.o audio.o

slayer:	$(OBJECTS)
	$(CXX) $(CFLAGS) -o $@ $(OBJECTS) $(LIBS)
libraries:	
	cd libs && make

clean:
	rm -f slayer *.o *~ widgets/*.go widgets/*~ extra/*~ *.go extra/*.go
	cd libs && make clean

again:
	make clean
	make -j
