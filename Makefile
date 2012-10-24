CC = gcc
CFLAGS = -Wall -pg `sdl-config --cflags` `guile-config compile` -DUSE_OPENGL -lGL -lGLU # -DUSE_EVENT_HANDLER
LIBS = `guile-config link` `sdl-config --libs` -lSDL_image -lSDL_ttf -lc -lGL -lGLU # -lSDL_net -lSDL_mixer -pg
OBJCOPY = objcopy


ARCH := $(shell uname -m)

ifeq ($(ARCH),x86_64)
OBJ_BINARCH = i386:x86-64
OBJ_TARGET = elf64-x86-64
else 
OBJ_BINARCH = i386
OBJ_TARGET = elf32-i386
endif


all: slayer

scm/widgets.o:	scm/widgets.scm
	$(OBJCOPY) -I binary -O $(OBJ_TARGET) -B $(OBJ_BINARCH) $< $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

OBJECTS = input.o slayer.o video.o file.o timer.o widgets.o image.o font.o scm/widgets.o

slayer:	$(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LIBS)

clean:
	rm -f slayer *.o *~ scm/*.o scm/*~ extra/*~
