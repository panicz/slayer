CC = gcc
CFLAGS = -Wall `sdl-config --cflags` `guile-config compile` # -DUSE_EVENT_HANDLER
LIBS = `guile-config link` `sdl-config --libs` -lSDL_net -lSDL_image -lSDL_mixer -lc

all: slayer

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

OBJECTS = input.o slayer.o video.o file.o timer.o widgets.o image.o

slayer:	$(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(LIBS)

clean:
	rm slayer *.o *~
