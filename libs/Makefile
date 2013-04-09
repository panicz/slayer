CC = gcc
CXX = g++
CFLAGS = `pkg-config --cflags guile-2.0 sdl` -I. -I..
LIBODE = `pkg-config --libs ode`

all: libguile-ode.so

libguile-ode.so: ode.c
	$(CC) $(CFLAGS) -shared -Wl,-soname,$@ -o $@ -fPIC $< $(LIBODE)
