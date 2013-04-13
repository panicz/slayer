CC = gcc
CXX = g++
CFLAGS = `pkg-config --cflags guile-2.0 sdl ode` -I. -I..
LIBODE = `pkg-config --libs ode`

all: libguile-ode.so

libguile-ode.so: ode.cc
	$(CXX) $(CFLAGS) -Wall -shared -Wl,-soname,$@ -o $@ -fPIC $< $(LIBODE)
