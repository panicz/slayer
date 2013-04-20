CC = gcc
CXX = g++ -std=c++0x
CFLAGS = `pkg-config --cflags guile-2.0 sdl ode` -I. -I../.. -Wall
LIBODE = `pkg-config --libs ode`

all: physics.so

physics.so: physics.cc
	$(CXX) $(CFLAGS) -shared -Wl,-soname,$@ -o $@ -fPIC $< $(LIBODE)
