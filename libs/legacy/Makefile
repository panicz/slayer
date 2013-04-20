CC = gcc
CXX = g++ -std=c++0x
CFLAGS = `pkg-config --cflags guile-2.0 sdl ode` -I. -I.. -Wall
LIBODE = `pkg-config --libs ode`

all: libguile-ode.so

libguile-ode.so: ode.cc
	$(CXX) $(CFLAGS) -shared -Wl,-soname,$@ -o $@ -fPIC $< $(LIBODE)
