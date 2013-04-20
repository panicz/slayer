CC = gcc
CXX = g++ -std=c++0x
CFLAGS = `pkg-config --cflags guile-2.0 sdl ode` -I. -I.. -Wall
LIBODE = `pkg-config --libs ode`

all: libguile-ode.so sim physics.so

libguile-ode.so: ode.cc
	$(CXX) $(CFLAGS) -shared -Wl,-soname,$@ -o $@ -fPIC $< $(LIBODE)

physics.so: physics.cc
	$(CXX) $(CFLAGS) -shared -Wl,-soname,$@ -o $@ -fPIC $< $(LIBODE)


sim: sim.cc
	$(CXX) $(CFLAGS) -o $@ $< $(LIBODE)
