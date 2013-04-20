CC = gcc
CXX = g++ -std=c++0x
CFLAGS = `pkg-config --cflags guile-2.0 sdl ode` -I. -I../.. -Wall
LIBODE = `pkg-config --libs ode`

all: physics.so

physics.so: physics.cc physics.h
	$(CXX) $(CFLAGS) -shared -Wl,-soname,$@ -o $@ -fPIC physics.cc $(LIBODE)
clean:
	rm -f *.so *~
