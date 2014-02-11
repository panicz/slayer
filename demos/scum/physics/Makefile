CC = gcc
CXX = g++ -std=c++0x
CFLAGS = `pkg-config --cflags guile-2.0 ode` -I. -I../../../src -Wall -O3
LIBODE = `pkg-config --libs ode`

.PHONY:	clean
all: physics.so

physics.so: physics.cc physics.hh body.cc joint.cc sim.cc rig.cc
	$(CXX) $(CFLAGS) -shared -Wl,-soname,$@ -o $@ -fPIC physics.cc $(LIBODE)
clean:
	rm -f *.so *~
