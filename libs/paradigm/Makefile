CC = gcc
CXX = g++ -std=c++0x
CFLAGS = `pkg-config --cflags guile-2.0 sdl ode` -I. -I../.. -Wall
LIBODE = `pkg-config --libs ode`

all: sim

sim: sim.cc
	$(CXX) $(CFLAGS) -o $@ $< $(LIBODE)
clean:
	rm -f *.so *~
