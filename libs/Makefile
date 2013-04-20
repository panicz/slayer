all: physics.so

physics.so:
	cd physics && make # && cp $@ ..

clean:
	rm -f *.o *.so *.go *~
	cd physics && make clean
	cd legacy && make clean
	cd paradigm && make clean
