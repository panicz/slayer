all: physics.so

.PHONY:	physics.so clean

physics.so:
	cd physics && make && cp $@ ..

clean:
	rm -f *.o *.go *.so *~
	cd physics && make clean
