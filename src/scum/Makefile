all: physics.so

.PHONY:	physics.so clean

physics.so:
	cd physics && make # && cp $@ ..

clean:
	rm -f *.o *.go *~
	cd physics && make clean
