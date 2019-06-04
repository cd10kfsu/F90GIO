all:
	mkdir build
	. config/env.sh; cd build; cmake ..; make; make test

