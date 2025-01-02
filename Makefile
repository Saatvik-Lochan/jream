CXX=g++
CCFlags=-std=c++20 -Wall -Pedantic

all: beamparser
beamparser: beamparser.o exceptions.o
%.o: %.cpp

clean:
	rm -f *.o 
