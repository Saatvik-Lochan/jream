CXX = g++
CXXFLAGS = -std=c++20 -Wall -pedantic
DEBUGFLAGS = -g -O0

all: beamparser

beamparser: beamparser.o exceptions.o
	$(CXX) -o $@ $^

beamparser.o: beamparser.cpp exceptions.h op_arity.h
	$(CXX) $(CXXFLAGS) -c $<

exceptions.o: exceptions.cpp exceptions.h
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f *.o beamparser

debug: CXXFLAGS += $(DEBUGFLAGS)
debug: beamparser

.PHONY: clean
