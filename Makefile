CXX = g++
CXXFLAGS = -std=c++20 -Wall -pedantic
DEBUGFLAGS = -g -O0
LDFLAGS = -lz -lglog # google logging, zlib

all: beamparser

beamparser: beamparser.o exceptions.o external_term.o execution.o
	$(CXX) -o $@ $^ $(LDFLAGS)

beamparser.o: beamparser.cpp exceptions.h op_arity.h beam_defs.h external_term.h
	$(CXX) $(CXXFLAGS) -c $<

exceptions.o: exceptions.cpp exceptions.h
	$(CXX) $(CXXFLAGS) -c $<

external_term.o: external_term.cpp external_term.h
	$(CXX) $(CXXFLAGS) -c $<

execution.o: execution.cpp beam_defs.h
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f *.o beamparser

debug: CXXFLAGS += $(DEBUGFLAGS)
debug: beamparser

.PHONY: clean
