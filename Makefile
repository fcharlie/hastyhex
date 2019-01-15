.POSIX:
CXX      = c++
CXXFLAGS  = -std=c++17 -pedantic -Wall -Wextra -O3 -march=native -funroll-loops
LDFLAGS =
LDLIBS  =

all: hastyhex

hastyhex: hastyhex.cc
	$(CXX) $(LDFLAGS) $(CXXFLAGS) -o $@ hastyhex.cc $(LDLIBS)

clean:
	rm -f hastyhex
