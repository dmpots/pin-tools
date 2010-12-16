
.PHONEY: all clean pintool

all: addressMapReader indirectionAnalyzer pintool test

addressMapReader: addressMapReader.cpp

indirectionAnalyzer: indirectionAnalyzer.cpp

test: test.c

pintool: indirectionProfiler.cpp indirectionProfiler.hpp
	make -f Makefile.pin


clean:
	rm -fr addressMapReader indirectionAnalyzer
	make -f Makefile.pin clean