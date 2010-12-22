GHC=ghc

.PHONEY: all clean pintool

all: addressMapReader indirectionAnalyzer pintool test.haskell

addressMapReader: addressMapReader.cpp

indirectionAnalyzer: indirectionAnalyzer.cpp

test.haskell: Test.hs
	$(GHC) -fforce-recomp $< -o $@

pintool: indirectionProfiler.cpp indirectionProfiler.hpp
	make -f Makefile.pin


clean:
	rm -fr addressMapReader indirectionAnalyzer test Test.hi Test.o
	make -f Makefile.pin clean
