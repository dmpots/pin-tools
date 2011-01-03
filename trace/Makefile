GHC=ghc

.PHONEY: all clean pintool

all: pintool test

test: Test.hs
	$(GHC) -fforce-recomp $< -o $@

pintool: trace.cpp
	make -f Makefile.pin

clean:
	rm -f test Test.hi Test.o
	make -f Makefile.pin clean
