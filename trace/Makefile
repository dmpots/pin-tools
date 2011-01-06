GHC=ghc

.PHONEY: all clean pintool

all: pintool testprog

testprog:
	$(GHC) $(HFLAGS) Test.hs -o test

pintool:
	make -f Makefile.pin

clean:
	rm -f test Test.hi Test.o *.log *.out *.out.*
	make -f Makefile.pin clean
