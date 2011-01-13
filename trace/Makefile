TOP=..
TOOL_ROOTS=trace
EXTRA=TOP=$(TOP) TOOL_ROOTS=$(TOOL_ROOTS)
GHC=ghc

all: pintool testprog

testprog:
	$(GHC) $(HFLAGS) Test.hs -o test

pintool:
	make -f $(TOP)/makefile.pin $(EXTRA)

clean:
	rm -f test Test.hi Test.o *.log *.out *.out.*
	make -f $(TOP)/makefile.pin $(EXTRA) clean
