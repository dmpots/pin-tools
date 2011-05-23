TOP=..
TOOL_ROOTS=ibmix
EXTRA=TOP=$(TOP) TOOL_ROOTS=$(TOOL_ROOTS)
GHC=ghc

all: pintool

pintool:
	make -f $(TOP)/makefile.pin $(EXTRA)

clean:
	rm -f test Test.hi Test.o *.log *.out *.out.*
	make -f $(TOP)/makefile.pin $(EXTRA) clean
