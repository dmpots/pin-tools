TOP=..
TOOL_ROOTS=ibdetails
EXTRA=TOP=$(TOP) TOOL_ROOTS=$(TOOL_ROOTS)

all: pintool

pintool:
	make -f $(TOP)/makefile.pin $(EXTRA)

clean:
	rm -f test Test.hi Test.o *.log *.out *.out.*
	make -f $(TOP)/makefile.pin $(EXTRA) clean
