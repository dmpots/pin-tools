TOP=..
TOOL_ROOTS=ibdetails
EXTRA=TOP=$(TOP) TOOL_ROOTS=$(TOOL_ROOTS) $(IMGFILT_EXTRA)

all: pintool

include $(TOP)/imgfilt/imgfilt.inc

pintool: $(IMGFILT_LIB)
	make -f $(TOP)/makefile.pin $(EXTRA)

clean:
	rm -f test Test.hi Test.o *.log *.out *.out.*
	make -f $(TOP)/makefile.pin $(EXTRA) clean
