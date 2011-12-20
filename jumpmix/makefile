TOP=..
TOOL_ROOTS=jumpmix
EXTRA=TOP=$(TOP) TOOL_ROOTS=$(TOOL_ROOTS) $(IMGFILT_EXTRA)

all: pintool

include $(TOP)/imgfilt/imgfilt.inc

pintool: $(IMGFILT_LIB)
	make -f $(TOP)/makefile.pin $(EXTRA)

clean:
	make -f $(TOP)/makefile.pin $(EXTRA) clean
