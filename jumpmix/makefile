TOP=..
TOOL_ROOTS=jumpmix
EXTRA=TOP=$(TOP) TOOL_ROOTS=$(TOOL_ROOTS)
all: pintool

pintool:
	make -f $(TOP)/makefile.pin $(EXTRA)

clean:
	make -f $(TOP)/makefile.pin $(EXTRA) clean
