TOP=..
TOOL_ROOTS=opcodemix
EXTRA=TOP=$(TOP) TOOL_ROOTS=$(TOOL_ROOTS)
all: pintool

pintool:
	make -f $(TOP)/makefile.pin $(EXTRA)

clean:
	make -f $(TOP)/makefile.pin $(EXTRA) clean
