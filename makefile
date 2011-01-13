
SUBDIRS = trace opcodemix jumpmix regmix
.PHONY: all $(SUBDIRS)

all: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	for dir in $(SUBDIRS); do \
               $(MAKE) -C $$dir clean; \
  done

