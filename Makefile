# User options
# see setup* files

# Override incorrect implicits
SUBDIRS = lib examples tests

clean:
	for dir in $(SUBDIRS); do \
	  (cd $$dir && $(MAKE) $@ $(MAKEFLAGS)) ;\
	done;
