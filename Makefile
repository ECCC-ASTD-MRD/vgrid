# User options
# see setup* files

# Override incorrect implicits
SUBDIRS = lib examples tests

all:
	for dir in $(SUBDIRS) ; do\
	  (cd $$dir && $(MAKE) $(MAKEFLAGS) RMN=$(RMN)) ;\
	done;

clean:
	for dir in $(SUBDIRS); do \
	  (cd $$dir && $(MAKE) $@ $(MAKEFLAGS)) ;\
	done;

distclean:
	for dir in $(SUBDIRS) ssm ; do \
	  (cd $$dir && $(MAKE) $@ $(MAKEFLAGS)) ;\
	done;

release: all
	set -e ; \
	if [ -z "$(VERSION)" ] ; then \
	  echo "VERSION= is a mandatory argument"; \
          exit 1 ; \
        fi; \
	for comp in \$(COMPILERS_$(BASE_ARCH)) ; do \
	  $(RELEASE_SCR) $$comp $(VERSION); \
        done
