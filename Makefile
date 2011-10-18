# User options
RELEASE_SCR = ./scripts/release.ksh
COMPILERS_AIX = Xlf12 xlf10
COMPILERS_AIX-powerpc7 = Xlf13
COMPILERS_Linux = pgi9xx
COMPILERS_Linux_x86-64 = pgi9xx svn_tag
VERSION = 

# Override incorrect implicits
SUBDIRS = lib bin examples tests

all:
	for dir in $(SUBDIRS) ; do\
	  (cd $$dir && $(MAKE) $(MAKEFLAGS)) ;\
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
