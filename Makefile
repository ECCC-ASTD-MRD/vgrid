# User options
FCFLAGS=
DEBUG_FLAGS='=-C -g'
RCOMPILE = r.compile
RELEASE_SCR = ./scripts/release.ksh
COMPILERS_AIX = Xlf12 xlf10
COMPILERS_AIX-powerpc7 = Xlf13
COMPILERS_Linux = pgi9xx
COMPILERS_Linux_x86-64 = pgi9xx svn_tag
VERSION = 

# Set default target
default: all

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) $(DEBUG_FLAGS) -src $<

OBJECTS = vgrid_descriptors.o vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o 

all: $(OBJECTS)

vgrid_descriptors.o: vgrid_descriptors.ftn90 vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o

clean:
	rm -f *.f90 *.o *.mod

release:
	set -e ; \
	if [ -z "$(VERSION)" ] ; then \
	  echo "VERSION= is a mandatory argument"; \
          exit 1 ; \
        fi; \
	for comp in \$(COMPILERS_$(BASE_ARCH)) ; do \
	  $(RELEASE_SCR) $$comp $(VERSION); \
        done
