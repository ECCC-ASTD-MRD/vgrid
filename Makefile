# User options
FCFLAGS='=-C -g'
RCOMPILE = r.compile
RELEASE_SCR = ./scripts/release.ksh
COMPILERS_AIX = xlf10 Xlf12
COMPILERS_LINUX = pgi6xx pgi9xx svn_tag
VERSION = 

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -src $<

OBJECTS = vgrid_descriptors.o vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o 

all: $(OBJECTS)

vgrid_descriptors.o: vgrid_descriptors.ftn90 vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o

clean:
	rm -f *.f90 *.o *.mod

release:
	if [ -z "$(VERSION)" ] ; then \
	  echo "VERSION= is a mandatory argument"; \
          exit 1 ; \
        fi; \
	if [ `uname` = "AIX" ] ; then \
	  for comp in $(COMPILERS_AIX) ; do \
            $(RELEASE_SCR) $$comp $(VERSION); \
          done; \
        fi; \
	if [ `uname` = "Linux" ] ; then \
	  for comp in $(COMPILERS_LINUX) ; do \
            $(RELEASE_SCR) $$comp $(VERSION); \
          done; \
        fi
