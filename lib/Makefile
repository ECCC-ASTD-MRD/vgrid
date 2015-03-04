# User options
FFLAGS=$(FFLAGS_INTEL)
#FFLAGS="$(FFLAGS_INTEL) -C -g"

# Set default target
default: lib

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .f .ftn90 .o
.ftn90.o:
	s.compile -includes . $(MY_INCLUDE_PATH) $(INCLUDE_PATH) -optf =$(FFLAGS) -src $<

OBJECTS = vgrid_descriptors.o vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o utils.o msg.o

all: Makefile.dep.mk $(OBJECTS)

include Makefile.dep.mk

lib: all
	ar cru libdescrip.a $(OBJECTS)

clean:
	rm -f *.f90 *.o *.mod lib*.a

distclean: clean
