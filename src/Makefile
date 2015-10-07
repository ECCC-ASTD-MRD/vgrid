# User options
FFLAGS="$(FFLAGS_INTEL) -openmp"
#FFLAGS="$(FFLAGS_INTEL) -openmp -C -g -O0"

COPTIONS        := -O 2 -optc '=-openmp'
#COPTIONS       := -O 2 -optc '=-openmp -g'
#COPTIONS       := -O 0 -optc '=-openmp -g'

# Set default target
default: lib

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .f .ftn90 .o
.ftn90.o:
	s.compile -optf =$(FFLAGS) -src $<
.c.o:
	s.compile -optf =$(COPTIONS) -src $<

OBJECTS = utils.o msg.o vgrid_descriptors.o vgrid.o

all: Makefile.dep.mk $(OBJECTS)

Makefile.dep.mk:
	ls *.ftn90 *.c | s.dependencies.pl -OMakefile.dep.mk \;

include Makefile.dep.mk

lib: all
	ar cru ../lib/libdescrip.a $(OBJECTS)

clean:
	rm -f *.f90 *.o *.mod ../lib/lib*.a

distclean: clean
