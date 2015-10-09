# User options
FFLAGS="-openmp"
#FFLAGS="-openmp -C -g -O0"

COPTIONS = '-openmp'
#COPTIONS = '-openmp -g -O0'

# Set default target
default: lib

SOURCES = msg.ftn90  utils.ftn90  vgrid_descriptors.ftn90 vgrid.c

lib:	${SOURCES}
	d.compile -o libdescrip.a -optc =$(COPTIONS) -optf =$(FFLAGS) -src $(SOURCES)

clean:
	rm -f *.f90 *.o *.mod ../lib/lib*.a
