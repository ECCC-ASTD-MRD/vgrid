# User options
FFLAGS="-openmp"
FFLAGS_DEBUG="=-openmp =-C =-g =-O0"

COPTIONS = "-openmp"
COPTIONS_DEBUG = "=-openmp =-g =-O0"

# Set default target
default: libdescrip.a libdescrip_debug.a

SOURCES = msg.ftn90 utils.ftn90 vgrid_descriptors.ftn90 vgrid.c

libdescrip.a: ${SOURCES}
	rm -f libdescrip.a && d.compile -o libdescrip.a -optc $(COPTIONS) -optf $(FFLAGS) -src $(SOURCES)

libdescrip_debug.a: ${SOURCES}
	rm -f libdescrip_debug.a &&  d.compile -o libdescrip_debug.a -optc $(COPTIONS_DEBUG) -optf $(FFLAGS_DEBUG) -src $(SOURCES)

clean:
	rm -f *.f90 *.o *.mod lib*.a ../lib*.a

