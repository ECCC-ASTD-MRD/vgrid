# User options
FCFLAGS=
#DEBUG_FLAGS='=-C -g'
#RCOMPILE = r.compile

# Set default target
default: lib

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .f .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -includes . $(MY_INCLUDE_PATH) $(INCLUDE_PATH) -optf $(FCFLAGS) $(DEBUG_FLAGS) -src $<

OBJECTS = vgrid_descriptors.o vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o utils.o msg.o

all: Makefile.dep.mk $(OBJECTS)

include Makefile.dep.mk

lib: all
	ar cru libdescrip.a $(OBJECTS)

clean:
	rm -f *.f90 *.o *.mod lib*.a

distclean: clean
