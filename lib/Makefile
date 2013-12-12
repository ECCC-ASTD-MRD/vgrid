# User options
FCFLAGS=
DEBUG_FLAGS='=-C -g'
RCOMPILE = s.compile

# Set default target
default: lib

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -includes $(INCLUDE_PATH) -optf $(FCFLAGS) $(DEBUG_FLAGS) -src $<

OBJECTS = vgrid_descriptors.o vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o utils.o

all: $(OBJECTS)

vgrid_descriptors.o: vgrid_descriptors.ftn90 vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o utils.o

lib: all
	ar cru libdescrip.a $(OBJECTS)

clean:
	rm -f *.f90 *.o *.mod lib*.a

distclean: clean
