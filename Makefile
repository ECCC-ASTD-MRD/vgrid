# User options
FCFLAGS='=-C -g'
RCOMPILE = r.compile

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -src $<

OBJECTS = vgrid_descriptors.o vgrid_genab_5002.o vgrid_genab_1002_5001.o

all:	vgrid_descriptors.o vgrid_genab_5002.o vgrid_genab_1002_5001.o

vgrid_descriptors.o: vgrid_descriptors.ftn90 vgrid_genab_5002.o vgrid_genab_1002_5001.o
vgrid_genab_5002.o: vgrid_genab_5002.ftn90
vgrid_genab_1002_5001.o: vgrid_genab_1002_5001.ftn90

clean:
	rm -f *.f90 *.o *.mod
