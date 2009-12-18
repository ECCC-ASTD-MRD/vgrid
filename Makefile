# User options
FCFLAGS='=-C -g'
RCOMPILE = r.compile

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -src $<

OBJECTS = vgrid_descriptors.o vgrid_genab2.o

vgrid_descriptors.o: vgrid_descriptors.ftn90 vgrid_genab2.o
vgrid_genab2.o: vgrid_genab2.ftn90

clean:
	rm -f *.f90 *.o *.mod
