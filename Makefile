# User options
FCFLAGS='=-C -g'
RCOMPILE = r.compile

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -src $<

OBJECTS = grid_descriptors.o

grid_descriptors.o: grid_descriptors.ftn90 

clean:
	rm -f *.f90 *.o *.mod
