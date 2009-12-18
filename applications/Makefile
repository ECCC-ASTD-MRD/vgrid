# User options
FCFLAGS='=-C -g'
RCOMPILE = r.compile
DESCRIP_PATH = ../
RMN = rmn_010

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -src $<

OBJECTS = $(DESCRIP_PATH)vgrid_descriptors.o $(DESCRIP_PATH)vgrid_genab2.o

all: compute_pressure convert_toctoc_5002

convert_toctoc_5002: convert_toctoc_5002.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90  -o $@ -librmn $(RMN)

compute_pressure: compute_pressure.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)

$(DESCRIP_PATH)vgrid_descriptors.o: $(DESCRIP_PATH)vgrid_descriptors.ftn90
	echo "STOP: $(DESCRIP_PATH)vgrid_descriptors has been updated ... recompilation needed in $(DESCRIP_PATH)"; \
	exit

$(DESCRIP_PATH)vgrid_genab2.o: $(DESCRIP_PATH)vgrid_genab2.ftn90
	echo "STOP: $(DESCRIP_PATH)vgrid_genab2.o has been updated ... recompilation needed in $(DESCRIP_PATH)"; \
	exit

clean:
	rm -f *.f90 *.o *.mod test_output.fst

distclean: clean
	rm -f compute_pressure convert_toctoc_5002

