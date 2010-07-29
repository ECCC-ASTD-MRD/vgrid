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

OBJECTS = $(DESCRIP_PATH)grid_descriptors.o

all: topLevel_levels lowLevel_levels

topLevel_levels: topLevel_levels.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)

lowLevel_levels: lowLevel_levels.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)
compute_pressure_form_keys: compute_pressure_form_keys.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)

$(DESCRIP_PATH)grid_descriptors.o: $(DESCRIP_PATH)grid_descriptors.ftn90
	echo "STOP: $(DESCRIP_PATH)grid_descriptors has been updated ... recompilation needed in $(DESCRIP_PATH)"; \
	exit

clean:
	rm -f *.f90 *.o *.mod test_output.fst

distclean: clean
	rm -f topLevel_levels lowLevel_levels compute_pressure_form_keys

