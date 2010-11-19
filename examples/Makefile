# User options
FCFLAGS='=-C -g'
RCOMPILE = r.compile
DESCRIP_PATH = ../
RMN = rmn_011

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -src $<

OBJECTS = $(DESCRIP_PATH)vgrid_descriptors.o $(DESCRIP_PATH)vgrid_genab_5002.o $(DESCRIP_PATH)vgrid_genab_1002_5001.o $(DESCRIP_PATH)vgrid_genab_1001.o  $(DESCRIP_PATH)vgrid_genab_2001.o $(DESCRIP_PATH)vgrid_genab_1004.o 

all: topLevel_levels lowLevel_levels 

topLevel_levels: topLevel_levels.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)

lowLevel_levels: lowLevel_levels.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)
compute_pressure_form_keys: compute_pressure_form_keys.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)

use_dpidpis:	use_dpidpis.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)

$(DESCRIP_PATH)vgrid_descriptors.o: $(DESCRIP_PATH)vgrid_descriptors.ftn90
	echo "STOP: $(DESCRIP_PATH)vgrid_descriptors has been updated ... recompilation needed in $(DESCRIP_PATH)"; \
	exit

$(DESCRIP_PATH)vgrid_genab.o: $(DESCRIP_PATH)vgrid_genab.ftn90
	echo "STOP: $(DESCRIP_PATH)vgrid_genab.o has been updated ... recompilation needed in $(DESCRIP_PATH)"; \
	exit

clean:
	rm -f *.f90 *.o *.mod test_output.fst

distclean: clean
	rm -f topLevel_levels lowLevel_levels compute_pressure_form_keys

