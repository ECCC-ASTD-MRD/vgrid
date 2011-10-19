# User options
#FCFLAGS='=-C -g'
RCOMPILE = r.compile
DESCRIP_PATH = ../lib

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -src $<

OBJECTS = $(DESCRIP_PATH)/vgrid_descriptors.o $(DESCRIP_PATH)/vgrid_genab_5002.o $(DESCRIP_PATH)/vgrid_genab_1002_5001.o $(DESCRIP_PATH)/vgrid_genab_1001.o  $(DESCRIP_PATH)/vgrid_genab_2001.o $(DESCRIP_PATH)/vgrid_genab_1004.o 

# Set default target
default: all

all: compute_pressure convert_toctoc_5002 add_toctoc print_toctoc

convert_toctoc_5002: convert_toctoc_5002.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90  -o $@ -librmn $(RMN)

print_toctoc: print_toctoc.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)

add_toctoc: add_toctoc.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)

compute_pressure: compute_pressure.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN)

$(DESCRIP_PATH)/vgrid_descriptors.o: $(DESCRIP_PATH)/vgrid_descriptors.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_descriptors.o)

$(DESCRIP_PATH)/vgrid_genab.o: $(DESCRIP_PATH)/vgrid_genab.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab.o)

clean:
	rm -f *.f90 *.o *.mod test_output.fst

distclean: clean
	rm -f print_toctoc add_toctoc compute_pressure print_toctoc convert_toctoc_5002 

