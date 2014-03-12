# User options
#FCFLAGS='=-C -g'
DESCRIP_PATH = ../lib

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -libpath $(LIBRARY_PATH) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -src $<

OBJECTS = $(DESCRIP_PATH)/vgrid_descriptors.o $(DESCRIP_PATH)/vgrid_genab_5002.o $(DESCRIP_PATH)/vgrid_genab_1002_5001.o $(DESCRIP_PATH)/vgrid_genab_1001.o  $(DESCRIP_PATH)/vgrid_genab_2001.o $(DESCRIP_PATH)/vgrid_genab_1004.o $(DESCRIP_PATH)/utils.o  $(DESCRIP_PATH)/msg.o

# Set default target
default: all

all: compute_pressure convert_toctoc_5002 add_toctoc print_toctoc

convert_toctoc_5002: convert_toctoc_5002.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -libpath $(MY_LIBPATH) -includes $(MY_INCLUDE_PATH) $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90  -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

print_toctoc: print_toctoc.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -libpath $(MY_LIBPATH) -includes $(MY_INCLUDE_PATH) $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

add_toctoc: add_toctoc.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -libpath $(MY_LIBPATH) -includes $(MY_INCLUDE_PATH) $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

compute_pressure: compute_pressure.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -libpath $(MY_LIBPATH) -includes $(MY_INCLUDE_PATH) $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

$(DESCRIP_PATH)/vgrid_descriptors.o: $(DESCRIP_PATH)/vgrid_descriptors.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) vgrid_descriptors.o)

$(DESCRIP_PATH)/vgrid_genab_5002.o: $(DESCRIP_PATH)/vgrid_genab_5002.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) vgrid_genab_5002.o)

$(DESCRIP_PATH)/vgrid_genab_1002_5001.o: $(DESCRIP_PATH)/vgrid_genab_1002_5001.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) vgrid_genab_1002_5001.o)

$(DESCRIP_PATH)/vgrid_genab_1001.o: $(DESCRIP_PATH)/vgrid_genab_1001.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) vgrid_genab_1001.o)

$(DESCRIP_PATH)/vgrid_genab_1004.o: $(DESCRIP_PATH)/vgrid_genab_1004.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) vgrid_genab_1004.o)

$(DESCRIP_PATH)/utils.o: $(DESCRIP_PATH)/utils.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) utils.o)

clean:
	rm -f *.f90 *.o *.mod test_output.fst utput_file.txt bin/data_tests

distclean: clean
	rm -f print_toctoc add_toctoc compute_pressure print_toctoc convert_toctoc_5002 

