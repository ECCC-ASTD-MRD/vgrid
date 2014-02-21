# User options
#FCFLAGS='=-C -g'
#RCOMPILE = r.compile
DESCRIP_PATH = ../lib
#RMN=rmn_014_rc2
DIST=use_new_read use_new_gen topLevel_levels lowLevel_levels use_dpidpis

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -src $<

OBJECTS = $(DESCRIP_PATH)/vgrid_descriptors.o $(DESCRIP_PATH)/vgrid_genab_5002.o $(DESCRIP_PATH)/vgrid_genab_1002_5001.o $(DESCRIP_PATH)/vgrid_genab_1001.o  $(DESCRIP_PATH)/vgrid_genab_2001.o $(DESCRIP_PATH)/vgrid_genab_1004.o $(DESCRIP_PATH)/utils.o $(DESCRIP_PATH)/msg.o

all: $(DIST)

use_new_read: use_new_read.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

use_new_gen: use_new_gen.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

topLevel_levels: topLevel_levels.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

lowLevel_levels: lowLevel_levels.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

use_dpidpis:	use_dpidpis.ftn90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

$(DESCRIP_PATH)/vgrid_descriptors.o: $(DESCRIP_PATH)/vgrid_descriptors.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_descriptors.o)

$(DESCRIP_PATH)/vgrid_genab_1001.o: $(DESCRIP_PATH)/vgrid_genab_1001.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_1001.o)

$(DESCRIP_PATH)/vgrid_genab_1004.o: $(DESCRIP_PATH)/vgrid_genab_1004.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_1004.o)

$(DESCRIP_PATH)/vgrid_genab_2001.o: $(DESCRIP_PATH)/vgrid_genab_2001.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_2001.o)

$(DESCRIP_PATH)/vgrid_genab_5002.o: $(DESCRIP_PATH)/vgrid_genab_5002.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_5002.o)

$(DESCRIP_PATH)/vgrid_genab_1002_5001.o: $(DESCRIP_PATH)/vgrid_genab_1002_5001.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_1002_5001.o)

$(DESCRIP_PATH)/utils.o: $(DESCRIP_PATH)/utils.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) utils.o)

clean:
	rm -f *.f90 *.o *.mod test_output.fst

distclean: clean
	rm -f $(DIST)

tild:
	rm -f *~

