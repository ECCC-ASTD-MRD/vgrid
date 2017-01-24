# User options
#FCFLAGS='=-C -g'
RCOMPILE = s.compile
DESCRIP_PATH = ../lib
#RMN=rmn_014_rc2
DIST=use_new_read use_new_gen topLevel_levels lowLevel_levels use_dpidpis gen_5999

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .F90 .o
.F90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -src $<

OBJECTS = $(DESCRIP_PATH)/vgrid_descriptors.o $(DESCRIP_PATH)/vgrid_genab_5002.o $(DESCRIP_PATH)/vgrid_genab_1002_5001.o $(DESCRIP_PATH)/vgrid_genab_1001.o  $(DESCRIP_PATH)/vgrid_genab_2001.o $(DESCRIP_PATH)/vgrid_genab_1004.o $(DESCRIP_PATH)/utils.o $(DESCRIP_PATH)/vgrid_genab_5100.o

all: $(DIST)

use_new_read: use_new_read.F90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.F90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

use_new_gen: use_new_gen.F90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.F90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

topLevel_levels: topLevel_levels.F90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.F90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

lowLevel_levels: lowLevel_levels.F90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.F90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

use_dpidpis:	use_dpidpis.F90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.F90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

gen_5999:	gen_5999.F90 $(OBJECTS)
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -obj $(OBJECTS) -src $@.F90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

$(DESCRIP_PATH)/vgrid_descriptors.o: $(DESCRIP_PATH)/vgrid_descriptors.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_descriptors.o)

$(DESCRIP_PATH)/vgrid_genab_1001.o: $(DESCRIP_PATH)/vgrid_genab_1001.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_1001.o)

$(DESCRIP_PATH)/vgrid_genab_1004.o: $(DESCRIP_PATH)/vgrid_genab_1004.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_1004.o)

$(DESCRIP_PATH)/vgrid_genab_2001.o: $(DESCRIP_PATH)/vgrid_genab_2001.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_2001.o)

$(DESCRIP_PATH)/vgrid_genab_5002.o: $(DESCRIP_PATH)/vgrid_genab_5002.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_5002.o)

$(DESCRIP_PATH)/vgrid_genab_1002_5001.o: $(DESCRIP_PATH)/vgrid_genab_1002_5001.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_1002_5001.o)

$(DESCRIP_PATH)/vgrid_genab_5100.o: $(DESCRIP_PATH)/vgrid_genab_5100.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_genab_5100.o)

$(DESCRIP_PATH)/utils.o: $(DESCRIP_PATH)/utils.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) utils.o)

clean:
	rm -f *.o *.mod test_output.fst

distclean: clean
	rm -f $(DIST)

tild:
	rm -f *~

