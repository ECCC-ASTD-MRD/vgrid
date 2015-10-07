# User options
FFLAGS="$(FFLAGS_INTEL)"
#FFLAGS="$(FFLAGS_INTEL) -C -g -O0"
CFLAGS="$(FFLAGS_INTEL)"
#CFLAGS="$(FFLAGS_INTEL) -g -O0"
DESCRIP_F_PATH = ../libf
DESCRIP_C_PATH = ../libc

DIST=use_new_read use_new_gen topLevel_levels lowLevel_levels use_dpidpis c_use_new_read
# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_F_PATH) -src $<

.c.o:
	$(RCOMPILE) -optc =$(CFLAGS) -src $<

OBJECTS_F = $(DESCRIP_F_PATH)/vgrid_descriptors.o $(DESCRIP_F_PATH)/utils.o $(DESCRIP_F_PATH)/msg.o

OBJECTS_C = $(DESCRIP_C_PATH)/vgrid.o

all: $(DIST)

use_new_read: use_new_read.ftn90 $(OBJECTS_F) $(OBJECTS_C)
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_F_PATH) -obj $(OBJECTS_F)  $(OBJECTS_C) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

c_use_new_read: c_use_new_read.c $(OBJECTS_C)
	$(RCOMPILE) -bidon c -main $@ -optf =$(CFLAGS) -includes $(DESCRIP_C_PATH) -obj $(OBJECTS_C) -src $@.c -o $@ -libappl rmn

use_new_gen: use_new_gen.ftn90 $(OBJECTS_F) $(OBJECTS_C)
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_F_PATH) -obj $(OBJECTS_F) $(OBJECTS_C) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

topLevel_levels: topLevel_levels.ftn90 $(OBJECTS_F) $(OBJECTS_C)
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_F_PATH) -obj $(OBJECTS_F)  $(OBJECTS_C) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

lowLevel_levels: lowLevel_levels.ftn90 $(OBJECTS_F) $(OBJECTS_C)
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_F_PATH) -obj $(OBJECTS_F)  $(OBJECTS_C) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

use_dpidpis:	use_dpidpis.ftn90 $(OBJECTS_F) $(OBJECTS_C)
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_F_PATH) -obj $(OBJECTS_F)  $(OBJECTS_C) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

$(DESCRIP_F_PATH)/vgrid_descriptors.o: $(DESCRIP_F_PATH)/vgrid_descriptors.ftn90
	(cd $(DESCRIP_F_PATH) && $(MAKE) vgrid_descriptors.o)

$(DESCRIP_F_PATH)/vgrid_genab_1001.o: $(DESCRIP_F_PATH)/vgrid_genab_1001.ftn90
	(cd $(DESCRIP_F_PATH) && $(MAKE) vgrid_genab_1001.o)

$(DESCRIP_F_PATH)/vgrid_genab_1004.o: $(DESCRIP_F_PATH)/vgrid_genab_1004.ftn90
	(cd $(DESCRIP_F_PATH) && $(MAKE) vgrid_genab_1004.o)

$(DESCRIP_F_PATH)/vgrid_genab_2001.o: $(DESCRIP_F_PATH)/vgrid_genab_2001.ftn90
	(cd $(DESCRIP_F_PATH) && $(MAKE) vgrid_genab_2001.o)

$(DESCRIP_F_PATH)/vgrid_genab_5002.o: $(DESCRIP_F_PATH)/vgrid_genab_5002.ftn90
	(cd $(DESCRIP_F_PATH) && $(MAKE) vgrid_genab_5002.o)

$(DESCRIP_F_PATH)/vgrid_genab_1002_5001.o: $(DESCRIP_F_PATH)/vgrid_genab_1002_5001.ftn90
	(cd $(DESCRIP_F_PATH) && $(MAKE) vgrid_genab_1002_5001.o)

$(DESCRIP_F_PATH)/utils.o: $(DESCRIP_F_PATH)/utils.ftn90
	(cd $(DESCRIP_F_PATH) && $(MAKE) utils.o)

$(DESCRIP_C_PATH)/vgrid.o: $(DESCRIP_C_PATH)/vgrid.c $(DESCRIP_C_PATH)/vgrid.h
	(cd $(DESCRIP_C_PATH) && $(MAKE) vgrid.o)

clean:
	rm -f *.f90 *.o *.mod test_output.fst

distclean: clean
	rm -f $(DIST)

tild:
	rm -f *~

