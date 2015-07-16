# User options
#FFLAGS=$(FFLAGS_INTEL)
FFLAGS="$(FFLAGS_INTEL) -C -g -O0"
CFLAGS="$(FFLAGS_INTEL) -g -O0"
DESCRIP_PATH_F = ../libf
DESCRIP_PATH_C = ../libc

DIST=use_new_read use_new_gen topLevel_levels lowLevel_levels use_dpidpis c_use_new_read
# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_PATH_F) -src $<

.c.o:
	$(RCOMPILE) -optf =$(CFLAGS) -src $<

OBJECTS_F = $(DESCRIP_PATH_F)/vgrid_descriptors.o $(DESCRIP_PATH_F)/utils.o $(DESCRIP_PATH_F)/msg.o

OBJECTS_C = $(DESCRIP_PATH_C)/vgrid.o

all: $(DIST)

use_new_read: use_new_read.ftn90 $(OBJECTS_F)
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_PATH_F) -obj $(OBJECTS_F) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

c_use_new_read: c_use_new_read.c c_use_new_read.o c_use_new_read.o $(OBJECTS_C)
	$(RCOMPILE) -bidon c -main $@ -optf =$(CFLAGS) -includes ../libc -obj ../libc/vgrid.o -src $@.c -o $@ -libappl rmn

use_new_gen: use_new_gen.ftn90 $(OBJECTS_F) $(DESCRIP_PATH_C)/vgrid.h
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_PATH_F) -obj $(OBJECTS_F) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

topLevel_levels: topLevel_levels.ftn90 $(OBJECTS_F)
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_PATH_F) -obj $(OBJECTS_F) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

lowLevel_levels: lowLevel_levels.ftn90 $(OBJECTS_F)
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_PATH_F) -obj $(OBJECTS_F) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

use_dpidpis:	use_dpidpis.ftn90 $(OBJECTS_F)
	$(RCOMPILE) -optf =$(FFLAGS) -includes $(DESCRIP_PATH_F) -obj $(OBJECTS_F) -src $@.ftn90 -o $@ -librmn $(RMN) -libappl ${MODELUTILS}

$(DESCRIP_PATH_F)/vgrid_descriptors.o: $(DESCRIP_PATH_F)/vgrid_descriptors.ftn90
	(cd $(DESCRIP_PATH_F) && $(MAKE) $(MAKEFLAGS) vgrid_descriptors.o)

$(DESCRIP_PATH_F)/vgrid_genab_1001.o: $(DESCRIP_PATH_F)/vgrid_genab_1001.ftn90
	(cd $(DESCRIP_PATH_F) && $(MAKE) $(MAKEFLAGS) vgrid_genab_1001.o)

$(DESCRIP_PATH_F)/vgrid_genab_1004.o: $(DESCRIP_PATH_F)/vgrid_genab_1004.ftn90
	(cd $(DESCRIP_PATH_F) && $(MAKE) $(MAKEFLAGS) vgrid_genab_1004.o)

$(DESCRIP_PATH_F)/vgrid_genab_2001.o: $(DESCRIP_PATH_F)/vgrid_genab_2001.ftn90
	(cd $(DESCRIP_PATH_F) && $(MAKE) $(MAKEFLAGS) vgrid_genab_2001.o)

$(DESCRIP_PATH_F)/vgrid_genab_5002.o: $(DESCRIP_PATH_F)/vgrid_genab_5002.ftn90
	(cd $(DESCRIP_PATH_F) && $(MAKE) $(MAKEFLAGS) vgrid_genab_5002.o)

$(DESCRIP_PATH_F)/vgrid_genab_1002_5001.o: $(DESCRIP_PATH_F)/vgrid_genab_1002_5001.ftn90
	(cd $(DESCRIP_PATH_F) && $(MAKE) $(MAKEFLAGS) vgrid_genab_1002_5001.o)

$(DESCRIP_PATH_F)/utils.o: $(DESCRIP_PATH_F)/utils.ftn90
	(cd $(DESCRIP_PATH_F) && $(MAKE) $(MAKEFLAGS) utils.o)

$(DESCRIP_PATH_C)/vgrid.c: $(DESCRIP_PATH_C)/vgrid.c
	(cd $(DESCRIP_PATH_C) && $(MAKE) $(MAKEFLAGS))

$(DESCRIP_PATH_C)/vgrid.h: $(DESCRIP_PATH_C)/vgrid.h
	(cd $(DESCRIP_PATH_C) && $(MAKE) $(MAKEFLAGS))

clean:
	rm -f *.f90 *.o *.mod test_output.fst

distclean: clean
	rm -f $(DIST)

tild:
	rm -f *~

