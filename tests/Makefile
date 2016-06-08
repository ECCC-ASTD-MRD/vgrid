# User options
FFLAGS="$(FFLAGS_INTEL) -C -g"
DESCRIP_PATH = ../lib
DATA_ARCH=`uname -s`
MAX_CPUS=12

include ../lib/setup.mk

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .F90 .o
.F90.o:
	s.f90 -c $(MY_FFLAGS) -libpath $(DESCRIP_PATH) -I$(DESCRIP_PATH) $<


OBJECTS = $(DESCRIP_PATH)/vgrid_descriptors.o $(DESCRIP_PATH)/utils.o unit_testing.o

unit_testing.o: unit_testing.F90 $(DESCRIP_PATH)/vgrid_descriptors.o $(DESCRIP_PATH)/utils.o

alltests:
	make tests DATA_ARCH=Linux; \
        make tests DATA_ARCH=AIX


$(DESCRIP_PATH)/vgrid_descriptors.o: $(DESCRIP_PATH)/vgrid_descriptors.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_descriptors.o)

$(DESCRIP_PATH)/utils.o: $(DESCRIP_PATH)/utils.F90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) utils.o)

tests:
	export BH_MAKE=${BH_MAKE} ;\
	rm -rf data/* 2>/dev/null ;\
	cd data ;\
	rm -rf WORK ;\
	mkdir -p WORK ;\
        ln -s ../data_$(DATA_ARCH)/* . ;\
	cd .. ;\
	./run_tests.ksh $(ONLY)

tests_par:
	export BH_MAKE=${BH_MAKE} ;\
	rm -rf data/* 2>/dev/null ;\
	cd data ;\
	rm -rf WORK ;\
	mkdir -p WORK ;\
        ln -s ../data_$(DATA_ARCH)/* . ;\
	cd .. ;\
	./run_tests_in_parallel.ksh -MAX_CPUS $(MAX_CPUS)

clean:
	rm -f *.f90 *.o *.mod *.out

distclean: clean
	rm -f data/*
