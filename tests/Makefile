# User options
FFLAGS="$(FFLAGS_INTEL) -C -g"
DESCRIP_F_PATH = ../libf
DATA_ARCH=`uname -s`

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	s.compile -libpath $(MY_LIBPATH) $(LIBRARY_PATH) -optf =$(FFLAGS) -includes $(MY_INCLUDE_PATH) $(DESCRIP_F_PATH) -src $<

OBJECTS = $(DESCRIP_F_PATH)/vgrid_descriptors.o $(DESCRIP_F_PATH)/utils.o unit_testing.o

unit_testing.o: unit_testing.ftn90 $(DESCRIP_F_PATH)/vgrid_descriptors.o $(DESCRIP_F_PATH)/utils.o

alltests:
	make tests DATA_ARCH=Linux; \
        make tests DATA_ARCH=AIX


$(DESCRIP_F_PATH)/vgrid_descriptors.o: $(DESCRIP_F_PATH)/vgrid_descriptors.ftn90
	(cd $(DESCRIP_F_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_descriptors.o)

$(DESCRIP_F_PATH)/utils.o: $(DESCRIP_F_PATH)/utils.ftn90
	(cd $(DESCRIP_F_PATH) && $(MAKE) $(MAKEFLAGS) utils.o)

tests:
	rm -rf data/* 2>/dev/null; \
	cd data ; \
	rm -rf WORK ; \
	mkdir -p WORK ; \
        ln -s ../data_$(DATA_ARCH)/* . ; \
	cd .. ; \
	./run_tests.ksh $(ONLY)

clean:
	rm -f *.f90 *.o *.mod *.out

distclean: clean
	rm -f data/*
