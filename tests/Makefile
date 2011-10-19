# User options
FCFLAGS='=-C -g'
RCOMPILE = r.compile
DESCRIP_PATH = ../lib
DATA_ARCH=`uname -s`

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -src $<

OBJECTS = $(DESCRIP_PATH)/vgrid_descriptors.o unit_testing.o

unit_testing.o: unit_testing.ftn90 $(DESCRIP_PATH)/vgrid_descriptors.o

alltests:
	make tests DATA_ARCH=Linux; \
        make tests DATA_ARCH=AIX


$(DESCRIP_PATH)/vgrid_descriptors.o: $(DESCRIP_PATH)/vgrid_descriptors.ftn90
	(cd $(DESCRIP_PATH) && $(MAKE) $(MAKEFLAGS) vgrid_descriptors.o)

tests:
	rm -f data/* 2>/dev/null; \
	cd data ; \
        ln -s ../data_$(DATA_ARCH)/* . ; \
	cd .. ; \
	run_tests.ksh $(ONLY)

clean:
	rm -f *.f90 *.o *.mod

distclean: clean
	rm -f data/*
