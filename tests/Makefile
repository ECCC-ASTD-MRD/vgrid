# User options
FCFLAGS='=-C -g'
RCOMPILE = r.compile
DESCRIP_PATH = ../
DATA_ARCH=$(EC_ARCH)

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	$(RCOMPILE) -optf $(FCFLAGS) -includes $(DESCRIP_PATH) -src $<

OBJECTS = $(DESCRIP_PATH)grid_descriptors.o unit_testing.o

unit_testing.o: unit_testing.ftn90

tests:
	make local_tests DATA_ARCH=Linux_pgi611; \
        make local_tests DATA_ARCH=AIX


$(DESCRIP_PATH)grid_descriptors.o: $(DESCRIP_PATH)grid_descriptors.ftn90
	echo "STOP: $(DESCRIP_PATH)grid_descriptors has been updated ... recompilation needed in $(DESCRIP_PATH)"; \
	exit

local_tests:
	rm -f data/* 2>/dev/null; \
	cd data ; \
        ln -s $(DATA_ARCH)/* . ; \
	cd .. ; \
	run_tests.ksh $(ONLY)

clean:
	rm -f *.f90 *.o *.mod

