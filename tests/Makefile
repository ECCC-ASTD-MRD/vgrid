# User options
FFLAGS="=-C =-g"
DESCRIP_PATH = ../src
DATA_ARCH=`uname -s`

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	d.compile -libpath $(DESCRIP_PATH) -optf $(FFLAGS) -includes $(DESCRIP_PATH) -src $<

unit_testing.o: unit_testing.ftn90 $(DESCRIP_PATH)/libdescrip_debug.a

alltests:
	gmake tests DATA_ARCH=Linux; \
        gmake tests DATA_ARCH=AIX


$(DESCRIP_PATH)/libdescrip_debug.a:
	cd $(DESCRIP_PATH) && gmake libdescrip_debug.a ; \

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
