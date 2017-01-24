ifeq ($(BASE_ARCH),$(EC_ARCH))
   $(info ERROR: please setup the compilation environment by typing the folowing:)
   $(info . setup.dot or . setup_gpsc.dot)
   $(error )
endif                                                                                                                                                           

# Set default target
default: lib

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .F90 .o

%.o: %.F90
	s.f90 -c $(MY_FFLAGS) -lrmn $<

OBJECTS = vgrid_descriptors.o vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o utils.o

all: Makefile.dep.mk $(OBJECTS) vgrid_version

include Makefile.dep.mk

lib: all
	ar cru libdescrip.a $(OBJECTS)

vgrid_version:
	echo "write(6,'(\"   *          VGRID $(BH_PULL_SOURCE_GIT_BRANCH) $(COMP_ARCH) $(ORDENV_PLAT) `date`\")')" > vgrid_version.cdk;
	echo "write(6,'(\"   ********************************************************************************************\")')" >> vgrid_version.cdk;

ssm:
	../bh_vgrid.py --host ${BH_HOST} -p $(ORDENV_PLAT) -m $(BH_MODE) -v BH_MAKE=$(BH_MAKE) -v BH_PULL_SOURCE=$(PWD)/.. -v BH_PULL_SOURCE_GIT_BRANCH=$(BH_PULL_SOURCE_GIT_BRANCH) -v BH_PACKAGE_VERSION=$(BH_PULL_SOURCE_GIT_BRANCH)-$(COMP_ARCH) --local;\

clean:
	rm -f *.o *.mod lib*.a

distclean: clean

