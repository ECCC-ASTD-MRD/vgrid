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

include Makefile.dep.mk

all: $(OBJECTS) vgrid_version

lib: all
	ar cru libdescrip.a $(OBJECTS) ;\

vgrid_version:
	echo "write(6,'(\"   *          VGRID $(BH_PULL_SOURCE_GIT_BRANCH) $(COMP_ARCH) $(ORDENV_PLAT) `date`\")')" > vgrid_version.cdk;
	echo "write(6,'(\"   ********************************************************************************************\")')" >> vgrid_version.cdk;

ssm:
	../bh_vgrid.py -p $(ORDENV_PLAT) -m $(BH_MODE) -v BH_MAKE=$(BH_MAKE) -v BH_PULL_SOURCE=$(PWD)/.. -v BH_PULL_SOURCE_GIT_BRANCH=$(BH_PULL_SOURCE_GIT_BRANCH) -v BH_PACKAGE_VERSION=$(BH_PULL_SOURCE_GIT_BRANCH)-$(COMP_ARCH) --local;\

clean:
	rm -f *.o *.mod ;\

clean_ssm: clean_bh
	rm -f *.ssm ;\

clean_bh:
	rm -rf vgriddescriptors vgriddescriptors_*  log-*-*-*-*-* report-*-*-*-*-* work-*-*-*-*-* ;\

distclean: clean
	rm -f libdescrip.a libdescripshared.so ;\

clean_tilde:
	rm -f *~ ;\

clean_all: distclean clean_ssm clean_bh clean_tilde
