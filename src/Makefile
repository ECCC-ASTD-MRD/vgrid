
ifeq ($(BASE_ARCH),$(EC_ARCH))
   $(info ERROR: please setup the compilation environment by typing the folowing:)
   $(info . setup.dot or . setup_gpsc.dot)
   $(error )
endif

default: libdescrip.a

dependencies.mk: *.c *.F90 *.h *.hc
	ls *.hc *.c *.F90 | ./s.dependencies.pl > dependencies.mk ;\

include dependencies.mk

%.o: %.F90
	s.f90 -c $(MY_FFLAGS) -lrmn $<

%.o: %.c
	s.cc -c $(MY_CFLAGS) $<

libdescrip.a: $(OBJECTS) dependencies.mk
	rm -f libdescrip.a && ar scru libdescrip.a *.o ;\

shared: libdescrip.a
	if [ $$ORDENV_PLAT = ubuntu-12.04-amd64-64 ]; then \
	   s.f90 -shared -o libdescripshared.so *.o -lrmnshared_015.2 -Wl,-rpath,/ssm/net/hpcs/201311/master/intelcomp_2013sp1_multi/composer_xe_2013_sp1.0.080/mkl/lib/intel64 -Wl,-rpath,/ssm/net/hpcs/201311/master/intelcomp_2013sp1_multi/composer_xe_2013_sp1.0.080/compiler/lib/intel64 -Wl,-rpath,/ssm/net/$(RPN_LIB)/$(ORDENV_PLAT)/lib/$(BASE_ARCH)/$(COMP_ARCH);\
	fi

vgrid_version:
	echo "character(len=128) :: vgrid_descriptors_version = \"VGRID $(BH_PULL_SOURCE_GIT_BRANCH) $(COMP_ARCH) $(ORDENV_PLAT) $(shell date)\"" > vgrid_version.hf ;\
	echo                "char vgrid_descriptors_version[] = \"VGRID $(BH_PULL_SOURCE_GIT_BRANCH) $(COMP_ARCH) $(ORDENV_PLAT) $(shell date)\";" > vgrid_version.hc ;\

ssm:
	../bh_vgrid.py -p $(ORDENV_PLAT) -m $(BH_MODE) -v BH_PULL_SOURCE=$(PWD)/.. -v BH_PULL_SOURCE_GIT_BRANCH=$(BH_PULL_SOURCE_GIT_BRANCH) -v BH_PACKAGE_VERSION=$(BH_PULL_SOURCE_GIT_BRANCH)-$(COMP_ARCH) --local ;\

clean:
	rm -f *.o *.mod ;\

distclean: clean
	rm -f libdescrip.a libdescripshared.so ;\
