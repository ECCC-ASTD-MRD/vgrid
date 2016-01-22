include setup.mk

default: libdescrip.a

dependencies.mk: *.c *.ftn90 *.h *.hc
	. ssmuse-sh -d $(HPCS_BASE) ;\
	ls *.c *.ftn90 | ./s.dependencies.pl > dependencies.mk ;\

include dependencies.mk

%.o: %.ftn90
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB);\
	s.compile -src $< -optf =$(FFLAGS) -libappl rmn

%.o: %.c
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB);\
	s.compile -src $< -optc =$(CFLAGS) -libappl rmn

libdescrip.a: $(OBJECTS) dependencies.mk
	rm -f libdescrip.a && ar scru libdescrip.a *.o ;\

shared: libdescrip.a
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB);\
	if [ $$ORDENV_PLAT = ubuntu-12.04-amd64-64 ]; then \
	   s.f90 -shared -o libdescripshared.so *.o -lrmnshared_015.2 -Wl,-rpath,/ssm/net/hpcs/201311/master/intelcomp_2013sp1_multi/composer_xe_2013_sp1.0.080/mkl/lib/intel64 -Wl,-rpath,/ssm/net/hpcs/201311/master/intelcomp_2013sp1_multi/composer_xe_2013_sp1.0.080/compiler/lib/intel64 -Wl,-rpath,/ssm/net/$(RPN_LIB)/$(ORDENV_PLAT)/lib/$(BASE_ARCH)/$(COMPILER);\
	fi

vgrid_version:
	echo "character(len=128) :: vgrid_descriptors_version = \"VGRID $(BH_PULL_SOURCE_GIT_BRANCH) $${COMP_ARCH} $${ORDENV_PLAT} $(shell date)\"" > vgrid_version.hf ;\
	echo                "char vgrid_descriptors_version[] = \"VGRID $(BH_PULL_SOURCE_GIT_BRANCH) $${COMP_ARCH} $${ORDENV_PLAT} $(shell date)\";" > vgrid_version.hc ;\

ssm:
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB) ;\
	../bh_vgrid.py --host ${BH_HOST} -p $(ORDENV_PLAT) -m $(BH_MODE) -v BH_PULL_SOURCE=$(PWD)/.. -v BH_PULL_SOURCE_GIT_BRANCH=$(BH_PULL_SOURCE_GIT_BRANCH) -v BH_PACKAGE_VERSION=$(BH_PULL_SOURCE_GIT_BRANCH)-$${COMP_ARCH} ;\

clean:
	rm -f *.f90 *.o *.mod dependencies.mk ;\

distclean: clean
	rm -f libdescrip.a libdescripshared.so ;\

