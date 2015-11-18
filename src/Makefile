include setup.mk

default: libdescrip.a

dependencies.mk: *.c *.ftn90 *.h *.hc
	. ssmuse-sh -d $(HPCS_BASE) ;\
	ls *.c *.ftn90 | ./s.dependencies.pl > dependencies.mk ;\

include dependencies.mk

%.o: %.ftn90
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB);\
	s.compile -src $< -optf =$(FFLAGS)

%.o: %.c
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB);\
	s.compile -src $< -optc =$(CFLAGS)

libdescrip.a: $(OBJECTS) dependencies.mk
	rm -f libdescrip.a && ar scru libdescrip.a *.o ;\
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB);\

vgrid_version:
	echo "character(len=128) :: vgrid_descriptors_version = \"VGRID $(BH_PULL_SOURCE_GIT_BRANCH) $${COMP_ARCH} $${ORDENV_PLAT} $(shell date)\"" > vgrid_version.hf ;\
	echo                "char vgrid_descriptors_version[] = \"VGRID $(BH_PULL_SOURCE_GIT_BRANCH) $${COMP_ARCH} $${ORDENV_PLAT} $(shell date)\";" > vgrid_version.hc ;\

ssm:
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB) ;\
	../bh_vgrid.py --host ${BH_HOST} -p $(ORDENV_PLAT) -m $(BH_MODE) -v BH_PULL_SOURCE=$(PWD)/.. -v BH_PULL_SOURCE_GIT_BRANCH=$(BH_PULL_SOURCE_GIT_BRANCH) -v BH_PACKAGE_VERSION=$(BH_PULL_SOURCE_GIT_BRANCH)-$${COMP_ARCH} ;\

clean:
	rm -f *.f90 *.o *.mod dependencies.mk ;\
	rm -f ../include/*.mod ../include/*.h

distclean: cleanxlf13
	rm -f libdescrip.a ../lib/libdescrip.a ;\

