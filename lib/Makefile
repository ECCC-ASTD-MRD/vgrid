# Set default target
default: lib

# Override incorrect implicits
%.o : %.mod

# Define suffix rules
.SUFFIXES: .ftn90 .o
.ftn90.o:
	s.compile -includes . $(MY_INCLUDE_PATH) $(INCLUDE_PATH) -optf "=$(MY_FFLAGS) $(FFLAGS)" -src $<

OBJECTS = vgrid_descriptors.o vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o utils.o msg.o

all: $(OBJECTS)

vgrid_descriptors.o: vgrid_descriptors.ftn90 vgrid_genab_1001.o vgrid_genab_1002_5001.o vgrid_genab_1004.o vgrid_genab_2001.o vgrid_genab_5002.o utils.o

lib: all
	ar cru libdescrip.a $(OBJECTS)

ssm:
	../bh_vgrid.py -p $(ORDENV_PLAT) -m $(BH_MODE) -v BH_MAKE=$(BH_MAKE) -v BH_PULL_SOURCE=$(PWD)/.. -v BH_PULL_SOURCE_GIT_BRANCH=$(BH_PULL_SOURCE_GIT_BRANCH) -v BH_PACKAGE_VERSION=$(BH_PULL_SOURCE_GIT_BRANCH)-$(COMP_ARCH) --local;\

clean:
	rm -f *.f90 *.o *.mod lib*.a

distclean: clean
