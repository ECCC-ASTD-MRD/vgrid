.SUFFIXES:

.SUFFIXES : .o .c .F90 .cpp .a .h .hpp

SHELL = /bin/bash

CFLAGS =
FFLAGS =
!!!FFLAGS="'-C -qopenmp -warn all'"

!!!MY_CFLAGS='-C'

SUPP_OPT = -openmp

OPTIMIZ = 2

MYLIB = libvgrid.a

include $(RPN_TEMPLATE_LIBS)/include/makefile_suffix_rules.inc

DEFINE = -DFICHSTD98

.PRECIOUS:

# It is assumed that the environment makes available makefile_suffix_rules.inc; e.g.
#     . ssmuse-sh -p /ssm/net/rpn/libs/4.1b/environment-includes_4.3_all


.cpp.o:
	s.cc -c -O $(OPTIMIZ) $(DEFINE) $(CFLAGS) $(SUPP_OPT) $(C_SUPP_OPT) $<


OBJECTS = vgrid_utils.o vgrid.o vgrid_subclasses.o vgrid_creators.o vgrid_descriptors.o C_interface.o coat_check.o vgrid_interfaces.o

# default:
libvgrid.a: $(OBJECTS)
	ar rcv libvgrid.a $(OBJECTS)

vgrid.h:  coat_check.hpp vgrid_descriptor.h

vgrid.hpp:  vgrid.h

vgrid_subclasses.hpp:  vgrid.hpp

coat_check.hpp:  vgrid_descriptor.h

C_interface.hpp: C_interface.h

vgrid.o: vgrid.cpp armnlib.h vgrid.hpp vgrid_descriptor.h armnlib.hpp

vgrid_subclasses.o: vgrid_subclasses.cpp vgrid_subclasses.hpp

vgrid_creators.o: vgrid_creators.cpp vgrid_creators.hpp

vgrid_descriptors.o: vgrid_descriptors.F90 vgrid_utils.o vgrid_descriptors.hf

vgrid_interfaces.o:  vgrid_interfaces.F90

vgrid_utils.o: vgrid_utils.F90 vgrid_descriptors.hf

C_interface.o:  C_interface.cpp vgrid_creators.hpp C_interface.hpp C_interface.h vgrid.hpp vgrid_descriptor.h

coat_check.o:  coat_check.cpp coat_check.hpp vgrid.h


genlib: $(OBJECTS)
	rm -f $(MYLIB)
	ar rcv $(MYLIB) $(OBJECTS)

clean:
	rm -f *.o *.mod libvgrid.a
