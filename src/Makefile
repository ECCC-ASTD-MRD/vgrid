.SUFFIXES:

.SUFFIXES : .o .c .F90 .cpp .a .h

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


OBJECTS = vgrid_utils.o vgrid.o vgrid_descriptors.o

obj:	$(OBJECTS)

vgrid.o: vgrid.cpp armnlib.h vgrid.h

vgrid_descriptors.o: vgrid_descriptors.F90 vgrid_utils.o vgrid_descriptors.hf

vgrid_utils.o: vgrid_utils.F90 vgrid_descriptors.hf


genlib: $(OBJECTS)
	rm -f $(MYLIB)
	ar rcv $(MYLIB) $(OBJECTS)

clean:
	rm -f *.o *.mod libvgrid.a
