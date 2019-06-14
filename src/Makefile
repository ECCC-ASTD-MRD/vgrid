.SUFFIXES:

.SUFFIXES : .o .F90 .c .a .h .hc .hf

SHELL = /bin/sh

!!!FFLAGS =
FFLAGS="'-C -qopenmp -fp-model source -warn all'"

CFLAGS="'-qopenmp -fp-model precise -w3'"

SUPP_OPT = -openmp

OPTIMIZ = 2
#OPTIMIZ =  1
#OPTIMIZ =  0
#OPTIMIZ =  0 -debug

MYLIB = libvgrid.a

UPDATEX =

TARRLS = beta

.PRECIOUS:

# It is assumed that the environment makes available makefile_suffix_rules.inc; e.g.
#     . ssmuse-sh -p /ssm/net/rpn/libs/4.1b/environment-includes_4.3_all


OBJECTS= \
	VertInterpConstants_90.o VerticalGrid_90.o VerticalInterpolation_90.o 
OBJECTS= \
	vgrid.o  vgrid_descriptors.o  vgrid_utils.o  

obj:	$(OBJECTS)

vgrid.o:	vgrid.c \
	BODY_C_compute_heights_4001.hc  BODY_C_compute_pressure_2001.hc  armnlib.h  BODY_C_compute_pressure_5100.hc  \
	BODY_Cvgd_diag_withref_2ref.hc BODY_C_compute_pressure_5002_5003_5004_5005.hc  vgrid.h  \
	BODY_C_compute_heights_0001.hc  BODY_C_compute_pressure_1003_5001.hc  BODY_C_compute_pressure_1001_1002.hc   \
	BODY_C_compute_heights_21001.hc  

vgrid_descriptors.o:	vgrid_descriptors.F90 \
	vgrid_utils.o  BODY_F_dpidpis_withref.hf  BODY_F_dpidpis_withref_prof.hf  BODY_F_diag_withref.hf  \
	BODY_F_levels_withref_prof.hf   BODY_F_levels_withref.hf  BODY_F_diag_withref_prof.hf  vgrid_descriptors.hf  

vgrid_utils.o:	vgrid_utils.F90 vgrid_descriptors.hf  


genlib: $(OBJECTS)
#Creer ou mettre a jour la programmatheque 
	ar rcv $(MYLIB) $(OBJECTS)

clean:
#Faire le grand menage. On enleve tous les fichiers sources\ninutiles et les .o 
	-if [ "*.ftn" != "`echo *.ftn`" ] ; \
	then \
	for i in *.ftn ; \
	do \
	fn=`r.basename $$i '.ftn'`; \
	rm -f $$fn.f; \
	done \
	fi
	rm -f *.o *.f90 *.mod *.stb  VInterp_*.Abs
