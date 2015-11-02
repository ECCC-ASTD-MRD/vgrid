MY_FFLAGS = -O2
#MY_FFLAGS = -C -g -O0
MY_CFLAGS = -O2
#MY_CFLAGS =    -g -O0

HPCS_BASE = hpcs/201402/02/base/
RPN_LIB=rpn/libs/15.2

ifeq ($(ORDENV_PLAT),ubuntu-12.04-amd64-64)
   HPCS_COMP = hpcs/201402/02/intel13sp1u2
   FFLAGS = '$(MY_FFLAGS) -openmp -fp-model source  -warn all'
   CFLAGS = '$(MY_CFLAGS) -openmp -fp-model precise -Wall'
endif

ifeq ($(ORDENV_PLAT),aix-7.1-ppc7-64)
   HPCS_COMP = hpcs/ext/xlf_13.1.0.10
   FFLAGS = '$(MY_FFLAGS)'
   CFLAGS = '$(MY_FFLAGS)'
endif

