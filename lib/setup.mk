unset MY_FFLAGS

MY_FFLAGS="-O2"
#MY_FFLAGS="-C -g -O0"

. version.dot

HPCS_BASE=hpcs/201402/02/base/
RPN_LIB=rpn/libs/15.2

ifeq ($(ORDENV_PLAT),ubuntu-12.04-amd64-64)
   BH_MAKE=make
   BH_HOST=pollux
   BH_MODE=intel
   COMPILER=intel13sp1u2
   HPCS_COMP = hpcs/201402/02/$(COMPILER)
   MY_FFLAGS = '$(MY_FFLAGS) -openmp -fp-model source  -warn all'
endif

ifeq ($(ORDENV_PLAT),ubuntu-14.04-amd64-64)
   BH_MAKE=make
   BH_HOST=einstein
   BH_MODE=intel
   COMPILER=intel13sp1u2
   HPCS_COMP = hpcs/201402/02/$(COMPILER)
   MY_FFLAGS = '$(MY_FFLAGS) -openmp -fp-model source  -warn all'
endif

ifeq ($(ORDENV_PLAT),aix-7.1-ppc7-64)
   BH_MAKE=gmake
   BH_HOST=hadar
   BH_MODE=xlf13
   COMPILER=xlf_13.1.0.10
   HPCS_COMP = hpcs/ext/$(COMPILER)
   MY_FFLAGS = '$(MY_FFLAGS)'
endif

export MY_FFLAGS
