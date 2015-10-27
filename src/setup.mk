DEBUG=0

MY_FFLAGS = -openmp
MY_FFLAGS_DEBUG = -openmp -C -g -O0
MY_CFLAGS = -openmp
MY_CFLAGS_DEBUG = -openmp -g -O0

HPCS_BASE = hpcs/201402/02/base/

ifeq ($(ORDENV_PLAT),ubuntu-12.04-amd64-64)
   HPCS_COMP = hpcs/201402/02/intel13sp1u2
   FFLAGS = '$(MY_FFLAGS) -fp-model source  -warn all'
   CFLAGS = '$(MY_CFLAGS) -fp-model precise -Wall'
   defaut: libdescrip.a
   ifeq ($(DEBUG),1)
      FFLAGS = '$(MY_FFLAGS_DEBUG) -fp-model source -warn all'
      CFLAGS = '$(MY_CFLAGS_DEBUG) -fp-model precise -Wall'
      defaut: libdescrip_debug.a
   endif
endif
