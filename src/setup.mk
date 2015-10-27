DEBUG=0

MY_FFLAGS = -openmp
MY_FFLAGS_DEBUG = -openmp -C -g -O0
MY_CFLAGS = -openmp
MY_CFLAGS_DEBUG = -openmp -g -O0

HPCS_BASE = hpcs/201402/02/base/

ifeq ($(ORDENV_PLAT),ubuntu-12.04-amd64-64)
   HPCS_COMP = hpcs/201402/02/intel13sp1u2
   FFLAGS = "-fp-model source $(MY_FFLAGS)"
   CFLAGS = "-fp-model source $(MY_CFLAGS)"
   defaut: libdescrip.a
   ifeq ($(DEBUG),1)
      FFLAGS = "-fp-model source $(MY_FFLAGS_DEBUG)"
      CFLAGS = "-fp-model source $(MY_CFLAGS_DEBUG)"
      defaut: libdescrip_debug.a
   endif
endif
