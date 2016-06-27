#MY_FFLAGS = -O2
#MY_FFLAGS = -C -g -O0
#MY_CFLAGS = -O2
#MY_CFLAGS =    -g -O0

BH_PULL_SOURCE_GIT_BRANCH=5.4.3
TESTS_MAX_CPUS=12

DOMAIN=$(shell hostname -d)
$(info DOMAIN is $(DOMAIN))

ifeq ($(ORDENV_PLAT),ubuntu-12.04-amd64-64)
   BH_MAKE=make
   BH_HOST=pollux
   BH_MODE=intel
   COMPILER=intel13sp1u2
   HPCS_BASE=hpcs/201402/02/base/
   HPCS_COMP = hpcs/201402/02/$(COMPILER)
   BH_PACKAGE=hpcs/tools/master/bh_0.17_all
   RPN_LIB=rpn/libs/15.2
   FFLAGS = '$(MY_FFLAGS) -openmp -fp-model source  -warn all'
   CFLAGS = '$(MY_CFLAGS) -openmp -fp-model precise -Wall'
endif

ifeq ($(ORDENV_PLAT),ubuntu-14.04-amd64-64)
   BH_MAKE=make
   BH_MODE=intel
   FFLAGS = '$(MY_FFLAGS) -openmp -fp-model source  -warn all'
   CFLAGS = '$(MY_CFLAGS) -openmp -fp-model precise -Wall'
   ifeq ($(DOMAIN),science.gc.ca)
     BH_HOST=$(ORDENV_HOST)
     COMPILER=intel-2016.1.156
     HPCS_BASE=/fs/ssm/hpco/tmp/eccc/201402/03/base
     HPCS_COMP=/fs/ssm/hpco/exp/$(COMPILER)
     RPN_LIB=/fs/ssm/eccc/mrd/rpn/libs/16.0-alpha
     CODE_TOOLS=-p /fs/ssm/hpco/exp/jdm536/code-tools/code-tools_2.0_all
     BH_PACKAGE=/fs/ssm/hpco/exp/bh/bh_0.17_all
   else
     BH_HOST=pollux
     COMPILER=intel13sp1u2
     HPCS_BASE=hpcs/201402/02/base/
     HPCS_COMP = hpcs/201402/02/$(COMPILER)
     RPN_LIB=rpn/libs/15.2
   endif
endif

ifeq ($(ORDENV_PLAT),aix-7.1-ppc7-64)
   BH_MAKE=gmake
   BH_HOST=hadar
   BH_MODE=xlf13
   HPCS_BASE=hpcs/201402/02/base/
   BH_PACKAGE=hpcs/tools/master/bh_0.17_all
   COMPILER=xlf_13.1.0.10
   HPCS_COMP = hpcs/ext/$(COMPILER)
   RPN_LIB=rpn/libs/15.2
   FFLAGS = '$(MY_FFLAGS)'
   CFLAGS = '$(MY_FFLAGS)'
endif
