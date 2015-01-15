#! /usr/bin/env python
#
# bh_toto.py

from os import environ
import sys
from bh import bhlib, actions

def _init(b):
   environ["BH_PROJECT_NAME"] = "vgriddescriptors"
   environ["BH_PACKAGE_NAMES"] = "vgriddescriptors"
   environ["BH_PACKAGE_VERSION"] = "5.3.1"
   environ["BH_PULL_SOURCE"] = "/users/dor/afsg/apm/data/vcoord/tags/release-5.3.1"
   b.shell("""export RCOMPILE=s.compile""", environ)
   environ["DISTINATION_MACH"] = "pollux"
   environ["DISTINATION_DIR"] = "/users/dor/afsg/apm/ords/vgrid/%(BH_PACKAGE_VERSION)s" % environ
   environ["RMN_VERSION"] = "15.1" % environ

   if b.mode == "intel":
       b.shell(""". ssmuse-sh -d hpcs/201402/01/base -d hpcs/201402/01/intel13sp1u2 -d rpn/libs/${RMN_VERSION}""", environ)
       environ["FC"] = "intel13sp1u21"
       environ["BH_MAKE"] = 'make'
   elif b.mode == "pgi14":
       b.shell(""". ssmuse-sh -d hpcs/201402/01/base -d hpcs/201402/01/pgi1401      -d rpn/libs/${RMN_VERSION}""", environ)
       environ["FC"] = "pgi1401"
       environ["BH_MAKE"] = 'make' 
   elif b.mode == "pgi9xx":
       b.shell(""". ssmuse-sh -d hpcs/13b/03/base -d rpn/libs/4.0;.  s.ssmuse.dot pgi9xx devtools""", environ)
       b.shell("""export RMN_EXT=_014""", environ)
       environ["FC"] = "pgi9xx"
       environ["BH_MAKE"] = 'make' 
   elif b.mode == "xlf13":
       b.shell(""". ssmuse-sh -d hpcs/201402/00/base -d hpcs/ext/xlf_13.1.0.10      -d rpn/libs/${RMN_VERSION}""", environ)
       environ["FC"] = "xlf"
       environ["BH_MAKE"] = 'gmake' 

if __name__ == "__main__":
   dr, b = bhlib.init(sys.argv, bhlib.PackageBuilder)

   b.actions.set("init", _init)
   b.actions.set("pull", actions.pull.copy_dir)
   b.actions.set("clean", ["""(cd ${BH_BUILD_DIR}; ${BH_MAKE} distclean)"""])
   b.actions.set("make", actions.make.make)
   b.actions.set("test",["""(cd ${BH_BUILD_DIR}/tests; ${BH_MAKE} tests)"""])
   b.actions.set("package",["""(cd ${BH_BUILD_DIR}/ssm; ${BH_MAKE})"""])

   b.supported_platforms = [
      "ubuntu-10.04-amd64-64",
      "ubuntu-12.04-amd64-64",
      "aix-7.1-ppc7-64",
   ]
   dr.run(b)

   b.supported_modes = [
      "intel",
      "pgi14",
      "pgi9xx",
      "xlf13",
   ]

# Exemple d'appel :
# ./bh_vgrid.py -p ubuntu-12.04-amd64-64 -m intel
