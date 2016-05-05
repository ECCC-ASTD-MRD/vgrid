#! /usr/bin/env python
# -*- coding: utf-8 -*-

from os import environ
import sys
from bh import bhlib, actions

def _init(b):
   
   # Adjust the folowing
   environ["BH_PROJECT_NAME"] = "vgriddescriptors"
   environ["BH_PULL_SOURCE"] = "%(BH_HERE_DIR)s" % environ
   environ["BH_PULL_SOURCE_GIT_BRANCH"] = "5.5.2"

   # Add compiler if needed, select rmn lib version
   if b.mode == "intel":
       b.shell(""". ssmuse-sh -d hpcs/201402/02/base -d hpcs/201402/02/intel13sp1u2 -d rpn/libs/15.2 """, environ)
       b.shell("""export FFLAGS_INTEL='-fp-model source'""", environ)
       environ["BH_MAKE"] = 'make'
   elif b.mode == "xlf13":
       b.shell(""". ssmuse-sh -d hpcs/201402/02/base -d hpcs/ext/xlf_13.1.0.10      -d rpn/libs/15.2""", environ)
       environ["BH_MAKE"] = 'gmake' 

   # Noting to change below this line
   environ["BH_PACKAGE_NAME"]  = "%(BH_PROJECT_NAME)s" % environ
   environ["BH_PACKAGE_NAMES"] = "%(BH_PROJECT_NAME)s" % environ
   environ["BH_PACKAGE_VERSION"] = "%(BH_PULL_SOURCE_GIT_BRANCH)s-%(COMP_ARCH)s" % environ
   environ["BH_PACKAGE_CONTROL_DIR"] = "%(BH_HERE_DIR)s" % environ
   environ["SCRIPT_NAME"] = __file__+" -m "+b.mode+" -p "+b.platform % environ

def _make(b):
   
    b.shell("""
            set -e
            cd ${BH_PULL_SOURCE}
            REMOTE=$(git remote -v | grep fetch | awk '{print $2}')
            (
             CONTROL_DIR=${BH_PACKAGE_CONTROL_DIR}/${BH_PROJECT_NAME}/.ssm.d
             mkdir -p ${CONTROL_DIR}
             cp ${BH_TOP_BUILD_DIR}/post-install ${CONTROL_DIR}
             CONTROL_FILE=${CONTROL_DIR}/control.template
             echo \"Package: ${BH_PACKAGE_NAME}\"                                                                  > ${CONTROL_FILE}
             echo \"Version: x\"                                                                                  >> ${CONTROL_FILE}
             echo \"Platform: x\"                                                                                 >> ${CONTROL_FILE}
             echo \"Maintainer: cmdn (A. Plante)\"                                                                >> ${CONTROL_FILE}
             echo \"BuildInfo: git clone ${REMOTE}\"                                                              >> ${CONTROL_FILE}
             echo \"           cd in new directory\"                                                              >> ${CONTROL_FILE}
             echo \"           git checkout ${BH_PULL_SOURCE_GIT_BRANCH}"\                                        >> ${CONTROL_FILE}
             echo \"           ${SCRIPT_NAME##*/}\"                                                               >> ${CONTROL_FILE}
             echo \"Vertical grid descriptors package\"                                                           >> ${CONTROL_FILE}
             cd ${BH_BUILD_DIR}/lib
             ${BH_MAKE}
            )""",environ)
   
def _test(b):
    b.shell("""
        (
         set -e
         cd ${BH_BUILD_DIR}/tests
         ${BH_MAKE} tests
        )""",environ)

def _install(b):
    b.shell("""
        (
         set -e        
         mkdir -p ${BH_INSTALL_DIR}/lib
         cd ${BH_INSTALL_DIR}/lib
         cp ${BH_TOP_BUILD_DIR}/lib/libdescrip.a libdescrip_${BH_PULL_SOURCE_GIT_BRANCH}.a
         ln -s libdescrip_${BH_PULL_SOURCE_GIT_BRANCH}.a libdescrip.a
         mkdir -p ${BH_INSTALL_DIR}/include
         cd ${BH_INSTALL_DIR}/include
         cp ${BH_TOP_BUILD_DIR}/lib/*.mod .
         echo "write(6,'(\\\"   *          VGRID ${BH_PULL_SOURCE_GIT_BRANCH} ${COMP_ARCH} ${ORDENV_PLAT} $(date)\\\")')" > vgrid_version.cdk
         echo "write(6,'(\\\"   ********************************************************************************************\\\")')" >> vgrid_version.cdk
         mkdir -p ${BH_INSTALL_DIR}/src
         cd ${BH_TOP_BUILD_DIR}/lib
         ${BH_MAKE} clean
         cp * ${BH_INSTALL_DIR}/src
        )""")

if __name__ == "__main__":
   dr, b = bhlib.init(sys.argv, bhlib.PackageBuilder)
   b.actions.set("init", _init)
   b.actions.set("pull", [actions.pull.git_archive])
   b.actions.set("clean", ["""(cd ${BH_BUILD_DIR}/lib; ./make_dependencies.ksh; ${BH_MAKE} clean)"""])
   b.actions.set("make", _make)
   #b.actions.set("test",_test)
   b.actions.set("install", _install)
   b.actions.set("package", actions.package.to_ssm)

   b.supported_platforms = [
      "ubuntu-10.04-amd64-64",
      "ubuntu-12.04-amd64-64",
      "ubuntu-14.04-amd64-64",
      "aix-7.1-ppc7-64",
   ]
   dr.run(b)

   b.supported_modes = [
      "intel",
      "xlf13",
   ]

# Exemple d'appel:
#   ./bh_vgrid.py -p ubuntu-12.04-amd64-64 -m intel
