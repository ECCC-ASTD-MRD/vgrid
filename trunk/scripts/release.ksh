#!/bin/ksh

#set -x

# Destination path
dest_path=/users/dor/armn/rmt/data/public/vgrid_descriptors

if [ ${#} -ne 2 ];then
   echo "Usage : $0 compiler vgd_version"
   exit 1
fi

COMPILER=${1}
VERSION=${2}

#==============================================================================

set -e

#==============================================================================
# Check compiler

case $(uname) in
   Linux) if ! echo ${COMPILER} | grep -qi pgi;then
             echo "Compiler unsupported"
             exit 1
          fi
          ;;
     AIX) if ! echo ${COMPILER} | grep -qi xlf;then
             echo "Compiler unsupported"
             exit 1
          fi
          EC_ARCH=AIX  #needed because r.ssmuse.dot doesn't seem to work recursively
          ;;
       *) echo "Architecture unsupported"
          exit 1;;
esac

set +e
. r.ssmuse.dot ${COMPILER}
set -e

#==============================================================================
# Check for version already there

dest_dir=${dest_path}/${VERSION}/${EC_ARCH}

if [ -d ${dest_dir} ];then
   echo "Looks like version already exist at ${dest_dir}, remove it or increase version number"
   exit 1
fi

#==============================================================================

make clean
make all
mkdir -p ${dest_dir}/mod
ar cru ${dest_dir}/libdescrip.a *.o
cp *.mod ${dest_dir}/mod
cat >${dest_dir}/README <<EOF
  Release date: $(date)
  Version ${VERSION} built on ${EC_ARCH} using ${COMPILER}
  
  Acquired with . r.ssmuse.dot ${COMPILER} 

  SVN 
EOF

