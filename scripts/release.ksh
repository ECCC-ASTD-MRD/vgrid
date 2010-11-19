#!/bin/ksh

set -x

# Destination path
dest_path=/users/dor/armn/rmt/data/public/vgrid_descriptors
dest_svn_url=svn://mrbsvn/others/vcoord

if [ ${#} -ne 2 ];then
   echo "Usage : $0 compiler vgd_version"
   exit 1
fi

COMPILER=${1}
VERSION=${2}

#==============================================================================

set -e

#==============================================================================
# Add code in tags

if [ ${COMPILER} = svn_tag ];then

   set +e

   echo "========================================"
   echo "= Make code tags in svn version begins ="
   echo "========================================"

   svn copy ${dest_svn_url}/trunk \
            ${dest_svn_url}/tags/release-${VERSION} \
      -m "Tagging the VERSION release of the vcoord project"
   
   if [ ${?} != 0 ];then
      echo "There is a problem in making the thag version ${dest_svn_url}/tags/release-${VERSION}"
      echo "Make sure this does not exist already, aborting"
      echo "You can remove tag with the following command:"
      echo "svn delete -m \"Deleting tag release-${VERSION}\" ${dest_svn_url}/tags/release-${VERSION}"
      echo "====================================================="
      echo "= Make code tags in svn version ends ABNORMALLY!!!! ="
      echo "====================================================="
      exit 1
   fi

   echo "==============================================="
   echo "= Make code tags in svn version ends normally ="
   echo "==============================================="

   exit

fi

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
. s.ssmuse.dot ${COMPILER} rmnlib-dev
set -e

#==============================================================================
# Create release tree

dest_dir=${dest_path}/${VERSION}
mkdir -p ${dest_dir}
cd ${dest_dir}
s.set_dir_struct --bin --lib --include --share
cd ${OLDPWD}

#==============================================================================
# Build and install library

make clean
make all DEBUG_FLAGS=
tmplib=.libdescrip.a
ar cru ${tmplib} *.o
install --backup -T ${tmplib} ${dest_dir}/lib/${EC_ARCH}/libdescrip.a
rm -f ${tmplib}
ar cru ${dest_dir}/lib/${EC_ARCH}/libdescrip.a *.o
install --backup -t ${dest_dir}/include/${EC_ARCH} *.mod
cat >${dest_dir}/share/README <<EOF
  Release date: $(date)
  Version ${VERSION} built on ${EC_ARCH} using ${COMPILER}
  
  Acquired with . r.ssmuse.dot ${COMPILER} 

  To get code, type:
  svn co ${dest_svn_url}/tags/release-${VERSION}
EOF

#==============================================================================
# Build and install applications
cd applications
make distclean
touch .buildstamp
sleep 1 #make sure that the timestamp of the generated files is newer
make all
install --backup -t ${dest_dir}/bin/${BASE_ARCH} $(find . -newer .buildstamp -executable -type f)
install --backup -t ${dest_dir}/include/${EC_ARCH} $(find . -name "*.mod")
cd ${OLDPWD}

#==============================================================================
# Temporary support for old architecture names

. s.old-arch.dot
#ln -sf ${dest_dir} ${dest_path}/${VERSION}/${EC_ARCH}

