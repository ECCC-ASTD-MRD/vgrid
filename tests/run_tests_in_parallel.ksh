#!/bin/ksh

if [ "${BH_MAKE}" = "" ];then
   echo "Run this script from make:"
   echo "make tests_par"
   exit 1
fi

eval `cclargs \
 -MAX_CPUS 12  12  "[Number of cpus to run tests]"\
 ++ $*`

set -e

# Get list of tests
if [ -z "$*" ] ; then
  set -A tests $(ls -1 src/*.ftn90 | perl -p -e 's|src/(.+)\.ftn90|$1|g') 
else
  set -A tests $*
fi

MASTER=$PWD

rm -rf WORK/$EC_ARCH
mkdir -p WORK/$EC_ARCH
cd WORK/$EC_ARCH

ln -s ${MASTER}/../lib .

NCPUS=1
DONE=0

echo "==============================================================================="
echo "Tests begin"
echo
echo "Using ${MAX_CPUS} cpus"
echo

for test in ${tests[*]} ; do

   rm -rf ${test}
   mkdir ${test}
   cd ${test}
   ln -s ${MASTER}/data_AIX .
   ln -s ${MASTER}/data_Linux .
   ln -s ${MASTER}/src .
   ln -s ${MASTER}/Makefile.tmpl
   ln -s ${MASTER}/Makefile
   ln -s ${MASTER}/run_tests.ksh
   ln -s ${MASTER}/unit_testing.ftn90
   ln -s ${MASTER}/ksh .
   mkdir data

   echo "   test ${test}"
   ${BH_MAKE} tests ONLY=${test} > ../log_${test} 2>&1 &
   ((NCPUS=NCPUS+1))
   if [[ ${NCPUS} -gt ${MAX_CPUS} ]];then
      NCPUS=1
      echo "================"
      echo "waiting for cpus"
      echo "----------------"
      wait
   fi
       
   cd ..

done

wait

echo RESULTS
for test in ${tests[*]} ; do
   if grep -q 'Builds Succeeded' log_${test};then
       Build_COLOR='\033[0;32m'
   else
       Build_COLOR='\033[0;31m'
   fi
   if grep -q 'Tests Succeeded' log_${test};then
       Test_COLOR='\033[0;32m'
   else
       Test_COLOR='\033[0;31m'
   fi
   
   echo "   ${Build_COLOR} $(grep 'Builds Succeeded' log_${test}) $(grep 'ERROR compiling' log_${test}) ${Test_COLOR} $(grep 'Tests Succeeded' log_${test}) $(grep Failed log_${test})   ->  '\033[0m' ${test}"
done
