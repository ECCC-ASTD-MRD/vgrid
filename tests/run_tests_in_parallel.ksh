#!/bin/ksh

eval `cclargs \
 -MAX_CPUS 12  12  "[Number of cpus to run tests]"\
 -compile  1   1   "[1 compile, 0 do not compile]"\
 -execute  1   1   "[1 execute, 0 do not execute]"\
 ++ $*`

set -e
# Get list of tests
if [ -z "$*" ] ; then
  set -A tests $(ls -1 src_tests/*.F90 | perl -p -e 's|src_tests/(.+)\.F90|$1|g') 
else
  set -A tests $*
fi

MASTER=$PWD

if [ ${compile} = 0 ];then
   if [ ! -d WORK/$EC_ARCH ];then
      echo "You are not compiling but there is no work dir with previously build abs, looking for dir WORK/$EC_ARCH"
      exit 1
   fi
   echo "Keeping work directory WORK/$EC_ARCH, these abs will be tested"
   cd WORK/$EC_ARCH
else
   rm -rf WORK/$EC_ARCH
   mkdir -p WORK/$EC_ARCH
   cd WORK/$EC_ARCH
   ln -s ${MASTER}/../src .
fi

NCPUS=1
DONE=0

echo "==============================================================================="
echo "Tests begin"
echo "Compiler user's flags:"
echo "   Fortran ${MY_FFLAGS} ${FFLAGS}"
echo "   C       ${MY_CFLAGS} ${CFLAGS}"
echo "Using ${MAX_CPUS} cpus"
echo

for test in ${tests[*]} ; do

   if [  ${compile} = 1 ];then
      rm -rf ${test}
      mkdir ${test}
      cd ${test}
      ln -s ${MASTER}/data_AIX .
      ln -s ${MASTER}/data_Linux .
      ln -s ${MASTER}/Makefile.tmpl
      ln -s ${MASTER}/Makefile
      ln -s ${MASTER}/src_tests
      ln -s ${MASTER}/run_tests.ksh
      ln -s ${MASTER}/unit_testing.F90
      ln -s ${MASTER}/c_ut_report.c
      ln -s ${MASTER}/ksh .
      mkdir data
      cd ..
   fi
   cd ${test}
   echo "   test ${test}"
   make tests ONLY=${test} COMPILE=${compile} EXECUTE=${execute}> ../log_${test} 2>&1 &
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
   if [ ${compile} = 1 ];then
      if grep -q 'Builds Succeeded' log_${test};then
          Build_COLOR='\033[0;32m'
      else
          Build_COLOR='\033[0;31m'
      fi
   fi
   if [ ${execute} = 1 ];then
      if grep -q 'Tests Succeeded' log_${test};then
          Test_COLOR='\033[0;32m'
      else
          Test_COLOR='\033[0;31m'
      fi
   fi
   echo "   ${Build_COLOR} $(grep 'Builds Succeeded' log_${test}) $(grep 'ERROR compiling' log_${test}) ${Test_COLOR} $(grep 'Tests Succeeded' log_${test}) $(grep Failed log_${test})   ->  '\033[0m' ${test}"
done
