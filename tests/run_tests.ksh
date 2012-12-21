#!/bin/ksh

# Get list of tests
if [ -z "$*" ] ; then
  set -A tests `ls -1 src/*.ftn90 | perl -p -e 's|src/(.+)\.ftn90|$1|g'`
else
  set -A tests $*
fi

#==============
function Other_Tests {

   result_o=' ok'
   if [ ${test} = get_quiet ];then
       . ksh/other_test.dot
   fi

}
#==============
# Compile all tests
printf "Compiling tests\n"
template=Makefile.tmpl
compile_log=compile.log
rm -f ${compile_log}
for test in ${tests[*]} ; do
  if echo ${test} | grep -q OMP ;then
      OPENMP=-openmp
  else
      OPENMP=''
  fi  
  printf "Building ${test} ${OPENMP} ..."
  ln -sf src/${test}.ftn90 .
  perl -p -e "s/UNIT_TEST/${test}/g" ${template} >Makefile.test
  echo "Compiling ${test}" >>${compile_log} 2>&1
  gmake -f Makefile.test ${test} OPENMP=${OPENMP} >>${compile_log} 2>&1
  if [ ! $? -eq 0 ] ; then
    printf "\n ERROR compiling test ${test} ... aborting (try 'gmake -f Makefile.test ${test}' for details)\n"
    exit
  fi
  rm -f ${test}.ftn90
  gmake -f Makefile.test clean >/dev/null 2>&1
  printf " ok\n"
done
printf " * All Builds Succeeded\n"

#==============
# Run all tests
printf "Running tests\n"
report_file=test_report.txt
passed=0
failed=0
for test in ${tests[*]} ; do
  if echo ${test} | grep -q OMP ;then
    export OPM_NUM_THREADS=4
    MESSAGE=" whith OPM_NUM_THREADS=${OPM_NUM_THREADS} "
  else
    export OPM_NUM_THREADS=1
    MESSAGE=''
  fi      
  printf "Testing ${test} ${MESSAGE}..."
  ${test} >${test}.out 2>&1
  result=`cat ${report_file}`
  Other_Tests
  if [[ ${result} == ' ok' && ${result_o} == ' ok' ]] ; then
     passed=$((passed + 1))
     printf "${result}\n"
     rm -f ${test} ${test}.out
  else
     failed=$((failed + 1))
     if [ "${result}" != ' ok' ];then
        printf "${result} (see ${test}.out for details)\n"
     else
        printf "   Shell script test failled, see reason above"
     fi
  fi
  rm -f ${report_file}
done
rm -f fort.* test.bin
total=$((passed + failed))
if [[ ${failed} > 0 ]] ; then
  printf " * Failed ${failed}/${total} Tests\n"
  exit
else
  printf " * All Tests Succeeded\n"
  rm -f Makefile.test
  rm -f compile.log
fi
