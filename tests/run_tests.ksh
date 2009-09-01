#!/bin/ksh

# Get list of tests
if [ -z "$*" ] ; then
  set -A tests `ls -1 src/*.ftn90 | perl -p -e 's|src/(.+)\.ftn90|$1|g'`
else
  set -A tests $*
fi

# Compile all tests
printf "Compiling tests\n"
template=Makefile.tmpl
compile_log=compile.log
rm -f ${compile_log}
for test in ${tests[*]} ; do
  printf "Building ${test} ..."
  ln -sf src/${test}.ftn90 .
  perl -p -e "s/UNIT_TEST/${test}/g" ${template} >Makefile.test
  echo "Compiling ${test}" >>${compile_log} 2>&1
  make -f Makefile.test ${test} >>${compile_log} 2>&1
  if [ ! $? -eq 0 ] ; then
    printf "\n ERROR compiling test ${test} ... aborting (try 'make -f Makefile.test ${test}' for details)\n"
    exit
  fi
  rm -f ${test}.ftn90
  make -f Makefile.test clean >/dev/null 2>&1
  printf " ok\n"
done
printf " * All Builds Succeeded\n"

# Run all tests
printf "Running tests\n"
report_file=test_report.txt
passed=0
failed=0
for test in ${tests[*]} ; do
  printf "Testing ${test} ..."
  ${test} >${test}.out 2>&1
  result=`cat ${report_file}`
  if [[ ${result} == ' ok' ]] ; then
     passed=$((passed + 1))
     printf "${result}\n"
     rm -f ${test} ${test}.out
  else
     failed=$((failed + 1))
     printf "${result} (see ${test}.out for details)\n"
  fi
  rm -f ${report_file}
done
rm -f fort.* test.bin
total=$((passed + failed))
if [[ ${failed} > 0 ]] ; then
  printf " * Failed ${failed}/${total} Tests\n"
else
  printf " * All Tests Succeeded\n"
  rm -f Makefile.test
  rm -f compile.log
fi
