#!/bin/ksh

# Get list of tests
eval `cclargs \
 -only     =-1 =-1 "[Only run the tests in the list e.g. -ONLY test1 test2]"\
 -compile  1   1   "[1 compile, 0 do not compile]"\
 -execute  1   1   "[1 execute, 0 do not execute]"\
 -valgrind ""  ""  "[Run tests with valgrind not equal to '', put result in ${valgrind} directory]"\
 ++ $*`

if [ "${only}" = -1 ];then
   set -A tests $(ls -1 src_tests/*.F90 | perl -p -e 's|src_tests/(.+)\.F90|$1|g') $(ls -1 src_tests/*.c | perl -p -e 's|src_tests/(.+)\.c|$1|g')
else
   set -A tests ${only}
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

if [ ${compile} = 1 ];then
   printf "Compiling tests\n"
   template=Makefile.tmpl
   compile_log=compile.log
   rm -f ${compile_log}

   for test in ${tests[*]} ; do

      # OpenMP ?
      if echo ${test} | grep -q OMP ;then
         OPENMP=-openmp
         OPENMP_MESSAGE='OPENMP=-openmp'
      else
         OPENMP=''
         OPENMP_MESSAGE=''
      fi

      # Fortran or C ?
      if echo ${test} | grep -q '^c_' ;then
         IS_C=yes
         EXT=.c
      else
         IS_C=no
         EXT=.F90
      fi
      printf "Building ${test} ${OPENMP} ..."
      ln -sf src_tests/${test}${EXT} .
      if [ ${IS_C} = yes ] ;then
         perl -p -e "s/UNIT_TEST_C/${test}/g" ${template} >Makefile.test
      else
         perl -p -e "s/UNIT_TEST_F/${test}/g" ${template} >Makefile.test
      fi
      echo "Compiling ${test}" >>${compile_log} 2>&1
      make -f Makefile.test ${test} OPENMP=${OPENMP} >>${compile_log} 2>&1
      if [ ! $? -eq 0 ] ; then
         printf "\n ERROR compiling test ${test} ... aborting (try 'make -f Makefile.test ${test} ${OPENMP_MESSAGE}' for details)\n"
         exit 1
      fi
      rm -f ${test}${EXT}
      make -f Makefile.test clean >/dev/null 2>&1
      printf " ok\n"
   done
   printf " * All Builds Succeeded\n"
fi

#==============
# Run all tests
if [ ${execute} = 1 ];then
   printf "Running tests\n"
   report_file=test_report.txt
   echo 'failed' > $report_file
   passed=0
   failed=0
   for test in ${tests[*]} ; do
      if echo ${test} | grep -q OMP ;then  
         if [ ${BASE_ARCH} != Linux ];then
            export OMP_NUM_THREADS=12
            MESSAGE=" with OMP_NUM_THREADS=${OMP_NUM_THREADS} "
         else
            export OMP_NUM_THREADS=1
            MESSAGE=''
         fi
      else
         export OMP_NUM_THREADS=1
         MESSAGE=''
      fi      
      printf "Testing ${test} ${MESSAGE}..."
      if [ "${valgrind}" != "" ];then	 
         d.valgrind ./${test} --leak-check=full --suppressions=/users/dor/afsg/apm/ords/cmdn/valgrid_stuff/fst.sup >${test}.out 2>&1	  
      else
         ./${test} >${test}.out 2>&1
      fi
      result=`cat ${report_file}`
      Other_Tests
      if [[ ${result} == ' ok' && ${result_o} == ' ok' ]] ; then
         passed=$((passed + 1))
         printf "${result}\n"
	 if [ "${valgrind}" != "" ];then
	    mkdir -p ${valgrind}/VALGRIND/$BASE_ARCH/$COMP_ARCH
            cp ${test}.out ${valgrind}/VALGRIND/$BASE_ARCH/$COMP_ARCH/.
         fi
         rm -f ${test} ${test}.out
      else
         failed=$((failed + 1))
         if [ "${result}" != ' ok' ];then
            if [ "${result}" == "" ];then
               result="failed"
            fi
            printf "${result} (see ${test}.out for details)\n"	
         else
            printf "   Shell script test failed, see reason above\n"
         fi
      fi
      rm -f ${report_file}
   done
   rm -f fort.* test.bin
   total=$((passed + failed))
   if [[ ${failed} > 0 ]] ; then
      printf " * Failed ${failed}/${total} Tests\n"
      exit 1
   else
      printf " * All Tests Succeeded\n"
      rm -f Makefile.test
      rm -f compile.log
   fi
fi
