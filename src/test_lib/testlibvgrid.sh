#!/bin/bash

printf "\n=====>  testlibvgrid.sh starts: $(date) ###########\n"
#set -e to exit on error
if [ "${COMP_ARCH}" = "" ];then
   echo "In ../src :"
   echo ". setup_{arch+compiler}.dot"
   echo "make genlib"
   echo "then try again"
   exit 1
fi
set -e
# Get list of tests
ftests=`cd src_tests; ls -1 *.F90 | perl -p -e 's|src_tests/(.+)\.F90|$1|g'; cd ..`
ctests=`cd src_tests; ls -1 *.c | perl -p -e 's|src_tests/(.+)\.c|$1|g'; cd ..`

MASTER=$PWD
rm -rf *.o *.mod vgrid_src
make unit_testing.o
make c_ut_report.o
rm -rf WORK
mkdir -p WORK
mkdir -p WORK/data_out
cd WORK
ln -s ../Makefile .
ln -s ../. myobj
ln -s ../../. vgrid_src
ln -s /home/sidr000/ords/vgrid_validation_data/b_6.5/data data

   for test in ${ftests[*]} ; do
       echo linking $test
       ln -s ../src_tests/$test .
   done
   for test in ${ctests[*]} ; do
       echo linking $test
       ln -s ../src_tests/$test .
   done
make alltests
#set +e to not exit on error
set +e
   for test in ${ftests[*]} ; do
       fn=`basename $test '.F90'`
       echo Testing $fn
       ./$fn > ${fn}.out 2>&1
       if [ -s test_report.txt ] ; then
	mv test_report.txt report_${fn}.F90
       else
        echo "failed: see ${fn}.out" > report_${fn}.F90
       fi
   done
   for test in ${ctests[*]} ; do
       fn=`basename $test '.c'`
       echo Testing $fn
       ./$fn > ${fn}.out 2>&1
       if [ -s test_report.txt ] ; then
	mv test_report.txt report_${fn}.c
       else
        echo "failed: no test_report.txt" > report_${fn}.c
       fi
   done
   echo "----"
   get_quiet=`grep -i cvgd get_quiet.out | wc -l`
   new_read_quiet=`grep -i cvgd new_read_quiet.out | wc -l`
   if [[ $get_quiet -gt 0 ]]; then
       if [[ $new_read_quiet -gt 0 ]]; then
        echo "failed: quiet option in vgrid is not working" > report_new_read_quiet.F90
       fi
   fi
   myok=`grep ok report_* | wc -l`
   myfail=`grep fail report_* | wc -l`
   echo "$myok tests ok"
   echo "$myfail tests failed"
   if [[ $myfail -gt 0 ]]; then
        grep fail report_*
   fi

   echo "All tests done in WORK directory"
   echo " "
   echo "Example to make/run one test only:"
   echo "cd WORK; make clean; make write_stat" 
   echo "write_stat"
   echo "Reminder: remove WORK to cleanup"

printf "\n=====>  testlibvgrid.sh ends: $(date) ###########\n"
