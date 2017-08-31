# For XC40
# Logon mom node than lanch tha script
# qsub -I -lselect=1:vntype=cray_compute -l place=scatter

cd ../lib
. ./setup.dot
cd -

rm -f la_job

cat > la_job<<EOF
   echo "=============="
   echo "MPI job begins"
   echo "--------------"
   echo "Message from \${MP_CHILD}"
   cd ${PWD}
   ./run_tests_in_parallel.ksh -MAX_CPUS 36 -compile 0 -execute 1
   echo "============"
   echo "MPI job ends"
   echo "------------"
EOF

chmod +x la_job
#cat la_job

r.run_in_parallel -pgm ${PWD}/la_job -npex 1 -npey 1

