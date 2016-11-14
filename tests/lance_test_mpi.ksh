. ssmuse-sh -d /fs/ssm/eccc/mrd/rpn/utils/16.0-beta-2

rm -f la_job

cat > la_job<<EOF
   echo "Message from \${MP_CHILD}"
   set -x
   cd $PWD
   cd ../lib
   . ./setup.dot
   cd -
   ./run_tests_in_parallel.ksh -MAX_CPUS 36
EOF

chmod +x la_job
cat la_job

r.run_in_parallel -pgm ${PWD}/la_job -npex 1 -npey 1
