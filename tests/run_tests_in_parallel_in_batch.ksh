cat > job_run_tests_in_parallel <<EOF
   cd ${PWD}/../src
   . ./setup_gpsc.dot
   cd ../tests
   ./run_tests_in_parallel.ksh -MAX_CPUS 16
EOF

ord_soumet job_run_tests_in_parallel -mach gpsc1 -cm 8G -t 1800 -cpus 16
