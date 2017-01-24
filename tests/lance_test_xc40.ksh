MASTER=$PWD

cd $TMPDIR
cat > vgrid_tests_${BH_PULL_SOURCE_GIT_BRANCH} <<EOF
. ssmuse-sh -d /fs/ssm/eccc/mrd/rpn/utils/16.1
cd ${MASTER}
./run_mpi.ksh
EOF

cat vgrid_tests_${BH_PULL_SOURCE_GIT_BRANCH}

ord_soumet vgrid_tests_${BH_PULL_SOURCE_GIT_BRANCH} -mach hare -cm 5G -t 1800 -cpus 36


