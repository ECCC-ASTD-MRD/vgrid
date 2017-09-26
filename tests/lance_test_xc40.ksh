PLAT=sles-11-broadwell-64-xc40
if [ ${ORDENV_PLAT} != ${PLAT} ];then
   echo "Looks like you'v got the wrong platform, expecting ${PLAT} got ${ORDENV_PLAT}, aborting"
   exit 1
fi

MASTER=$PWD

echo "============================================================================="
echo "First compiling tests since there is no compiler on the backend compute nodes"
 ./run_tests_in_parallel.ksh -compile 1 -execute 0

cd $TMPDIR
cat > vgrid_tests_${BH_PULL_SOURCE_GIT_BRANCH} <<EOF
. ssmuse-sh -d /fs/ssm/eccc/mrd/rpn/utils/16.1
cd ${MASTER}
./run_mpi.ksh
EOF

cat vgrid_tests_${BH_PULL_SOURCE_GIT_BRANCH}

ord_soumet vgrid_tests_${BH_PULL_SOURCE_GIT_BRANCH} -mach ${TRUE_HOST} -cm 5G -t 1800 -cpus 36
