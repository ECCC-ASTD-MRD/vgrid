set -e

PROC_PATH=$(true_path ${0})
PROC_PATH=${PROC_PATH%/*}

cd ${PROC_PATH}/../src
. ./setup.dot
make clean
make
cd ../tests
make clean
./run_tests_in_parallel.ksh
