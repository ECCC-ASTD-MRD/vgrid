set -e
if [ ${0} = ${0##*/} -o ${0} = ./${0##*/} ];then
   echo "Run script with full path"
   exit 1
fi

PROC_PATH=$(true_path ${0})
PROC_PATH=${PROC_PATH%/*}

cd ${PROC_PATH}/../src
. ./setup.dot
make clean
make
cd ../tests
make clean
./run_tests_in_parallel.ksh
