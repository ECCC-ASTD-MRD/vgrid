if [ ${0} = ${0##*/} -o ${0} = ./${0##*/} ];then
   echo "Run script with full path"
   exit 1
fi

cd ${0%/*}/../src
. ./setup.dot
make clean
make
cd ../tests
make clean
./run_tests_in_parallel.ksh
