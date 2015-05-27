if [ ${#} -ne 1 ];then
   echo "Usage : $0 abs_name"
   exit 1
fi

valgrind --gen-suppressions=all ./${1} > $TMPDIR/list 2>&1

./parse_valgrind_suppressions.ksh $TMPDIR/list > ${1}.supp

echo "use the following command:"
echo valgrind --suppressions=${1}.supp ./${1}
