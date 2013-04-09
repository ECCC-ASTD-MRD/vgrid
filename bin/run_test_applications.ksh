convert_toctoc_5002=oui

add_toctoc_and_compute_pressure=oui

print_toctoc=oui

#set -e

#set -x

#===========================================================================
#===========================================================================
#===========================================================================

rm -f data_tests
ln -s ../tests/data_$(uname -s) data_tests

#===========================================================================
#===========================================================================
#===========================================================================
if [ ${convert_toctoc_5002} = oui ];then
   rm -f ${TMPDIR}/dm2007050912-00-00_001_sans_p0
   editfst -s data/dm2007050912-00-00_001 -d ${TMPDIR}/dm2007050912-00-00_001_sans_p0 <<EOF
      exclure(-1,P0)
EOF
   if [ ${?} -ne 0 ];then
      echo "Problem with editfst"
      exit
   fi
   #===========================================================================
   if [ oui = oui ];then
      # Test to convert !! and convert all !!SF to P0 if P0 not there
      # Write result in separet file
      ./convert_toctoc_5002 -s ${TMPDIR}/dm2007050912-00-00_001_sans_p0 -d ${TMPDIR}/out_convert_toctoc_5002 
      if [ ${?} -ne 0 ];then
         echo "TEST 1 FAILLED!"
         exit
      fi
   fi
   #===========================================================================
   if [ oui = oui ];then
      # Test to convert !! and convert all !!SF to P0 if P0 not there
      # Write result in same file and rm !!SF and old !!
      rm -f  ${TMPDIR}/dm2007050912-00-00_001_same_file
      cp ${TMPDIR}/dm2007050912-00-00_001_sans_p0 ${TMPDIR}/dm2007050912-00-00_001_same_file
      ./convert_toctoc_5002 -samefile -s ${TMPDIR}/dm2007050912-00-00_001_same_file
      if [ ${?} -ne 0 ];then
         echo "TEST 2 FAILLED!"
         exit
      fi
   fi
   #===========================================================================
   if [ oui = oui ];then
      # Selec one time, convert and recompute PX and compaire result with model PX
      rm -f ${TMPDIR}/tempo
      editfst -s ${TMPDIR}/dm2007050912-00-00_001_sans_p0 -d ${TMPDIR}/tempo <<EOF
         desire(-1,['>>','^^','!!'])
        desire(-1,-1,-1,338995474)
EOF
      ./convert_toctoc_5002 -samefile -s ${TMPDIR}/tempo
      rm -f ${TMPDIR}/px
      compute_pressure -s ${TMPDIR}/tempo -d ${TMPDIR}/px -var ALL_LEVELS
      STATUS=$(fstcomp -ne -a ${TMPDIR}/px -b ${TMPDIR}/dm2007050912-00-00_001_sans_p0 | grep PX | awk '{if($7 > 5.e-4)print "NOTOK"}')
      if [ "${STATUS}" = 'NOTOK' ];then
         echo "TEST 3 FAILED"
         exit 1
      fi
   fi
fi

#===========================================================================
#===========================================================================
#===========================================================================

if [ ${add_toctoc_and_compute_pressure} = oui ];then

   for ITEM in east.eta glbeta glbhyb regeta reghyb
   do

     echo ==============
     echo ${ITEM}
     echo ==============
      rm -f $TMPDIR/toctoc
      ./add_toctoc -s data/${ITEM} -d $TMPDIR/toctoc
      if [ ${?} -ne 0 ];then
         echo "TEST add_toctoc with ${ITEM} FAILLED!"
         exit
      fi
      rm -f $TMPDIR/px
      ./compute_pressure -s data/${ITEM} -d $TMPDIR/px -var MOMENTUM
      STATUS=$(fstcomp -ne -a $TMPDIR/px -b data/${ITEM} | grep PX | awk '{if($7 > 6.e-6)print "NOTOK"}')
      if [ "${STATUS}" = 'NOTOK' ];then
         echo "TEST compute_pressure with ${ITEM} FAILED"
         exit 1
      fi
      echo "Test on ${ITEM} OK"
      #pause

   done

fi
#===========================================================================
#===========================================================================
#===========================================================================
if [ ${print_toctoc} = oui ];then

    DEBUG=non

   for ITEM in dm_2001_5001_from_editfst dm_1001_from_model_run dm_1002_from_model_run dm_1003_from_pgsm_lam_east_ops 2001_from_model_run dm_5001_from_model_run dm_5001_from_model_run dm_5001_from_model_run
   do
     
      kinds='=-1'
      if [ ${ITEM} = dm_2001_5001_from_editfst ];then
         kinds='2 5'
      fi
      
      for kind in ${kinds}
      do

	 lable=_kind_${kind}
         if [ ${kind} = =-1 ];then
            lable=""
         fi

         ./print_toctoc -fst data_tests/${ITEM} -no_box -kind ${kind} > to_erase.txt 2>&1
         FILE=data/$(uname -s)/print_toctoc_${ITEM}${lable}.txt
         #cp  to_erase.txt ${FILE}
         diff to_erase.txt ${FILE}
         if [ $? != 0 ];then
            echo "ERROR 1: ./print_toctoc on data/${ITEM} do not mach with ${FILE}"
	    echo "faire : xxdiff  to_erase.txt ${FILE}"
	    if [ ${DEBUG} = oui ];then
		xxdiff  to_erase.txt ${FILE}
	    else
		exit
	    fi
         fi

         ./print_toctoc -fst data_tests/${ITEM} -ip1m_only -kind ${kind} > to_erase.txt 2>&1
         FILE=data/$(uname -s)/print_toctoc_ip1m_only_${ITEM}${lable}.txt
         #cp to_erase.txt ${FILE}
         diff -b to_erase.txt ${FILE}
         if [ $? != 0 ];then
            echo "ERROR 2: ./print_toctoc -ip1m_only on data_tests/${ITEM} do not mach with ${FILE}"
            echo "faire : xxdiff  to_erase.txt ${FILE}"
	    if [ ${DEBUG} = oui ];then
		xxdiff  to_erase.txt ${FILE}
	    else
		exit
	    fi
         fi

         ./print_toctoc -fst data_tests/${ITEM} -ip1t_only -kind ${kind} > to_erase.txt 2>&1
         FILE=data/$(uname -s)/print_toctoc_ip1t_only_${ITEM}${lable}.txt
         #cp to_erase.txt ${FILE}
         diff -b to_erase.txt ${FILE}
         if [ $? != 0 ];then
            echo "ERROR 3: ./print_toctoc -ip1t_only on data_tests/${ITEM} do not mach with ${FILE}"
            echo "faire : xxdiff  to_erase.txt ${FILE}"
	    if [ ${DEBUG} = oui ];then
		xxdiff  to_erase.txt ${FILE}
	    else
		exit
	    fi
         fi

         ./print_toctoc -fst data_tests/${ITEM} -ip1m_only -out output_file.txt -kind ${kind} > to_erase.txt 2>&1
         FILE=data/$(uname -s)/print_toctoc_ip1m_only_out_${ITEM}${lable}.txt
         #cp output_file.txt ${FILE}
         diff output_file.txt ${FILE}
         if [ $? != 0 ];then
            echo "ERROR 4.2: ./print_toctoc on data_tests/${ITEM} do not mach with ${FILE}"
	    echo "faire : xxdiff  output_file.txt ${FILE}"
	    if [ ${DEBUG} = oui ];then
		xxdiff  to_erase.txt ${FILE}
	    else
		exit
	    fi
         fi

      done

   done

   #rm -f to_erase.txt
fi
#===========================================================================
#===========================================================================
#===========================================================================

echo "TEST(S) OK"
