#!/bin/bash

#remove the diff tests for all tests except for vgrid_sample

convert_toctoc_5002=oui
add_toctoc=oui
compute_pressure=oui
print_toctoc=oui
vgrid_sample=oui

set -e
#set -x

#===========================================================================
#===========================================================================
#===========================================================================
BIN_PATH=${1}
VALIDATION_DATA=${2}
TMPDIR=$(pwd)/data_out

#===========================================================================
#===========================================================================
#===========================================================================
if [ ${convert_toctoc_5002} = oui ];then
   rm -f ${TMPDIR}/dm2007050912-00-00_001_sans_p0
   editfst -s ${VALIDATION_DATA}/data_tests/dm2007050912-00-00_001 -d ${TMPDIR}/dm2007050912-00-00_001_sans_p0 <<EOF
      exclure(-1,P0)
EOF
   if [ ${?} -ne 0 ];then
      echo "Problem with editfst"
      exit 1
   fi
   #===========================================================================
   if [ oui = oui ];then
      # Test to convert !! and convert all !!SF to P0 if P0 not there
      # Write result in separate file
      ${BIN_PATH}/r.convert_toctoc_5002 -s ${TMPDIR}/dm2007050912-00-00_001_sans_p0 -d ${TMPDIR}/out_convert_toctoc_5002 
      if [ ${?} -ne 0 ];then
         echo "TEST 1 FAILLED!"
         exit 1
      fi
   fi
   #===========================================================================
   if [ oui = oui ];then
      # Test to convert !! and convert all !!SF to P0 if P0 not there
      # Write result in same file and rm !!SF and old !!
      rm -f  ${TMPDIR}/dm2007050912-00-00_001_same_file
      cp ${TMPDIR}/dm2007050912-00-00_001_sans_p0 ${TMPDIR}/dm2007050912-00-00_001_same_file
      ${BIN_PATH}/r.convert_toctoc_5002 -samefile -s ${TMPDIR}/dm2007050912-00-00_001_same_file
      if [ ${?} -ne 0 ];then
         echo "TEST 2 FAILLED!"
         exit 1
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
      ${BIN_PATH}/r.convert_toctoc_5002 -samefile -s ${TMPDIR}/tempo
      rm -f ${TMPDIR}/px
      ${BIN_PATH}/r.compute_pressure -s ${TMPDIR}/tempo -d ${TMPDIR}/px -var ALL_LEVELS
      fstcomp -ne -a ${TMPDIR}/px -b ${TMPDIR}/dm2007050912-00-00_001_sans_p0 | grep PX | awk '{if($7 > 5.e-4)print "NOTOK"}' > res
#xrec -imflds ${TMPDIR}/px 
      if grep -q NOTOK res;then
         echo "TEST 3 FAILED"
         exit 1
      fi
   fi
fi

#===========================================================================
#===========================================================================
#===========================================================================

if [ ${add_toctoc} = oui ];then

   echo "==============="
   echo "Test add_toctoc"
   echo "---------------"

   for ITEM in east.eta glbeta glbhyb regeta reghyb dm_1001_from_model_run
   do

     echo ==============
     echo ${ITEM}
     echo ==============
      rm -f $TMPDIR/toctoc
      ${BIN_PATH}/r.add_toctoc -s ${VALIDATION_DATA}/data_tests/${ITEM} -d $TMPDIR/toctoc -allow_sigma
      if [ ${?} -ne 0 ];then
         echo "TEST add_toctoc with ${ITEM} FAILLED!"
         exit 1
      fi
   done
fi

#===========================================================================
#===========================================================================
#===========================================================================

if [ ${compute_pressure} = oui ];then

   ITEM=dm_21001_from_model_run_NON_SLEVE
   echo "TESTING ${VAR} ${VALIDATION_DATA}/data_tests/${ITEM}"
   rm -f $TMPDIR/px
   #${BIN_PATH}/r.compute_pressure -s ${VALIDATION_DATA}/data_tests/${ITEM} -d $TMPDIR/px -var MOMENTUM
   if [ ${?} == 0 ];then
	   echo 'The previous command should have produce an error'
	   #exit 1
   fi

   #for ITEM in east.eta glbeta glbhyb regeta reghyb dm_1001_from_model_run dm_1002_from_model_run dm_1003_from_pgsm_lam_east_ops dm_5001_from_model_run dm_5001_from_model_run dm_5001_from_model_run dm_5002_from_model_run dm_5003_from_model_run dm_5004_from_model_run dm_5005_from_model_run dm_5100_from_model_run dm_5999_from_model_run;do
   #enleve mauvais fichier dm_5002_from_model_run
   for ITEM in east.eta glbeta glbhyb regeta reghyb dm_1001_from_model_run dm_1002_from_model_run dm_1003_from_pgsm_lam_east_ops dm_5001_from_model_run dm_5001_from_model_run dm_5001_from_model_run dm_5003_from_model_run dm_5004_from_model_run dm_5005_from_model_run dm_5100_from_model_run dm_5999_from_model_run;do
      VAR_LIST=MOMENTUM
      if [ ${ITEM} = 2001_from_model_run ];then
         VAR_LIST=TT
      elif [ ${ITEM} = dm_5002_from_model_run ];then
	      VAR_LIST='MOMENTUM THERMO'
      elif [ ${ITEM} = dm_5003_from_model_run ];then
	      VAR_LIST='MOMENTUM THERMO'
      elif [ ${ITEM} = dm_5004_from_model_run ];then
	      VAR_LIST='MOMENTUM THERMO'
      elif [ ${ITEM} = dm_5005_from_model_run ];then
	      VAR_LIST='MOMENTUM THERMO'
      elif [ ${ITEM} = dm_5100_from_model_run ];then
	      VAR_LIST='MOMENTUM THERMO'
      fi

      for VAR in $VAR_LIST;do

         echo "TESTING ${VAR} ${VALIDATION_DATA}/data_tests/${ITEM}"
         rm -f $TMPDIR/px
         ${BIN_PATH}/r.compute_pressure -allow_sigma -s ${VALIDATION_DATA}/data_tests/${ITEM} -d $TMPDIR/px -var ${VAR}
         fstcomp -ne -a $TMPDIR/px -b ${VALIDATION_DATA}/data_tests/${ITEM} | sed -e 's/</ /' -e 's/>/ /' | grep PX | awk '{if($7 > 6.e-6)print "NOTOK"}' > res
         if grep -q NOTOK res;then
            echo "TEST compute_pressure with ${ITEM} FAILED for $VAR"
            exit 1
         fi
         echo "Test on ${VAR} ${ITEM} OK"
         #pause
      done

   done

fi
#===========================================================================
#===========================================================================
#===========================================================================
if [ ${print_toctoc} = oui ];then
    
   echo =================
   echo Test print_toctoc
   echo -----------------

   DEBUG=non

   cat > no_ip1t.txt<<EOF
 dm_1003_from_pgsm_lam_east_ops
EOF

   #enleve mauvais fichier dm_5002_from_model_run
   #for ITEM in dm_2001_5001_from_editfst dm_1001_from_model_run dm_1002_from_model_run dm_1003_from_pgsm_lam_east_ops 2001_from_model_run dm_4001_from_model_run dm_5001_from_model_run dm_5001_from_model_run dm_5001_from_model_run dm_5002_from_model_run dm_5003_from_model_run dm_5004_from_model_run dm_5005_from_model_run dm_5100_from_model_run dm_5999_from_model_run dm_21001_from_model_run_NON_SLEVE dm_21001_from_model_run_SLEVE dm_21002_from_model_run_NON_SLEVE dm_21002_from_model_run_SLEVE

   for ITEM in dm_2001_5001_from_editfst dm_1001_from_model_run dm_1002_from_model_run dm_1003_from_pgsm_lam_east_ops 2001_from_model_run dm_4001_from_model_run dm_5001_from_model_run dm_5001_from_model_run dm_5001_from_model_run dm_5003_from_model_run dm_5004_from_model_run dm_5005_from_model_run dm_5100_from_model_run dm_5999_from_model_run dm_21001_from_model_run_NON_SLEVE dm_21001_from_model_run_SLEVE dm_21002_from_model_run_NON_SLEVE dm_21002_from_model_run_SLEVE
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

	      echo "Test print_toctoc with kind=${kind} on ${VALIDATION_DATA}/data_tests/${ITEM}"

         ${BIN_PATH}/r.print_toctoc -fst ${VALIDATION_DATA}/data_tests/${ITEM} -no_box -kind ${kind} > to_erase.txt 2>&1
         if [ $? != 0 ];then
            echo "ERROR 1:  r.print_toctoc on ${VALIDATION_DATA}/data/${ITEM} -no_box -kind ${kind}"
		      exit 1
         fi

         ${BIN_PATH}/r.print_toctoc -fst ${VALIDATION_DATA}/data_tests/${ITEM} -no_box -kind ${kind} -convip > to_erase.txt 2>&1
         if [ $? != 0 ];then
            echo "ERROR 1:  r.print_toctoc on ${VALIDATION_DATA}/data/${ITEM} -no_box -kind ${kind} -convip "
		      exit 1
         fi

         ${BIN_PATH}/r.print_toctoc -fst ${VALIDATION_DATA}/data_tests/${ITEM} -ip1m_only -kind ${kind} > to_erase.txt 2>&1
         if [ $? != 0 ];then
            echo "ERROR 2:  r.print_toctoc -ip1m_only on ${VALIDATION_DATA}/data_tests/${ITEM} "
		      exit 1
         fi

         ${BIN_PATH}/r.print_toctoc -fst ${VALIDATION_DATA}/data_tests/${ITEM} -ip1t_only -kind ${kind} > to_erase.txt 2>&1
         if [ $? != 0 ];then
		      echo "ERROR 3:  r.print_toctoc -ip1t_only on ${VALIDATION_DATA}/data_tests/${ITEM} "
		      exit 1
      	fi

         ${BIN_PATH}/r.print_toctoc -fst ${VALIDATION_DATA}/data_tests/${ITEM} -ip1m_only -out output_file.txt -kind ${kind} > to_erase.txt 2>&1
         if [ $? != 0 ];then
            echo "ERROR 4.2:  r.print_toctoc on ${VALIDATION_DATA}/data_tests/${ITEM}"
		      exit 1
         fi

      done

   done

   #rm -f to_erase.txt
fi

#===========================================================================
#===========================================================================
#===========================================================================

if [ ${vgrid_sample} = oui ];then

   echo "==================="
   echo "Test r.vgrid_sample"
   echo "-------------------"

   echo "Test r.vgrid_sample 1"
   # r.vgrid_sample test 1
   rm -rf out_dir vgrid_sample.nml
   ${BIN_PATH}/r.vgrid_sample -out_dir out_dir > listing_r.vgrid_sample    
   #mkdir -p ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}
   #rm -rf ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}/out_dir
   #cp -r out_dir ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}

   # Test if all files produced by the current program
   # are in the validation directory
   OK=1
   for file in out_dir/*;do
	   if [ ! -f ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}/out_dir/${file##*/} ];then
	      echo "ERROR: new file \"${file##*/}\" not in validation data ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}/out_dir"
	      OK=0
	   fi
   done
    
   _file_checked=0
    for file in $(ls ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}/out_dir/*);do
	   ((n_file_checked=n_file_checked+1))
      ${BIN_PATH}/r.print_toctoc -no_box -fst out_dir/${file##*/} > ${file##*/}_tests.txt
	   ${BIN_PATH}/r.print_toctoc -no_box -fst ${file} > ${file##*/}_ctrl.txt
	   if diff  ${file##*/}_tests.txt ${file##*/}_ctrl.txt;then
         :
      else
	      OK=0
         echo "ERROR with ${file##*/} on r.vgrid_sample test 1"
      fi
   done
   if [ ${OK} = 0 ];then
      echo "ERROR with r.vgrid_sample for r.vgrid_sample test 1 for some Vcode, see diff output above"
      exit 1
   fi    
   if [ ${n_file_checked} = 0 ];then
	   echo "ERROR with r.vgrid_sample for r.vgrid_sample test 1 no file were checked"
	   exit 1
   fi

   # r.vgrid_sample test 2
   echo "Test r.vgrid_sample 2"
   cat > vgrid_sample.nml<<EOF
   &cfg
   vc_eta%ptop_8 = 1000.
   levs_eta = 0.0 ,0.5, 1.0
/
EOF
   rm -rf out_dir_2
   ${BIN_PATH}/r.vgrid_sample -out_dir out_dir_2 > listing_r.vgrid_sample_2
   #mkdir -p ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}
   #rm -rf ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}/out_dir_2
   #cp -r out_dir_2 ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}
   # Test if all files produced by the current program
   # are in the validation directory
   OK=1
   for file in out_dir_2/*;do
	   if [ ! -f ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}/out_dir_2/${file##*/} ];then
	      echo "ERROR: new file \"${file##*/}\" not in validation data ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}/out_dir_2"
	      OK=0
	   fi
   done
   n_file_checked=0
   for file in $(ls ${VALIDATION_DATA}/data_tests_res/vgrid_sample/${EC_ARCH}/out_dir_2/*);do
      ((n_file_checked=n_file_checked+1))
	   ${BIN_PATH}/r.print_toctoc -no_box -fst out_dir_2/${file##*/} > ${file##*/}_tests.txt
	   ${BIN_PATH}/r.print_toctoc -no_box -fst ${file} > ${file##*/}_ctrl.txt
	   if diff  ${file##*/}_tests.txt ${file##*/}_ctrl.txt;then
         :
      else
	      OK=0
         echo "ERROR with ${file##*/} on r.vgrid_sample test 2"
      fi
   done
   if [ ${OK} = 0 ];then
      echo "ERROR with r.vgrid_sample for r.vgrid_sample test 2 for some Vcode, see diff output above"
      exit 1
   fi
   if [ ${n_file_checked} = 0 ];then
	   echo "ERROR with r.vgrid_sample for r.vgrid_sample test 2 no file were checked"
	   exit 1
   fi

fi

echo 
echo "TEST(S) OK"
echo "Reminder: remove WORK to cleanup"
printf "\n=====>  testutilvgrid.sh ends: $(date) ###########\n"
echo
