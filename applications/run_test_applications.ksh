rm -f data/dm2007050912-00-00_001_sans_p0

editfst -s data/dm2007050912-00-00_001 -d data/dm2007050912-00-00_001_sans_p0 <<EOF
 exclure(-1,P0)
EOF

if [ ${?} -ne 0 ];then
    echo "Problem with editfst"
    exit
fi


#===========================================================================

if [ oui = oui ];then

   # Test to convert !! and convert all !!SF to P0 if P0 not there
   # Write result in seperet
   ./convert_toctoc_5002 -s data/dm2007050912-00-00_001_sans_p0 -d data/out_convert_toctoc_5002 

   if [ ${?} -ne 0 ];then
       echo "TEST 1 FAILLED!"
       exit
   fi

fi

#===========================================================================
if [ oui = oui ];then

   # Test to convert !! and convert all !!SF to P0 if P0 not there
   # Write result in same file and rm !!SF and old !!
   rm -f  data/dm2007050912-00-00_001_same_file
   cp data/dm2007050912-00-00_001_sans_p0 data/dm2007050912-00-00_001_same_file

   convert_toctoc_5002 -samefile -s data/dm2007050912-00-00_001_same_file

   if [ ${?} -ne 0 ];then
       echo "TEST 2 FAILLED!"
       exit
   fi

fi

#===========================================================================
if [ oui = oui ];then

   # Selec on time, convert and recompute PX and compaire result with model PX

   rm -f data/tempo
   editfst -s data/dm2007050912-00-00_001_sans_p0 -d data/tempo <<EOF
     desire(-1,['>>','^^','!!'])
     desire(-1,-1,-1,338995474)
EOF

   convert_toctoc_5002 -samefile -s data/tempo

   rm -f data/px
   compute_pressure -s data/tempo -d data/px


   STATUS=$(fstcomp -ne -a data/px -b data/dm2007050912-00-00_001_sans_p0 | grep PX | awk '{if($7 > 5.e-4)print "NOTOK"}')

   if [ "${STATUS}" = 'NOTOK' ];then

      echo "TEST 3 FAILED"
      exit 1

   fi

fi

#===========================================================================
# Remove temp file
rm -f data/out_convert_toctoc_5002
rm -f data/dm2007050912-00-00_001_sans_p0
rm -f data/dm2007050912-00-00_001_same_file
rm -f data/tempo
rm -f data/px
#===========================================================================

echo "ALL TESTS OK"
