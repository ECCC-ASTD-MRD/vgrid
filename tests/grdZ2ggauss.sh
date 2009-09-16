#!/bin/ksh
#
rep=/users/dor/arma/ccc/boreas23/vcoord/trunk/tests
fileZ=data/Linux_pgi611/dm_5001_from_model_run
fileGG=data/Linux_pgi611/gg_5001_from_model_run
cd $TMPDIR

rm temp_gg.fst temp_px.fst

pgsm -iment ${rep}/${fileZ} -ozsrt temp_gg.fst -i <<EOF
 sortie(STD,4000,A)
 GRILLE(GAUSS,40,20,GLOBAL)
 heure(0)
 compac=-32
 setintx(LINEAIR)
 champ('P0',tout)
 champ('TT',tout)
 champ(UV,tout)
 end
EOF


r.hy2pres -iment temp_gg.fst -ozsrt temp_px.fst -var TT

editfst -s temp_px.fst -d temp_gg.fst -n -i <<EOF
 desire(-1,'PX')
EOF
mv temp_gg.fst ${rep}/${fileGG}

 


