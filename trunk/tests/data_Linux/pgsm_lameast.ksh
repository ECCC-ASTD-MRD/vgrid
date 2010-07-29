#!/bin/ksh

cat > directives <<EOF
 SORTIE(STD,1000,A)
 GRILLE(PS,30,30,-140.,310.,15000.,20.,NORD)
 HEURE(-1)
 CHAMP(TT)
 CHAMP(P0)
 END
EOF

rm -f o
pgsm -iment /cnfs/ops/production/gridpt/dbase/prog/lam/east.eta/2010060812_000_2.5km\
     -ozsrt o -i directives

rm -f px
r.hy2pres -iment o -ozsrt px -var TT 

rm -f o_px
editfst -s o px -d o_px -i 0


 
