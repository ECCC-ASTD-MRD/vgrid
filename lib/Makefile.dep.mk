FTN90DECKS=
INCDECKS=
CDK90DECKS=
PTN90DECKS=
PTNDECKS=
HFDECKS= \
	vgrid.hf  
CDECKS=
HDECKS=
FHDECKS=
FTNDECKS=
F90DECKS= \
	utils.F90  vgrid_descriptors.F90  vgrid_genab_1001.F90  vgrid_genab_1002_5001.F90  vgrid_genab_1004.F90   \
	vgrid_genab_2001.F90  vgrid_genab_5002.F90  
TMPL90DECKS=
FDECKS=
CDKDECKS=
OBJECTS= \
	utils.o  vgrid_descriptors.o  vgrid_genab_1001.o  vgrid_genab_1002_5001.o  vgrid_genab_1004.o   \
	vgrid_genab_2001.o  vgrid_genab_5002.o  
utils.o:	utils.F90 \
	vgrid.hf  

vgrid_descriptors.o:	vgrid_descriptors.F90 \
	utils.o  vgrid.hf  vgrid_genab_1001.o  vgrid_genab_1002_5001.o  vgrid_genab_1004.o   \
	vgrid_genab_2001.o  vgrid_genab_5002.o  

vgrid_genab_1001.o:	vgrid_genab_1001.F90 \
	vgrid.hf  

vgrid_genab_1002_5001.o:	vgrid_genab_1002_5001.F90 \
	vgrid.hf  

vgrid_genab_1004.o:	vgrid_genab_1004.F90 \
	vgrid.hf  

vgrid_genab_2001.o:	vgrid_genab_2001.F90 \
	vgrid.hf  

vgrid_genab_5002.o:	vgrid_genab_5002.F90 \
	utils.o  vgrid.hf  

