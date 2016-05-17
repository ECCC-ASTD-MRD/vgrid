FTN90DECKS= \
	utils.ftn90  vgrid_descriptors.ftn90  vgrid_genab_1001.ftn90  vgrid_genab_1002_5001.ftn90  vgrid_genab_1004.ftn90   \
	vgrid_genab_2001.ftn90  vgrid_genab_5002.ftn90  
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
F90DECKS=
TMPL90DECKS=
FDECKS=
CDKDECKS=
OBJECTS= \
	utils.o  vgrid_descriptors.o  vgrid_genab_1001.o  vgrid_genab_1002_5001.o  vgrid_genab_1004.o   \
	vgrid_genab_2001.o  vgrid_genab_5002.o  
utils.o:	utils.ftn90 \
	vgrid.hf  

vgrid_descriptors.o:	vgrid_descriptors.ftn90 \
	utils.o  vgrid.hf  vgrid_genab_1001.o  vgrid_genab_1002_5001.o  vgrid_genab_1004.o   \
	vgrid_genab_2001.o  vgrid_genab_5002.o  

vgrid_genab_1001.o:	vgrid_genab_1001.ftn90 \
	vgrid.hf  

vgrid_genab_1002_5001.o:	vgrid_genab_1002_5001.ftn90 \
	vgrid.hf  

vgrid_genab_1004.o:	vgrid_genab_1004.ftn90 \
	vgrid.hf  

vgrid_genab_2001.o:	vgrid_genab_2001.ftn90 \
	vgrid.hf  

vgrid_genab_5002.o:	vgrid_genab_5002.ftn90 \
	utils.o  vgrid.hf  

