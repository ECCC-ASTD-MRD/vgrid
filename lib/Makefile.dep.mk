TMPL90DECKS=
HDECKS=
F90DECKS= \
	vgrid_descriptors.F90  vgrid_genab_1001.F90  vgrid_genab_1002_5001.F90  vgrid_genab_1004.F90  vgrid_genab_2001.F90   \
	vgrid_genab_3001.F90  vgrid_genab_5002.F90  vgrid_genab_5100.F90  vgrid_utils.F90  
PTN90DECKS=
PTNDECKS=
INCDECKS=
FTNDECKS=
FDECKS=
FHDECKS=
CDECKS=
FTN90DECKS=
CDKDECKS=
HFDECKS= \
	vgrid.hf  
CDK90DECKS=
OBJECTS= \
	vgrid_descriptors.o  vgrid_genab_1001.o  vgrid_genab_1002_5001.o  vgrid_genab_1004.o  vgrid_genab_2001.o   \
	vgrid_genab_3001.o  vgrid_genab_5002.o  vgrid_genab_5100.o  vgrid_utils.o  
vgrid_descriptors.o:	vgrid_descriptors.F90 \
	vgrid.hf  vgrid_genab_1001.o  vgrid_genab_1002_5001.o  vgrid_genab_1004.o  vgrid_genab_2001.o   \
	vgrid_genab_3001.o  vgrid_genab_5002.o  vgrid_genab_5100.o  vgrid_utils.o  

vgrid_genab_1001.o:	vgrid_genab_1001.F90 \
	vgrid.hf  

vgrid_genab_1002_5001.o:	vgrid_genab_1002_5001.F90 \
	vgrid.hf  

vgrid_genab_1004.o:	vgrid_genab_1004.F90 \
	vgrid.hf  

vgrid_genab_2001.o:	vgrid_genab_2001.F90 \
	vgrid.hf  

vgrid_genab_3001.o:	vgrid_genab_3001.F90 \
	vgrid.hf  

vgrid_genab_5002.o:	vgrid_genab_5002.F90 \
	vgrid.hf  vgrid_utils.o  

vgrid_genab_5100.o:	vgrid_genab_5100.F90 \
	vgrid.hf  vgrid_utils.o  

vgrid_utils.o:	vgrid_utils.F90 \
	vgrid.hf  

