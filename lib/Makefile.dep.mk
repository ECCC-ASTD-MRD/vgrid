FTN90DECKS= \
	msg.ftn90  utils.ftn90  vgrid_descriptors.ftn90  vgrid_genab_1001.ftn90  vgrid_genab_1002_5001.ftn90   \
	vgrid_genab_1004.ftn90  vgrid_genab_2001.ftn90  vgrid_genab_5002.ftn90  
INCDECKS=
CDK90DECKS=
PTN90DECKS=
PTNDECKS=
HFDECKS= \
	vgrid.hf  
CDECKS=
HDECKS= \
	msg.h  
FHDECKS=
FTNDECKS=
F90DECKS=
TMPL90DECKS=
FDECKS=
CDKDECKS=
OBJECTS= \
	msg.o  utils.o  vgrid_descriptors.o  vgrid_genab_1001.o  vgrid_genab_1002_5001.o   \
	vgrid_genab_1004.o  vgrid_genab_2001.o  vgrid_genab_5002.o  
msg.o:	msg.ftn90 \
	msg.h  

utils.o:	utils.ftn90 \
	vgrid.hf  msg.h  

vgrid_descriptors.o:	vgrid_descriptors.ftn90 \
	utils.o  vgrid.hf  msg.h  vgrid_genab_1001.o  vgrid_genab_1002_5001.o   \
	vgrid_genab_1004.o  vgrid_genab_2001.o  vgrid_genab_5002.o  

vgrid_genab_1001.o:	vgrid_genab_1001.ftn90 \
	vgrid.hf  msg.h  

vgrid_genab_1002_5001.o:	vgrid_genab_1002_5001.ftn90 \
	vgrid.hf  msg.h  

vgrid_genab_1004.o:	vgrid_genab_1004.ftn90 \
	vgrid.hf  msg.h  

vgrid_genab_2001.o:	vgrid_genab_2001.ftn90 \
	vgrid.hf  msg.h  

vgrid_genab_5002.o:	vgrid_genab_5002.ftn90 \
	utils.o  vgrid.hf  msg.h  

