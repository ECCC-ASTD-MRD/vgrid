FTN90DECKS= \
	msg.ftn90  vgrid_descriptors.ftn90  
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
	msg.o  vgrid_descriptors.o  
msg.o:	msg.ftn90 \
	msg.h  
vgrid_descriptors.o:	vgrid_descriptors.ftn90 \
	vgrid.hf  msg.h  
