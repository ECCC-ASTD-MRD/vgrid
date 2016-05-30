INCDECKS=
PTN90DECKS=
HDECKS= \
	vgrid.h  
CHDECKS=
FTNDECKS=
F90DECKS=
FDECKS=
CDKDECKS=
FTN90DECKS=
CDK90DECKS=
PTNDECKS=
HCDECKS= \
	BODY_C_compute_pressure_1001_1002.hc  BODY_C_compute_pressure_1003_5001.hc  BODY_C_compute_pressure_2001.hc  BODY_C_compute_pressure_5002_5003_5004_5005.hc  BODY_Cvgd_diag_withref.hc  
CDECKS= \
	vgrid.c  
HFDECKS= \
	vgrid_descriptors.hf  
FHDECKS=
TMPL90DECKS=
OBJECTS= \
	utils.o  vgrid.o  vgrid_descriptors.o  
utils.o:	utils.F90 \
	vgrid_descriptors.hf  
vgrid.o:	vgrid.c \
	BODY_Cvgd_diag_withref.hc  BODY_C_compute_pressure_2001.hc  vgrid.h  BODY_C_compute_pressure_5002_5003_5004_5005.hc  BODY_C_compute_pressure_1003_5001.hc   \
	BODY_C_compute_pressure_1001_1002.hc  
vgrid_descriptors.o:	vgrid_descriptors.F90 \
	vgrid_descriptors.hf  utils.o  
