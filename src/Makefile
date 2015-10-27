#=============================================
# Usage: make
# To make libdescrip_debug.a do the following:
# make DEBUG=1
#=============================================

include setup.mk

dependencies.mk: *.c *.ftn90
	. ssmuse-sh -d $(HPCS_BASE) ;\
	ls *.c *.ftn90 | s.dependencies.pl > dependencies.mk ;\

include dependencies.mk

%.o: %.ftn90
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) ;\
	s.compile -src $< -optf =$(FFLAGS)

%.o: %.c
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) ;\
	s.compile -src $< -optc =$(CFLAGS)

libdescrip.a: $(OBJECTS)
	rm -f libdescrip.a && ar scru libdescrip.a *.o ;\

libdescrip_debug.a: $(OBJECTS)
	rm -f libdescrip_debug.a && ar scru libdescrip_debug.a *.o ;\

clean:
	rm -f *.f90 *.o *.mod dependencies.mk ;\

distclean: clean
	rm -f libdescrip.a libdescrip_debug.a ;\
