include setup.mk

default: libdescrip.a

dependencies.mk: *.c *.ftn90 *.h *.hc
	. ssmuse-sh -d $(HPCS_BASE) ;\
	ls *.c *.ftn90 | ./s.dependencies.pl > dependencies.mk ;\

include dependencies.mk

%.o: %.ftn90
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB);\
	s.compile -src $< -optf =$(FFLAGS)

%.o: %.c
	. ssmuse-sh -d $(HPCS_BASE) -d $(HPCS_COMP) -d $(RPN_LIB);\
	s.compile -src $< -optc =$(CFLAGS)

libdescrip.a: $(OBJECTS) dependencies.mk
	rm -f libdescrip.a && ar scru libdescrip.a *.o ;\

clean:
	rm -f *.f90 *.o *.mod dependencies.mk ;\

distclean: clean
	rm -f libdescrip.a libdescrip_debug.a ;\
