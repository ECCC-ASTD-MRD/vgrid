
ifeq ($(BASE_ARCH),$(EC_ARCH))
   $(info ERROR: please setup the compilation environment by typing the folowing:)
   $(info . setup.dot)
   $(error )
endif

DESCRIP_PATH = ../src

DIST = levels_with_ref lowLevel_levels topLevel_levels use_dpidpis use_dpidpis_withref_prof use_new_gen use_new_read c_simple_pressure c_use_new_read c_use_build

default: distclean all

all: $(DIST)

levels_with_ref: levels_with_ref.ftn90 $(DESCRIP_PATH)/libdescrip.a
	s.compile -optf =$(MY_FFLAGS) -includes $(DESCRIP_PATH) -src $@.ftn90 -o $@ -libpath $(DESCRIP_PATH) -libappl rmn descrip

lowLevel_levels: lowLevel_levels.ftn90 $(DESCRIP_PATH)/libdescrip.a
	s.compile -optf =$(MY_FFLAGS) -includes $(DESCRIP_PATH) -src $@.ftn90 -o $@ -libpath $(DESCRIP_PATH) -libappl rmn descrip

topLevel_levels: topLevel_levels.ftn90 $(DESCRIP_PATH)/libdescrip.a
	s.compile -optf =$(MY_FFLAGS) -includes $(DESCRIP_PATH) -src $@.ftn90 -o $@ -libpath $(DESCRIP_PATH) -libappl rmn descrip

use_dpidpis: use_dpidpis.ftn90 $(DESCRIP_PATH)/libdescrip.a
	s.compile -optf =$(MY_FFLAGS) -includes $(DESCRIP_PATH) -src $@.ftn90 -o $@ -libpath $(DESCRIP_PATH) -libappl rmn descrip

use_dpidpis_withref_prof: use_dpidpis_withref_prof.ftn90 $(DESCRIP_PATH)/libdescrip.a
	s.compile -optf =$(MY_FFLAGS) -includes $(DESCRIP_PATH) -src $@.ftn90 -o $@ -libpath $(DESCRIP_PATH) -libappl rmn descrip

use_new_gen: use_new_gen.ftn90 $(DESCRIP_PATH)/libdescrip.a
	s.compile -optf =$(MY_FFLAGS) -includes $(DESCRIP_PATH) -src $@.ftn90 -o $@ -libpath $(DESCRIP_PATH) -libappl rmn descrip

use_new_read: use_new_read.ftn90 $(DESCRIP_PATH)/libdescrip.a
	s.compile -optf =$(MY_FFLAGS) -includes $(DESCRIP_PATH) -src $@.ftn90 -o $@ -libpath $(DESCRIP_PATH) -libappl rmn descrip

c_simple_pressure: c_simple_pressure.c $(DESCRIP_PATH)/libdescrip.a
	s.compile -bidon c -main $@ -optc =$(MY_CFLAGS) -includes $(DESCRIP_PATH) -src $@.c -o $@ -libpath $(DESCRIP_PATH) -libappl rmn descrip

c_use_new_read: c_use_new_read.c $(DESCRIP_PATH)/libdescrip.a
	s.compile -bidon c -main $@ -optc =$(MY_CFLAGS) -includes $(DESCRIP_PATH) -src $@.c -o $@ -libpath $(DESCRIP_PATH) -libappl rmn descrip

c_use_build: c_use_build.c $(DESCRIP_PATH)/libdescrip.a
	s.compile -bidon c -main $@ -optc =$(MY_CFLAGS) -includes $(DESCRIP_PATH) -src $@.c -o $@ -libpath $(DESCRIP_PATH) -libappl descrip -librmn

$(DESCRIP_PATH)/libdescrip.a: $(DESCRIP_PATH)/*.c $(DESCRIP_PATH)/*.ftn90 $(DESCRIP_PATH)/*.h $(DESCRIP_PATH)/*.hc
	cd $(DESCRIP_PATH) ;\
	gmake;\

clean:
	rm -f *.f90 *.o *.mod test_output.fst

distclean: clean
	rm -f $(DIST)

tild:
	rm -f *~
