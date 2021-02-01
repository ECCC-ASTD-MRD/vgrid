ifeq ($(BASE_ARCH),$(EC_ARCH))
   $(info ERROR: please setup the compilation environment by typing the folowing:)
   $(info . setup.dot)
   $(error )
endif

.SUFFIXES : .F90 .c .o

OPTIMIZ = -O 2
$(info OPTIMIZ is ${OPTIMIZ})

unit_testing.o: 
	s.f90 -c $(OPTIMIZ) -src unit_testing.F90

c_ut_report.o: 
	s.cc -c $(OPTIMIZ) -src c_ut_report.c

OBJETF= ./myobj/unit_testing.o

OBJETC= ./myobj/c_ut_report.o

TEST_VGRID=./vgrid_src

TEST_UNIT=./myobj

.F90:
	fn=`basename $< '.F90'`; \
	s.f90 $(OPTIMIZ) -o $$fn $(OBJETF) -I$(TEST_VGRID) -I$(TEST_UNIT) -L$(TEST_VGRID) -lvgrid -lrmn -src $<

.c:
	fn=`basename $< '.c'`; \
	s.compile $(OPTIMIZ) -o $$fn -bidon c -main $$fn -obj $(OBJETC) -includes $(TEST_VGRID) -libpath $(TEST_VGRID) -libappl vgrid -librmn rmn -src $<


alltests:
	if [ "*.F90" != "`echo *.F90`" ] ; \
	then \
	for i in *.F90 ; \
	do \
	fn=`basename $$i '.F90'`; \
	echo compile $$i building $$fn ;\
	s.f90 $$i -o $$fn $(OBJETF) $(OPTIMIZ) -I$(TEST_VGRID) -I$(TEST_UNIT) -L$(TEST_VGRID) -lvgrid -lrmn; \
	done \
	fi
	if [ "*.c" != "`echo *.c`" ] ; \
	then \
	for i in *.c ; \
	do \
	echo compile $$i building $$fn ;\
	fn=`basename $$i '.c'`; \
	s.compile $(OPTIMIZ) -src $$i -o $$fn -bidon c -main $$fn -obj $(OBJETC) -includes $(TEST_VGRID) -libpath $(TEST_VGRID) -libappl vgrid -librmn rmn; \
	done \
	fi

clean:
	rm -f *.o *.mod *.out

distclean: clean
	rm -rf WORK
