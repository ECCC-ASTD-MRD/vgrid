ifeq ($(BASE_ARCH),$(EC_ARCH))
   $(info ERROR: please setup the compilation environment by typing the folowing:)
   $(info . setup.dot)
   $(error )
endif

DATA_ARCH=`uname -s`

alltests:
	make tests DATA_ARCH=Linux; \
        make tests DATA_ARCH=AIX

tests:
	mkdir -p data;\
	rm -rf data/* 2>/dev/null; \
	cd data ; \
	rm -rf WORK ; \
	mkdir -p WORK ; \
        ln -s ../data_$(DATA_ARCH)/* . ; \
	cd .. ; \
	./run_tests.ksh -only $(ONLY) -compile $(COMPILE) -execute $(EXECUTE)

tests_par:
	rm -rf data/* 2>/dev/null ;\
	cd data ;\
	rm -rf WORK ;\
	mkdir -p WORK ;\
	ln -s ../data_$(DATA_ARCH)/* . ;\
	cd .. ;\
	./run_tests_in_parallel.ksh -MAX_CPUS $(MAX_CPUS) -compile $(COMPILE) -execute $(EXECUTE)

clean:
	rm -f *.o *.mod *.out

distclean: clean
	rm -rf data/*
