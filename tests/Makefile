
ifeq ($(BASE_ARCH),$(EC_ARCH))
   $(info ERROR: please setup the compilation environment by typing the folowing:)
   $(info . setup.dot)
   $(error )
endif

DATA_ARCH=`uname -s`

alltests:
	$(BH_MAKE) tests DATA_ARCH=Linux; \
        $(BH_MAKE) tests DATA_ARCH=AIX

tests:
	mkdir -p data;\
	rm -rf data/* 2>/dev/null; \
	cd data ; \
	rm -rf WORK ; \
	mkdir -p WORK ; \
        ln -s ../data_$(DATA_ARCH)/* . ; \
	cd .. ; \
	./run_tests.ksh -ONLY $(ONLY)

clean:
	rm -f *.o *.mod *.out

distclean: clean
	rm -rf data/*
