ifeq ($(BASE_ARCH),$(EC_ARCH))
   $(info ERROR: please setup the compilation environment by typing the folowing:)
   $(info . setup.dot)
   $(error )
endif

# Not to run valgrind
#VALGRIND=
# To run valgrind and see result in execution directory
#VALGRIND=$PWD

DATA_ARCH=`uname -s`

alltests:
	make tests

# e.g. make tests ONLY=c_use_new_read
tests:
	mkdir -p data;\
	rm -rf data/* 2>/dev/null; \
	cd data ; \
	rm -rf WORK ; \
	mkdir -p WORK ; \
  ln -s ../data_Linux/* . ; \
	cd .. ; \
	./run_tests.ksh -only $(ONLY) -compile $(COMPILE) -execute $(EXECUTE) -valgrind $(VALGRIND)

tests_par:
	rm -rf data/* 2>/dev/null ;\
	cd data ;\
	rm -rf WORK ;\
	mkdir -p WORK ;\
	ln -s ../data_$(DATA_ARCH)/* . ;\
	cd .. ;\
	./run_tests_in_parallel.ksh -MAX_CPUS $(MAX_CPUS) -compile $(COMPILE) -execute $(EXECUTE) -valgrind $(VALGRIND)

help:
	@echo 'Possible commands:'
	@echo '   make tests'
	@echo '   make tests ONLY=HY_with_datev'
	@echo '   make tests ONLY="c_get_put_get_all c_levels"'

clean:
	rm -f *.o *.mod *.out Makefile.test

distclean: clean
	rm -rf data/*
