DATA_ARCH=`uname -s`

alltests:
	gmake tests DATA_ARCH=Linux; \
        gmake tests DATA_ARCH=AIX

tests:
	rm -rf data/* 2>/dev/null; \
	cd data ; \
	rm -rf WORK ; \
	mkdir -p WORK ; \
        ln -s ../data_$(DATA_ARCH)/* . ; \
	cd .. ; \
	./run_tests.ksh $(ONLY)

clean:
	rm -f *.f90 *.o *.mod *.out

distclean: clean
	rm -f data/*
