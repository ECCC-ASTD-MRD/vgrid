# ATTENTION!
# YOU NEED TO USE THE GMAKE IN /users/dor/afsi/sul/ovbin/gmake for this
# Makefile to work, other versions installed are currently too old!
#
# common section definition
include config/config.mk

ssm: profile.sh
	echo "Creating SSM package $(SSMPACKAGE)..."
	ssmprep --basePath $(shell pwd)/$(SSMPACKAGE) -p $(SSMPACKAGE) --editorPath /bin/echo; \
        chmod a+x post-install;
	cp post-install profile.sh $(SSMPACKAGE)/.ssm.d;
	mkdir -p $(SSMPACKAGE)/src/base $(SSMPACKAGE)/src/bin $(SSMPACKAGE)/src/examples; \
	find ../lib -name "*.ftn90" -exec cp {}  $(SSMPACKAGE)/src/base \; ; \
	find ../bin -name "*.ftn90" -exec cp {} $(SSMPACKAGE)/src/bin \; ; \
g	find ../examples -name "*.ftn90" -exec cp {} $(SSMPACKAGE)/src/examples \; ; \
	s.set_dir_struct --bin --lib --include --dest=$(SSMPACKAGE); \
	find ../bin -perm -a+x -type f \( ! -name "*.ksh*" \) -exec cp {} $(SSMPACKAGE)/bin/$(BASE_ARCH) \; ; \
	find ../lib -name "*.mod" -type f -exec cp {} $(SSMPACKAGE)/include/$(EC_ARCH) \; ; \
	find ../lib -name "lib*.a" -exec cp {} $(SSMPACKAGE)/lib/$(EC_ARCH) \; ; \
	echo "Package: ${SWNAME}" > $(SSMPACKAGE)/.ssm.d/control; \
	echo "Version: ${VERSION}" >> $(SSMPACKAGE)/.ssm.d/control; \
	echo "Platform: ${SSM_MACH_ID}" >> $(SSMPACKAGE)/.ssm.d/control; \
	echo "Maintainer: ${WHO_I_AM}" >> $(SSMPACKAGE)/.ssm.d/control; \
	echo "BuildInfo:" >> $(SSMPACKAGE)/.ssm.d/control; \
	echo "Description: ${SWDESCRIP}" >> $(SSMPACKAGE)/.ssm.d/control; \
	tar cvf - $(SSMPACKAGE) | gzip -> $(SSMPACKAGE).ssm

profile.sh: profile
	echo "__version=${VERSION}" >$@ ;\
	echo "__swname=${SWNAME}" >>$@ ;\
	cat profile >>$@

clean:
	rm -rf $(SSMPACKAGE).ssm $(SSMPACKAGE) profile.sh

distclean: clean

