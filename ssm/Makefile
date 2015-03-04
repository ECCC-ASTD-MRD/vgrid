include config/config.mk

ssm:
	echo "Creating SSM package $(SSMPACKAGE)..."
	mkdir -p $(SSMPACKAGE)/.ssm.d; \
	chmod +x post-install; \
	cp post-install $(SSMPACKAGE)/.ssm.d; \
	mkdir -p $(SSMPACKAGE)/lib; \
	mkdir -p $(SSMPACKAGE)/include; \
	mkdir -p $(SSMPACKAGE)/src; \
	echo   "print*,'  *          VGRID ${BH_PULL_SOURCE_GIT_BRANCH} ${COMP_ARCH} ${ORDENV_PLAT} $$(date)'" > $(SSMPACKAGE)/include/vgrid_version.cdk
	echo   "print*,'  ********************************************************************************************'" >> $(SSMPACKAGE)/include/vgrid_version.cdk
	find ../lib -name "*.mod" -type f -exec cp {} $(SSMPACKAGE)/include \; ; \
	find ../lib -name "lib*.a"        -exec cp {} $(SSMPACKAGE)/lib \; ; \
	find ../lib -name "*.ftn90"       -exec cp {} $(SSMPACKAGE)/src \; ; \
	find ../lib -name "*.cdk90"       -exec cp {} $(SSMPACKAGE)/src \; ; \
	find ../lib -name "*.h"           -exec cp {} $(SSMPACKAGE)/src \; ; \
	find ../lib -name "*.hf"          -exec cp {} $(SSMPACKAGE)/src \; ; \
	echo "Package: ${BH_PACKAGE_NAMES}" > $(SSMPACKAGE)/.ssm.d/control; \
	echo "Version: ${SSM_VERSION}" >> $(SSMPACKAGE)/.ssm.d/control; \
	echo "Platform: ${ORDENV_PLAT}" >> $(SSMPACKAGE)/.ssm.d/control; \
	echo "Maintainer: $(shell whoami)" >> $(SSMPACKAGE)/.ssm.d/control; \
	echo "BuildInfo: svn co $(shell svn info | grep URL | sed -e 's/URL://' -e 's@/ssm@@' )" >> $(SSMPACKAGE)/.ssm.d/control; \
	echo "Description: ${SWDESCRIP}" >> $(SSMPACKAGE)/.ssm.d/control; \
	tar cvf - $(SSMPACKAGE) | gzip -> $(SSMPACKAGE).ssm
	ssh $(DISTINATION_MACH) mkdir -p $(DISTINATION_DIR); \
	scp $(SSMPACKAGE).ssm $(DISTINATION_MACH):$(DISTINATION_DIR); \
	scp -r $(SSMPACKAGE) $(DISTINATION_MACH):$(DISTINATION_DIR); \

clean:
	rm -rf $(SSMPACKAGE).ssm $(SSMPACKAGE) profile.sh

distclean: clean

