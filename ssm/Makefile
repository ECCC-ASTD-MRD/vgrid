include config/config.mk

ssm: profile.sh
	echo "Creating SSM package $(SSMPACKAGE)..."
	mkdir -p $(SSMPACKAGE)/.ssm.d; \
	cp profile.sh $(SSMPACKAGE)/.ssm.d; \
	mkdir -p $(SSMPACKAGE)/lib; \
	mkdir -p $(SSMPACKAGE)/include; \
	echo   "print*,'  *          VGRID ${BH_PACKAGE_VERSION} ${COMP_ARCH} ${ORDENV_PLAT} $$(date)'" > $(SSMPACKAGE)/include/vgrid_version.cdk
	echo   "print*,'  ********************************************************************************************'" >> $(SSMPACKAGE)/include/vgrid_version.cdk
	find ../lib -name "*.mod" -type f -exec cp {} $(SSMPACKAGE)/include \; ; \
	find ../lib -name "lib*.a" -exec cp {} $(SSMPACKAGE)/lib \; ; \
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

profile.sh: profile
	echo "__version=${SSM_VERSION}" >$@ ;\
	echo "__swname=${BH_PACKAGE_NAMES}" >>$@ ;\
	cat profile >>$@

clean:
	rm -rf $(SSMPACKAGE).ssm $(SSMPACKAGE) profile.sh

distclean: clean

