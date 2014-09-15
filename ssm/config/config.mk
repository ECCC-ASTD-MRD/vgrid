# this file contains common config that is sourced by all makefiles
# Warning : no underscore '_' in CURRENT_VERSION string
SSM_VERSION=$(BH_PACKAGE_VERSION)-$(COMP_ARCH)
SWDESCRIP='Vertical grid descriptors package'
SSMPACKAGE=$(BH_PACKAGE_NAMES)_$(SSM_VERSION)_$(BH_PACKAGE_PLATFORM)

