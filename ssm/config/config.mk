# this file contains common config that is sourced by all makefiles
# Warning : no underscore '_' in CURRENT_VERSION string
VERSION=5.0.1
SSM_VERSION=$(VERSION)-$(COMP_ARCH)
SWNAME=vgriddescriptors
SWDESCRIP='Vertical grid descriptors package'

SSMPACKAGE=$(SWNAME)_$(SSM_VERSION)_${PLATFORM}
