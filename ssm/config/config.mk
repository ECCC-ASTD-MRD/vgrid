# this file contains common config that is sourced by all makefiles
WHO_I_AM=$(shell whoami)
VERSION=4.2.0
MACHINE=$(shell uname -s)
ARCH=$(MACHINE)
SWNAME=vgriddescriptors
SWDESCRIP='Vertical grid descriptors package'

# platform specific definition
SSMPACKAGE=$(SWNAME)_$(VERSION)_multi
