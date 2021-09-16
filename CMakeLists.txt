cmake_minimum_required(VERSION 3.14)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake_rpn)
include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git state
ec_parse_manifest()        # Parse MANIFEST file

project(${NAME} DESCRIPTION "${DESCRIPTION}")
set(PROJECT_VERSION ${VERSION})

ec_build_info()            # Generate build information
ec_install_prefix()        # Define install prefix  

include(CTest)
add_custom_target(check COMMAND CTEST_OUTPUT_ON_FAILURE=true ${CMAKE_CTEST_COMMAND})

enable_language(C Fortran)

include(ec_doxygen)          # Doxygen target (-DWITH_DOC=TRUE)
include(ec_compiler_presets)
include(ec_openmp)           # Enable OpenMP (-DWITH_OPENMP=TRUE)

find_package(RMN ${RMN_REQ_VERSION})
if (RMN_FOUND) 
   add_compile_definitions(HAVE_RMN)
   include_directories(${RMN_INCLUDE_DIR})
endif()

add_subdirectory(src/lib lib)
add_subdirectory(src/utils utils)
#add_subdirectory(src/examples examples)
add_subdirectory(test test)

ec_build_config()      # Create build configuration script
ec_prepare_ssm()       # Prepare ssm packaging files

set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGE_VENDOR "ECCC")
set(CPACK_PACKAGE_CONTACT "${MAINTAINER}")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE.txt")
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.org")
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_CURRENT_BINARY_DIR}/package")
set(CPACK_PACKAGE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
set(CPACK_PACKAGE_FILE_NAME "${CMAKE_INSTALL_PREFIX}")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "${CMAKE_INSTALL_PREFIX}")
include(CPack)
