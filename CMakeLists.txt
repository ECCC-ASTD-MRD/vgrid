cmake_minimum_required(VERSION 3.14)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/cmake_rpn)
include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git state

project(vgrid)
set(PROJECT_VERSION ${GIT_VERSION} CACHE STRING "Define the version of VGRID that will be baked into the various artifacts of this build")

if("${CMAKE_INSTALL_PREFIX}" STREQUAL "/usr/local")
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/install-site CACHE FILEPATH "CMake Installation prefix for ${PROJECT_NAME}" FORCE)
  message(STATUS "Setting CMAKE_INSTALL_PREFIX to ${CMAKE_INSTALL_PREFIX}")
endif()

include(CTest)
enable_testing() 

enable_language(C Fortran)

include(ec_compiler_presets)

find_package(RMN)
if (RMN_FOUND) 
   add_compile_definitions(HAVE_RMN)
   include_directories(${RMN_INCLUDE_DIR})
endif()

add_subdirectory(src/lib lib)
add_subdirectory(src/utils utils)
add_subdirectory(src/examples examples)
add_subdirectory(test test)

configure_file(config.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config @ONLY)
install(PROGRAMS ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config DESTINATION bin)





