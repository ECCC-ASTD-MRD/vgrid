cmake_minimum_required(VERSION 3.14)


list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/cmake_rpn)
include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git state
project(vgrid C Fortran)

find_package(RMN)
# f OBJECTS·=·vgrid_utils.o·vgrid.o·vgrid_descriptors.o
# genlib: $(OBJECTS)
# 	rm -f $(MYLIB)
# 	ar rcv $(MYLIB) $(OBJECTS)
# 	echo 'character(len=128) :: vgrid_descriptors_version = $(VGD_VERSION)' > vgrid_version.hf
# 	echo 'char vgrid_descriptors_version[] = $(VGD_VERSION)' > vgrid_version.hc

add_library(genlib STATIC vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
target_link_libraries(genlib PRIVATE ${RMN_LIBRARIES})

add_executable(compute_pressure src_utils/compute_pressure.F90)
target_link_libraries(compute_pressure PRIVATE genlib)
target_include_directories(compute_pressure PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})

add_executable(convert_toctoc_5002 src_utils/convert_toctoc_5002.F90)
target_link_libraries(convert_toctoc_5002 PRIVATE genlib)
target_include_directories(convert_toctoc_5002 PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})

add_executable(add_toctoc src_utils/add_toctoc.F90)
target_link_libraries(add_toctoc PRIVATE genlib)
target_include_directories(add_toctoc PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})

add_executable(pring_toctoc src_utils/print_toctoc.F90)
target_link_libraries(pring_toctoc PRIVATE genlib)
target_include_directories(pring_toctoc PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})

add_executable(vcode src_utils/vcode.F90)
target_link_libraries(vcode PRIVATE genlib)
target_include_directories(vcode PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})

add_executable(vgrid_sample src_utils/vgrid_sample.F90)
target_link_libraries(vgrid_sample PRIVATE genlib)
target_include_directories(vgrid_sample PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
#vgrid_version.hf







