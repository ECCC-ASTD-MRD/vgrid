cmake_minimum_required(VERSION 3.14)


list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/cmake_rpn)
include(ec_init)           # Initialise compilers and ec specific functions
ec_git_version()           # Get version from git state

project(vgrid C Fortran)
set(PROJECT_VERSION ${GIT_VERSION} CACHE STRING "Define the version of VGRID that will be baked into the various artifacts of this build")
set(PROJECT_INCLUDE_FILES vgrid.h vgrid_version.hc vgrid_version.hf)
set(PROJECT_MODULE_FILES ${CMAKE_BINARY_DIR}/vgrid_descriptors.mod ${CMAKE_BINARY_DIR}/vgrid_utils.mod)

if("${CMAKE_INSTALL_PREFIX}" STREQUAL "/usr/local")
  set(CMAKE_INSTALL_PREFIX ${CMAKE_BINARY_DIR}/install-site CACHE FILEPATH "CMake Installation prefix for ${PROJECT_NAME}" FORCE)
  message(STATUS "Setting CMAKE_INSTALL_PREFIX to ${CMAKE_INSTALL_PREFIX}")
endif()

include(ec_compiler_presets)

find_package(RMN)

add_library(vgrid STATIC vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
target_link_libraries(vgrid PRIVATE ${RMN_LIBRARIES})
set_target_properties(vgrid PROPERTIES
  VERSION ${PROJECT_VERSION}
  PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
  POSITION_INDEPENDENT_CODE ON)

add_library(vgridshared SHARED vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
target_link_libraries(vgridshared PRIVATE ${RMN_LIBRARIES})
set_target_properties(vgridshared PROPERTIES
  VERSION ${PROJECT_VERSION}
  PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
  POSITION_INDEPENDENT_CODE ON)

add_executable(r.compute_pressure src_utils/compute_pressure.F90)
target_link_libraries(r.compute_pressure PRIVATE vgrid)
target_include_directories(r.compute_pressure PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.compute_pressure PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.convert_toctoc_5002 src_utils/convert_toctoc_5002.F90)
target_link_libraries(r.convert_toctoc_5002 PRIVATE vgrid)
target_include_directories(r.convert_toctoc_5002 PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.convert_toctoc_5002 PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.add_toctoc src_utils/add_toctoc.F90)
target_link_libraries(r.add_toctoc PRIVATE vgrid)
target_include_directories(r.add_toctoc PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.add_toctoc PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.print_toctoc src_utils/print_toctoc.F90)
target_link_libraries(r.print_toctoc PRIVATE vgrid)
target_include_directories(r.print_toctoc PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.print_toctoc PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.vcode src_utils/vcode.F90)
target_link_libraries(r.vcode PRIVATE vgrid)
target_include_directories(r.vcode PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.vcode PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.vgrid_sample src_utils/vgrid_sample.F90)
target_link_libraries(r.vgrid_sample PRIVATE vgrid)
target_include_directories(r.vgrid_sample PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.vgrid_sample PROPERTIES VERSION ${PROJECT_VERSION})

install(TARGETS vgrid vgridshared r.compute_pressure r.convert_toctoc_5002 r.add_toctoc r.print_toctoc r.vcode r.vgrid_sample)
install(FILES ${PROJECT_MODULE_FILES} TYPE INCLUDE)

configure_file(config.in ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config @ONLY)
install(PROGRAMS ${CMAKE_BINARY_DIR}/${PROJECT_NAME}-config DESTINATION bin)
