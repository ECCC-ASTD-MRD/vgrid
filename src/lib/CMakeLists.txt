message(STATUS "(EC) Generating vgrid librairie")

set(PROJECT_INCLUDE_FILES vgrid.h)
set(PROJECT_MODULE_FILES ${CMAKE_CURRENT_BINARY_DIR}/vgrid_descriptors.mod ${CMAKE_CURRENT_BINARY_DIR}/vgrid_utils.mod)

add_library(vgrid STATIC vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
add_dependencies(vgrid ${PROJECT_NAME}_build_info)
set_target_properties(vgrid PROPERTIES
  VERSION ${PROJECT_VERSION}
  PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
  POSITION_INDEPENDENT_CODE ON
  OUTPUT_NAME vgrid)

set(PROJECT_MODULE_FILES ${CMAKE_CURRENT_BINARY_DIR}/vgrid_descriptors.mod ${CMAKE_CURRENT_BINARY_DIR}/vgrid_utils.mod)
add_library(vgridshared SHARED vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
set_target_properties(vgridshared PROPERTIES
  VERSION ${PROJECT_VERSION}
  PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
  POSITION_INDEPENDENT_CODE ON
  OUTPUT_NAME vgrid)
 
target_link_libraries(vgrid rmn::rmn)
target_link_libraries(vgridshared rmn::rmn)
if (tdpack_FOUND)
   target_link_libraries(vgrid tdpack::tdpack)
   target_link_libraries(vgridshared tdpack::tdpack)
endif()

install(TARGETS vgrid vgridshared
  PUBLIC_HEADER DESTINATION include
  LIBRARY       DESTINATION lib
  ARCHIVE       DESTINATION lib)
install(FILES ${PROJECT_MODULE_FILES} DESTINATION include)
