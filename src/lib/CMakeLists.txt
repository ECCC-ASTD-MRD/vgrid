message(STATUS "(EC) Generating vgrid librairie")

set(PROJECT_INCLUDE_FILES vgrid.h)
set(PROJECT_MODULE_FILES ${CMAKE_BINARY_DIR}/lib/vgrid_descriptors.mod ${CMAKE_BINARY_DIR}/lib/vgrid_utils.mod)

add_library(vgrid STATIC vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
add_dependencies(vgrid build_info)
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
 
install(TARGETS vgrid vgridshared)
install(FILES ${PROJECT_MODULE_FILES} TYPE INCLUDE)