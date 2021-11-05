message(STATUS "(EC) Generating vgrid librairie")

set(PROJECT_INCLUDE_FILES vgrid.h)
set(PROJECT_MODULE_FILES ${CMAKE_CURRENT_BINARY_DIR}/vgrid_descriptors.mod ${CMAKE_CURRENT_BINARY_DIR}/vgrid_utils.mod)

add_library(vgrid STATIC vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
add_dependencies(vgrid ${PROJECT_NAME}_build_info)
set_target_properties(vgrid PROPERTIES
  VERSION ${PROJECT_VERSION}
  PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
  POSITION_INDEPENDENT_CODE ON)

target_link_libraries(vgrid RMN::RMN)
if (tdpack_FOUND)
   target_link_libraries(vgrid tdpack::tdpack)
endif()

set(PROJECT_MODULE_FILES ${CMAKE_CURRENT_BINARY_DIR}/vgrid_descriptors.mod ${CMAKE_CURRENT_BINARY_DIR}/vgrid_utils.mod)
add_library(vgridshared SHARED vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
set_target_properties(vgridshared PROPERTIES
  VERSION ${PROJECT_VERSION}
  PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
  POSITION_INDEPENDENT_CODE ON)
 
install(TARGETS vgrid vgridshared
   PUBLIC_HEADER DESTINATION include/$ENV{EC_ARCH}
   LIBRARY       DESTINATION lib/$ENV{EC_ARCH}
   ARCHIVE       DESTINATION lib/$ENV{EC_ARCH})
install(FILES ${PROJECT_MODULE_FILES} DESTINATION include/$ENV{EC_ARCH})

target_link_libraries(vgrid RMN::RMN)
if (tdpack_FOUND)
   target_link_libraries(vgrid tdpack::tdpack)
endif()
