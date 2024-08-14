message(STATUS "(EC) Generating vgrid librairie")

set(PROJECT_INCLUDE_FILES vgrid.h)
set(MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)
set(PROJECT_MODULE_FILES ${MODULE_DIR}/vgrid_descriptors.mod ${MODULE_DIR}/vgrid_utils.mod)

add_library(${PROJECT_NAME}-static STATIC vgrid_descriptors.F90 vgrid_utils.F90 vgrid.c)
add_dependencies(${PROJECT_NAME}-static ${PROJECT_NAME}_build_info)
if (${VGRID_BUILD_SHARED})
  add_library(${PROJECT_NAME}-shared SHARED $<TARGET_OBJECTS:${PROJECT_NAME}-static>)
  add_dependencies(${PROJECT_NAME}-shared ${PROJECT_NAME}_build_info)
endif()

add_dependencies(${PROJECT_NAME}-static ${PROJECT_NAME}_build_info)

if (${VGRID_BUILD_SHARED})
set_target_properties(${PROJECT_NAME}-shared PROPERTIES
  VERSION ${PROJECT_VERSION}
  PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
  Fortran_MODULE_DIRECTORY ${MODULE_DIR}
  INTERFACE_COMPILE_DEFINITIONS HAVE_VGRID="${PROJECT_VERSION}"
  INTERFACE_INCLUDE_DIRECTORIES $<BUILD_INTERFACE:${MODULE_DIR}>
  POSITION_INDEPENDENT_CODE ON
  OUTPUT_NAME vgrid)
endif()

set_target_properties(${PROJECT_NAME}-static PROPERTIES
  VERSION ${PROJECT_VERSION}
  PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
  Fortran_MODULE_DIRECTORY ${MODULE_DIR}
  INTERFACE_COMPILE_DEFINITIONS HAVE_VGRID="${PROJECT_VERSION}"
  INTERFACE_INCLUDE_DIRECTORIES $<BUILD_INTERFACE:${MODULE_DIR}>
  POSITION_INDEPENDENT_CODE ON
  OUTPUT_NAME vgrid)
 
target_link_libraries(${PROJECT_NAME}-static rmn::rmn-static)
if (${VGRID_BUILD_SHARED})
  target_link_libraries(${PROJECT_NAME}-shared rmn::rmn-shared)
endif()

install(TARGETS ${PROJECT_NAME}-static
  EXPORT ${PROJECT_NAME}-static-targets
  INCLUDES      DESTINATION include
  PUBLIC_HEADER DESTINATION include
  LIBRARY       DESTINATION lib
  ARCHIVE       DESTINATION lib)

if (${VGRID_BUILD_SHARED})
  install(TARGETS ${PROJECT_NAME}-shared
    EXPORT ${PROJECT_NAME}-shared-targets
    INCLUDES      DESTINATION include
    PUBLIC_HEADER DESTINATION include
    LIBRARY       DESTINATION lib
    ARCHIVE       DESTINATION lib)
endif()

install(FILES ${PROJECT_MODULE_FILES} DESTINATION include)

add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME}-static)

# Symbolic link for archive numbered version
add_custom_command(TARGET ${PROJECT_NAME}-static POST_BUILD
    COMMAND ln -sfv lib${PROJECT_NAME}.a lib${PROJECT_NAME}_${PROJECT_VERSION}.a)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${PROJECT_NAME}_${PROJECT_VERSION}.a DESTINATION lib)
