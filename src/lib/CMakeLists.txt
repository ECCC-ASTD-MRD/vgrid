message(STATUS "(EC) Generating vgrid librairie")

set(PROJECT_INCLUDE_FILES vgrid.h)
set(MODULE_DIR ${CMAKE_CURRENT_BINARY_DIR}/include)
set(PROJECT_MODULE_FILES ${MODULE_DIR}/vgrid_descriptors.mod ${MODULE_DIR}/vgrid_utils.mod)
set(PROJECT_SOURCE_FILES
  vgrid_descriptors.F90
  vgrid_utils.F90
  vgrid.c
)

set(all_targets ${PROJECT_NAME}-static)
set(static_targets ${PROJECT_NAME}-static)

add_library(${PROJECT_NAME}-static STATIC ${PROJECT_SOURCE_FILES})
add_library(${PROJECT_NAME}::${PROJECT_NAME}-static ALIAS ${PROJECT_NAME}-static)
add_library(${PROJECT_NAME}::${PROJECT_NAME} ALIAS ${PROJECT_NAME}-static)  # Default to static
target_link_libraries(${PROJECT_NAME}-static rmn::rmn-static)


if (${VGRID_BUILD_SHARED})
  add_library(${PROJECT_NAME}-shared SHARED $<TARGET_OBJECTS:${PROJECT_NAME}-static>)
  add_library(${PROJECT_NAME}::${PROJECT_NAME}-shared ALIAS ${PROJECT_NAME}-shared)
  target_link_libraries(${PROJECT_NAME}-shared rmn::rmn-shared)
  set(all_targets ${all_targets} ${PROJECT_NAME}-shared)
  set(shared_targets ${PROJECT_NAME}-shared)
endif()

if (TARGET rmn::rmn-ompi)
  add_library(${PROJECT_NAME}-ompi-static STATIC ${PROJECT_SOURCE_FILES})
  add_library(${PROJECT_NAME}::${PROJECT_NAME}-ompi-static ALIAS ${PROJECT_NAME}-ompi-static)
  target_link_libraries(${PROJECT_NAME}-ompi-static rmn::rmn-ompi-static)
  set(all_targets ${all_targets} ${PROJECT_NAME}-ompi-static)
  set(static_targets ${static_targets} ${PROJECT_NAME}-ompi-static)
  set(ompi_targets ${PROJECT_NAME}-ompi-static)

  if (${VGRID_BUILD_SHARED})
    add_library(${PROJECT_NAME}-ompi-shared SHARED $<TARGET_OBJECTS:${PROJECT_NAME}-ompi-static>)
    add_library(${PROJECT_NAME}::${PROJECT_NAME}-ompi-shared ALIAS ${PROJECT_NAME}-ompi-shared)
    target_link_libraries(${PROJECT_NAME}-ompi-shared rmn::rmn-ompi-shared)
    set(all_targets ${all_targets} ${PROJECT_NAME}-ompi-shared)
    set(ompi_targets ${ompi_targets} ${PROJECT_NAME}-ompi-shared)
    set(shared_targets ${shared_targets} ${PROJECT_NAME}-ompi-shared)
  endif()
endif()

foreach(TARGET IN ITEMS ${all_targets})
  add_dependencies(${TARGET} ${PROJECT_NAME}_build_info)
  set_target_properties(${TARGET} PROPERTIES
    VERSION ${PROJECT_VERSION}
    PUBLIC_HEADER "${PROJECT_INCLUDE_FILES}"
    Fortran_MODULE_DIRECTORY ${MODULE_DIR}
    INTERFACE_COMPILE_DEFINITIONS HAVE_VGRID="${PROJECT_VERSION}"
    INTERFACE_INCLUDE_DIRECTORIES $<BUILD_INTERFACE:${MODULE_DIR}>
    POSITION_INDEPENDENT_CODE ON
    OUTPUT_NAME vgrid)
endforeach()

foreach(OMPI_TARGET IN ITEMS ${ompi_targets})
  set_property(TARGET ${OMPI_TARGET} PROPERTY OUTPUT_NAME vgrid-ompi)
endforeach()

install(TARGETS ${static_targets}
  EXPORT ${PROJECT_NAME}-static-targets
  INCLUDES      DESTINATION include
  PUBLIC_HEADER DESTINATION include
  LIBRARY       DESTINATION lib
  ARCHIVE       DESTINATION lib)

if (${VGRID_BUILD_SHARED})
  install(TARGETS ${shared_targets}
    EXPORT ${PROJECT_NAME}-shared-targets
    INCLUDES      DESTINATION include
    PUBLIC_HEADER DESTINATION include
    LIBRARY       DESTINATION lib
    ARCHIVE       DESTINATION lib)
endif()

install(FILES ${PROJECT_MODULE_FILES} DESTINATION include)

# Symbolic link for archive numbered version
add_custom_command(TARGET ${PROJECT_NAME}-static POST_BUILD
    COMMAND ln -sfv lib${PROJECT_NAME}.a lib${PROJECT_NAME}_${PROJECT_VERSION}.a)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${PROJECT_NAME}_${PROJECT_VERSION}.a DESTINATION lib)

if (TARGET ${PROJECT_NAME}-ompi-static)
  add_custom_command(TARGET ${PROJECT_NAME}-ompi-static POST_BUILD
      COMMAND ln -sfv lib${PROJECT_NAME}-ompi.a lib${PROJECT_NAME}-ompi_${PROJECT_VERSION}.a)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${PROJECT_NAME}-ompi_${PROJECT_VERSION}.a DESTINATION lib)
endif()
