message(STATUS "(EC) Generating targets for vgrid utilities")

set_source_files_properties(compute_pressure.F90 convert_toctoc_5002.F90 add_toctoc.F90 print_toctoc.F90 vcode.F90  vgrid_sample.F90
PROPERTIES Fortran_PREPROCESS ON)

add_executable(r.compute_pressure compute_pressure.F90)
add_dependencies(r.compute_pressure ${PROJECT_NAME}_build_info)
target_link_libraries(r.compute_pressure PRIVATE vgrid)
target_include_directories(r.compute_pressure PRIVATE ${CMAKE_BINARY_DIR}/lib ${CMAKE_SOURCE_DIR}/src/lib ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.compute_pressure PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.convert_toctoc_5002 convert_toctoc_5002.F90)
add_dependencies(r.convert_toctoc_5002 ${PROJECT_NAME}_build_info)
target_link_libraries(r.convert_toctoc_5002 PRIVATE vgrid)
target_include_directories(r.convert_toctoc_5002 PRIVATE ${CMAKE_BINARY_DIR}/lib ${CMAKE_SOURCE_DIR}/src/lib ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.convert_toctoc_5002 PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.add_toctoc add_toctoc.F90)
add_dependencies(r.add_toctoc ${PROJECT_NAME}_build_info)
target_link_libraries(r.add_toctoc PRIVATE vgrid)
target_include_directories(r.add_toctoc PRIVATE ${CMAKE_BINARY_DIR}/lib ${CMAKE_SOURCE_DIR}/src/lib ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.add_toctoc PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.print_toctoc print_toctoc.F90)
add_dependencies(r.print_toctoc ${PROJECT_NAME}_build_info)
target_link_libraries(r.print_toctoc PRIVATE vgrid)
target_include_directories(r.print_toctoc PRIVATE ${CMAKE_BINARY_DIR}/lib ${CMAKE_SOURCE_DIR}/src/lib ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.print_toctoc PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.vcode vcode.F90)
add_dependencies(r.vcode ${PROJECT_NAME}_build_info)
target_link_libraries(r.vcode PRIVATE vgrid)
target_include_directories(r.vcode PRIVATE ${CMAKE_BINARY_DIR}/lib ${CMAKE_SOURCE_DIR}/src/lib ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.vcode PROPERTIES VERSION ${PROJECT_VERSION})

add_executable(r.vgrid_sample vgrid_sample.F90)
add_dependencies(r.vgrid_sample ${PROJECT_NAME}_build_info)
target_link_libraries(r.vgrid_sample PRIVATE vgrid)
target_include_directories(r.vgrid_sample PRIVATE ${CMAKE_BINARY_DIR}/lib ${CMAKE_SOURCE_DIR}/src/lib ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})
set_target_properties(r.vgrid_sample PROPERTIES VERSION ${PROJECT_VERSION})

install(TARGETS r.compute_pressure r.convert_toctoc_5002 r.add_toctoc r.print_toctoc r.vcode r.vgrid_sample)
