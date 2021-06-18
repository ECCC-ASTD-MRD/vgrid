if(BUILD_TESTING)

   #----- Library tests
   message(STATUS "(EC) Generating vgrid library and utilities tests")

   file(REMOVE_RECURSE ${CMAKE_BINARY_DIR}/Testing/data_out)
   execute_process(COMMAND ln -s /home/sidr000/ords/vgrid_validation_data/b_6.5/data ${CMAKE_BINARY_DIR}/Testing/data)
   execute_process(COMMAND mkdir ${CMAKE_BINARY_DIR}/Testing/data_out)

   file(GLOB SRC_TEST lib/src/*)

   #----- Loop on tests
   foreach(testfile ${SRC_TEST})
      get_filename_component(ext  ${testfile} EXT)
      get_filename_component(test ${testfile} NAME)
      get_filename_component(test ${test}     NAME_WLE)
      message(STATUS "(EC) Adding ${ext} test ${testfile}")
   
      if(${ext} MATCHES ".c")
         add_executable(${test} ${testfile} lib/c_ut_report.c)
      else()
         add_executable(${test} ${testfile} lib/unit_testing.F90)
      endif()

      target_link_libraries(${test} PRIVATE vgrid)
      target_include_directories(${test} PRIVATE ${CMAKE_BINARY_DIR}/lib ${CMAKE_SOURCE_DIR}/src/lib ${CMAKE_CURRENT_SOURCE_DIR} ${RMN_INCLUDE_DIRS})

      add_test(NAME ${test} COMMAND ${test} WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Testing)
   endforeach()

   #----- Utilities tests
   find_program(BASH_COMMAND bash)
   add_test(NAME testutils COMMAND ${BASH_COMMAND} ${CMAKE_SOURCE_DIR}/test/utils/testutilvgrid.sh ${CMAKE_BINARY_DIR}/utils /home/sidr000/ords/vgrid_validation_data/1.9.b3 WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Testing)
endif()