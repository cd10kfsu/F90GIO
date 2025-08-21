if (BUILD_FAST_IO AND HAS_NC)

    set(EXE test_ncio)
    set(TESTSRC test_ncio.f90)
    set(TESTLIBS ncio $ENV{NC_LIBS})
    if (HAS_NC_C)
        set(TESTLIBS ${TESTLIBS} $ENV{NC_C_LIBS})
    endif()
    add_executable(${EXE} ${TESTSRC})
#target_include_directories(${EXE} PRIVATE "$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>")
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE}
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-fast)

    set(EXE test_ncio_group)
    set(TESTSRC test_ncio_group.f90)
    set(TESTLIBS ncio $ENV{NC_LIBS})
    if (HAS_NC_C)
        set(TESTLIBS ${TESTLIBS} $ENV{NC_C_LIBS})
    endif()
    add_executable(${EXE} ${TESTSRC})
    target_compile_definitions(ncio PUBLIC ${FPPS_FLAGS})
#target_include_directories(${EXE} PRIVATE "$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>")
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE}
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-fast)
 
endif()

if (BUILD_FAST_IO AND HAS_H5)

    set(EXE test_h5io)
    set(TESTSRC test_h5io.f90)
    set(TESTLIBS h5io $ENV{H5_LIBS})
    add_executable(${EXE} ${TESTSRC})
#target_include_directories(${EXE} PUBLIC "$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>")
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE}
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-fast)

endif()
