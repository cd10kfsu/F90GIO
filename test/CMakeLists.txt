if (HAS_NC)

    set(EXE test_nc)
    set(TESTSRC test_nc.f90)
    set(TESTLIBS f90gionc $ENV{NC_LIBS})
    add_executable(${EXE} ${TESTSRC})
#target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)

    set(EXE test_nc_g)
    set(TESTSRC test_nc_g.f90)
    set(TESTLIBS f90gio ${F90GIO_LIBS})
    add_executable(${EXE} ${TESTSRC})
#target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)


    set(EXE test_nc_write)
    set(TESTSRC test_nc_write.f90)
    set(TESTLIBS f90gionc $ENV{NC_LIBS})
    add_executable(${EXE} ${TESTSRC})
#target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
       PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)


    set(EXE test_nc_nolib)
    set(TESTSRC ../libsrc/mod_f90gionc.f90 test_nc.f90)
    set(TESTLIBS $ENV{NC_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PRIVATE "$<BUILD_INTERFACE:$ENV{NC_INCLUDE}>")
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
       PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-nolib)

endif()


# test h4 libs
if (HAS_H4)

    set(EXE test_h4)
    set(TESTSRC test_h4.f90)
    set(TESTLIBS f90gioh4 $ENV{H4_LIBS})
    add_executable(${EXE} ${TESTSRC})
#target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)

    set(EXE test_h4_g)
    set(TESTSRC test_h4_g.f90)
    set(TESTLIBS f90gio ${F90GIO_LIBS})
    add_executable(${EXE} ${TESTSRC})
#target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)

    set(EXE test_h4_nolib)
    set(TESTSRC ../libsrc/mod_f90gioh4.f90 test_h4.f90)
    set(TESTLIBS $ENV{H4_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PRIVATE "$<BUILD_INTERFACE:$ENV{H4_INCLUDE}>")
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-nolib)

endif()


# test h5 libs
if (HAS_H5)

    set(EXE test_h5)
    set(TESTSRC test_h5.f90)
    set(TESTLIBS f90gioh5 $ENV{H5_LIBS})
    add_executable(${EXE} ${TESTSRC})
#target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
    PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)

    set(EXE test_h5_g)
    set(TESTSRC test_h5_g.f90)
    set(TESTLIBS f90gio ${F90GIO_LIBS})
    add_executable(${EXE} ${TESTSRC})
#target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)

    set(EXE test_h5_nolib)
    set(TESTSRC ../libsrc/mod_f90gioh5.f90 test_h5.f90)
    set(TESTLIBS $ENV{H5_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PRIVATE "$<BUILD_INTERFACE:$ENV{H5_INCLUDE}>")
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-nolib)

endif()

