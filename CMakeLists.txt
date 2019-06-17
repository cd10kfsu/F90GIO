cmake_minimum_required(VERSION 3.1)

#set(CMAKE_Fortran_COMPILER ifort)
project(f90gio Fortran)

# for message display
if(NOT WIN32)
  string(ASCII 27 Esc)
  set(MsgStart    "${Esc}[32m=================>")
  set(MsgEnd      "<=================${Esc}[m")
endif()


set(CMAKE_VERBOSE_MAKEFILE off)

option(BUILD_NC "build f90gionc" ON)
option(BUILD_H4 "build f90gioh4" OFF)
option(BUILD_H5 "build f90gioh5" OFF)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "release")
endif()


set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

if (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
	set(CMAKE_Fortran_FLAGS_DEBUG "-fbacktrace -O0")
	set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
    add_definitions(-cpp)
elseif (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
	set(CMAKE_Fortran_FLAGS_DEBUG "-traceback -check bounds -O0")
	set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -fp-model precise")
    add_definitions(-fpp)
else()
    message(FATAL_ERROR "unsupported compiler, Add compiling flags in CMakeLists.txt first")
endif()

# check if netcdf vars is set
if (BUILD_NC) 
    if (DEFINED ENV{NC_INCLUDE} AND DEFINED ENV{NC_LIBS} )
        set(HAS_NC TRUE)
        set(FPPS_FLAGS ${FPPS_FLAGS} HAS_NC)
	    message("${MsgStart} will build f90gionc ${MsgEnd}")
	    message("NC_INCLUDE= $ENV{NC_INCLUDE}")
	    message("NC_LIBS= $ENV{NC_LIBS}")
    else()
        set(HAS_NC FALSE)
	    message("${MsgStart} BUILD_NC=ON but NC_INCLUDE, NC_LIBS not set ${MsgEnd}")
	    message("will not build f90gionc")
    endif()
endif()

# check if hdf4 var is set
if (BUILD_H4)
    if (DEFINED ENV{H4_INCLUDE} AND DEFINED ENV{H4_LIBS} )
        set(HAS_H4 TRUE)
        set(FPPS_FLAGS ${FPPS_FLAGS} HAS_H4)
	    message("${MsgStart} will build f90gioh4 ${MsgEnd}")
	    message("H4_INCLUDE= $ENV{H4_INCLUDE}")
	    message("H4_LIBS= $ENV{H4_LIBS}")
    else()
        set(HAS_H4 FALSE)
	    message("${MsgStart} BUILD_H4=ON but H4_INCLUDE, H4_LIBS not set ${MsgEnd}")
	    message("will not build f90gioh4")
    endif()
endif()

# check if hdf5 var is set
if (BUILD_H5)
    if (DEFINED ENV{H5_INCLUDE} AND DEFINED ENV{H5_LIBS} )
        set(HAS_H5 TRUE)
        set(FPPS_FLAGS ${FPPS_FLAGS} HAS_H5)
	    message("${MsgStart} will build f90gioh5 ${MsgEnd}")
	    message("H5_INCLUDE= $ENV{H5_INCLUDE}")
	    message("H5_LIBS= $ENV{H5_LIBS}")
    else()
        set(HAS_H5 FALSE)
	    message("${MsgStart} BUILD_H5=ON but H5_INCLUDE, H5_LIBS not set ${MsgEnd}")
	    message("will not build f90gioh5")
    endif()
endif()


# build f90gionc
if (HAS_NC) 
	include_directories($ENV{NC_INCLUDE})
	add_library(f90gionc
	libsrc/mod_f90gionc.f90)

    set(F90GIO_SRCS ${F90GIO_SRCS} libsrc/mod_f90gionc.f90)
    set(F90GIO_LIBS ${F90GIO_LIBS} $ENV{NC_LIBS})
endif()

# build f90gioh4
if (HAS_H4)
    include_directories($ENV{H4_INCLUDE})
    add_library(f90gioh4
    libsrc/mod_f90gioh4.f90)

    set(F90GIO_SRCS ${F90GIO_SRCS} libsrc/mod_f90gioh4.f90)
    set(F90GIO_LIBS ${F90GIO_LIBS} $ENV{H4_LIBS})
endif()

# build f90gioh5
if (HAS_H5)
	include_directories($ENV{H5_INCLUDE})
    add_library(f90gioh5
	libsrc/mod_f90gioh5.f90)

    set(F90GIO_SRCS ${F90GIO_SRCS} libsrc/mod_f90gioh5.f90)
    set(F90GIO_LIBS ${F90GIO_LIBS} $ENV{H5_LIBS})
endif()

# build f90gio
add_library(f90gio ${F90GIO_SRCS} libsrc/mod_f90gio.f90)
target_compile_definitions(f90gio PRIVATE ${FPPS_FLAGS})
#add_compile_definitions(${FPPS_FLAGS})


# testing
enable_testing()

# test netcdf libs
if (HAS_NC)

    set(EXE test_nc)
    set(TESTSRC test/test_nc.f90)
    set(TESTLIBS f90gionc $ENV{NC_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")


    set(EXE test_nc_g)
    set(TESTSRC test/test_nc_g.f90)
    set(TESTLIBS f90gio ${F90GIO_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")


    set(EXE test_nc_write)
    set(TESTSRC test/test_nc_write.f90)
    set(TESTLIBS f90gionc $ENV{NC_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
       PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")


    set(EXE test_nc_nolib)
    set(TESTSRC libsrc/mod_f90gionc.f90 test/test_nc.f90)
    set(TESTLIBS $ENV{NC_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
       PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-nolib)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")

endif()


# test h4 libs
if (HAS_H4)

    set(EXE test_h4)
    set(TESTSRC test/test_h4.f90)
    set(TESTLIBS f90gioh4 $ENV{H4_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")


    set(EXE test_h4_g)
    set(TESTSRC test/test_h4_g.f90)
    set(TESTLIBS f90gio ${F90GIO_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")


    set(EXE test_h4_nolib)
    set(TESTSRC libsrc/mod_f90gioh4.f90 test/test_h4.f90)
    set(TESTLIBS $ENV{H4_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-nolib)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")

endif()


# test h5 libs
if (HAS_H5)

    set(EXE test_h5)
    set(TESTSRC test/test_h5.f90)
    set(TESTLIBS f90gioh5 $ENV{H5_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
    PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")


    set(EXE test_h5_g)
    set(TESTSRC test/test_h5_g.f90)
    set(TESTLIBS f90gio ${F90GIO_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_include_directories(${EXE} PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>)
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")


    set(EXE test_h5_nolib)
    set(TESTSRC libsrc/mod_f90gioh5.f90 test/test_h5.f90)
    set(TESTLIBS $ENV{H5_LIBS})
    add_executable(${EXE} ${TESTSRC})
    target_link_libraries(${EXE} ${TESTLIBS})
    set_target_properties(${EXE} 
        PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test-nolib)
    add_test(
        NAME ${EXE} 
        COMMAND ${EXE}
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test)
    set_tests_properties(${EXE} 
        PROPERTIES PASS_REGULAR_EXPRESSION "Test passed")

endif()

