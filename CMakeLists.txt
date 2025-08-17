cmake_minimum_required(VERSION 3.6)

project(f90gio Fortran)
#set(CMAKE_VERBOSE_MAKEFILE on)

# for message display
if(NOT WIN32)
  string(ASCII 27 Esc)
  set(MsgStart    "${Esc}[32m=================>")
  set(MsgEnd      "<=================${Esc}[m")
endif()

option(BUILD_NC "build f90gionc" ON)
option(BUILD_H4 "build f90gioh4" OFF)
option(BUILD_H5 "build f90gioh5" ON)
option(BUILD_FAST_IO "build fast io libs" ON)

option(H5_VERSION_1_8 "H5LIB <=1.8" OFF)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "release")
endif()

set(BUILD_SHARED_LIBS OFF) # build static libraries


set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

if (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
	set(CMAKE_Fortran_FLAGS_DEBUG "-fbacktrace -O0 -g -fcheck=bounds")
	set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
    add_definitions(-cpp)
    if (CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
        #set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch -O -lm -ltirpc")
	    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
    endif()
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
        set(F90GIO_LIBS ${F90GIO_LIBS} $ENV{NC_LIBS})
        set(F90GIO_INCLUDE $ENV{NC_INCLUDE} ${F90GIO_INCLUDE})
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
        set(F90GIO_LIBS ${F90GIO_LIBS} $ENV{H4_LIBS})
        set(F90GIO_INCLUDE $ENV{H4_INCLUDE} ${F90GIO_INCLUDE})
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
        if (H5_VERSION_1_8)
            set(FPPS_FLAGS ${FPPS_FLAGS} H5_VERSION_1_8)
            message("H5_VERSION_1_8=${H5_VERSION_1_8}, update FPPS_FLAGS=${H5_VERSION_1_8}")
        else()
            message(WARNING "H5_VERSION_1_8=${H5_VERSION_1_8}, make sure your hdf5 lib has version >=1.10")
        endif()
        set(F90GIO_LIBS ${F90GIO_LIBS} $ENV{H5_LIBS})
        set(F90GIO_INCLUDE $ENV{H5_INCLUDE} ${F90GIO_INCLUDE})
	    message("${MsgStart} will build f90gioh5 ${MsgEnd}")
	    message("H5_INCLUDE= $ENV{H5_INCLUDE}")
	    message("H5_LIBS= $ENV{H5_LIBS}")
    else()
        set(HAS_H5 FALSE)
	    message("${MsgStart} BUILD_H5=ON but H5_INCLUDE, H5_LIBS not set ${MsgEnd}")
	    message("will not build f90gioh5")
    endif()
endif()

message("${MsgStart} will build f90gio ${MsgEnd}")
message("F90GIO_INCLUDE=${F90GIO_INCLUDE}")
message("F90GIO_LIBS=${F90GIO_LIBS}")

# F90GIO lib
add_subdirectory(libsrc)
# F90GIO fast I/O lib
add_subdirectory(libsrc/fast/nc)
add_subdirectory(libsrc/fast/h5)
# tests for F90GIO lib
add_subdirectory(test)
# tests for F90GIO fast I/O lib
add_subdirectory(test-fast)

# testing
enable_testing()

macro(cda_add_test TARGET CMD WORK_DIR PASS_RESULT)
    add_test(
        NAME ${TARGET} 
        COMMAND ${CMD}
        WORKING_DIRECTORY ${WORK_DIR})
    set_tests_properties(${TARGET} 
        PROPERTIES PASS_REGULAR_EXPRESSION ${PASS_RESULT})
endmacro(cda_add_test)

# test netcdf libs
if (HAS_NC)
    cda_add_test(test_nc       test_nc       ${CMAKE_SOURCE_DIR}/test "Test passed")
    cda_add_test(test_nc_g     test_nc_g     ${CMAKE_SOURCE_DIR}/test "Test passed")
    cda_add_test(test_nc_write test_nc_write ${CMAKE_SOURCE_DIR}/test "Test passed")
    cda_add_test(test_nc_nolib test_nc_nolib ${CMAKE_SOURCE_DIR}/test "Test passed")
endif()


# test h4 libs
if (HAS_H4)
    cda_add_test(test_h4       test_h4       ${CMAKE_SOURCE_DIR}/test "Test passed")
    cda_add_test(test_h4_g     test_h4_g     ${CMAKE_SOURCE_DIR}/test "Test passed")
    cda_add_test(test_h4_nolib test_h4_nolib ${CMAKE_SOURCE_DIR}/test "Test passed")
endif()

# test h5 libs
if (HAS_H5)
    cda_add_test(test_h5       test_h5       ${CMAKE_SOURCE_DIR}/test "Test passed")
    cda_add_test(test_h5_g     test_h5_g     ${CMAKE_SOURCE_DIR}/test "Test passed")
    cda_add_test(test_h5_nolib test_h5_nolib ${CMAKE_SOURCE_DIR}/test "Test passed")
endif()

# test nc Fast I/O lib
if (BUILD_FAST_IO AND HAS_NC)
    cda_add_test(test_ncio  test_ncio  ${CMAKE_SOURCE_DIR}/test-fast "Test passed")
endif()

# test hdf5 Fast I/O lib
if (BUILD_FAST_IO AND HAS_H5)
    cda_add_test(test_h5io  test_h5io  ${CMAKE_SOURCE_DIR}/test-fast "Test passed")
endif()

