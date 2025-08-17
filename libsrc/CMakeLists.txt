# build f90gionc
if (HAS_NC) 
#include_directories($ENV{NC_INCLUDE})
	add_library(f90gionc mod_f90gionc.f90)
    target_include_directories(f90gionc PUBLIC "$<BUILD_INTERFACE:$ENV{NC_INCLUDE}>")

    set(F90GIO_SRCS mod_f90gionc.f90 ${F90GIO_SRCS})
    set(LIBS_TO_INSTALL f90gionc ${LIBS_TO_INSTALL})
#set(F90GIO_LIBS ${F90GIO_LIBS} $ENV{NC_LIBS})

endif()

# build f90gioh4
if (HAS_H4)
#include_directories($ENV{H4_INCLUDE})
    add_library(f90gioh4 mod_f90gioh4.f90)
    target_include_directories(f90gioh4 PUBLIC "$<BUILD_INTERFACE:$ENV{H4_INCLUDE}>")

    set(F90GIO_SRCS mod_f90gioh4.f90 ${F90GIO_SRCS})
    set(LIBS_TO_INSTALL f90gioh4 ${LIBS_TO_INSTALL})
#set(F90GIO_LIBS ${F90GIO_LIBS} $ENV{H4_LIBS})
endif()

# build f90gioh5
if (HAS_H5)
#include_directories($ENV{H5_INCLUDE})
    add_library(f90gioh5 mod_f90gioh5.f90)
    target_include_directories(f90gioh5 PUBLIC "$<BUILD_INTERFACE:$ENV{H5_INCLUDE}>")
    
    set(F90GIO_SRCS mod_f90gioh5.f90 ${F90GIO_SRCS})
    set(LIBS_TO_INSTALL f90gioh5 ${LIBS_TO_INSTALL})
#set(F90GIO_LIBS ${F90GIO_LIBS} $ENV{H5_LIBS})
endif()

# build f90gio
add_library(f90gio mod_f90gio.f90 ${F90GIO_SRCS})
target_compile_definitions(f90gio PUBLIC ${FPPS_FLAGS})
target_include_directories(f90gio PUBLIC "$<BUILD_INTERFACE:${F90GIO_INCLUDE}>")
#add_compile_definitions(${FPPS_FLAGS})
set(LIBS_TO_INSTALL f90gio ${LIBS_TO_INSTALL})

# install
foreach (lib_to_install ${LIBS_TO_INSTALL})
    install(TARGETS ${lib_to_install}
            LIBRARY DESTINATION lib    
            ARCHIVE DESTINATION lib)
    install(FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/mod_${lib_to_install}.mod 
            DESTINATION include)
endforeach()
