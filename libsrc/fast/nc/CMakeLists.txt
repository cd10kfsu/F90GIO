if (HAS_NC AND BUILD_FAST_IO)
    set(SRCS_NCIO m_ncio.f90)
    if (HAS_NC_C) 
        set(SRCS_NCIO nc_rdatt_cstr1d.c ${SRCS_NCIO}) 
    endif()
    #add_library(ncio  ${SRCS_NCIO})
    add_library(ncio  nc_rdatt_cstr1d.c m_ncio.f90)
    target_compile_definitions(ncio PUBLIC ${FPPS_FLAGS})
    target_include_directories(ncio PUBLIC $ENV{NC_INCLUDE} $ENV{NC_C_INCLUDE})
    #target_include_directories(ncio PUBLIC $<BUILD_INTERFACE:$ENV{NC_INCLUDE}; $ENV{NC_C_INCLUDE}>)

    install(TARGETS ncio
        LIBRARY DESTINATION lib    
        ARCHIVE DESTINATION lib)
    install(FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/m_ncio.mod DESTINATION include)
endif()
