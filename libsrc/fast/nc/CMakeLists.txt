if (HAS_NC AND BUILD_FAST_IO)
    add_library(ncio  m_ncio.f90)
    target_include_directories(ncio PUBLIC $<BUILD_INTERFACE:$ENV{NC_INCLUDE}>)

    install(TARGETS ncio
        LIBRARY DESTINATION lib    
        ARCHIVE DESTINATION lib)
    install(FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/m_ncio.mod DESTINATION include)
endif()
