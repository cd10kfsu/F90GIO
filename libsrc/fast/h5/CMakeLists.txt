if (HAS_H5 AND BUILD_FAST_IO)
    add_library(h5io  m_h5io.f90)
    target_include_directories(h5io PUBLIC $<BUILD_INTERFACE:$ENV{H5_INCLUDE}>)

    install(TARGETS h5io
        LIBRARY DESTINATION lib 
        ARCHIVE DESTINATION lib)
    install(FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/m_h5io.mod DESTINATION include)
endif()
