add_library(h5io  m_h5io.f90)
target_include_directories(h5io PUBLIC $<BUILD_INTERFACE:$ENV{H5_INCLUDE}>)
