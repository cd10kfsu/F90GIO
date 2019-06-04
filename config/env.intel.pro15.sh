##/bin/bash -xe

#FC="ifort"
#FC_CFLAGS="-fp-model precise -O2"
#FC_LFLAGS=""

#ARCHIVER=ar
#ARCHIEVER_FLAGS= -crvs

# config for F90GIO installation
#F90GIO_DIR=/Users/cda/Documents/work/lab/F90GIO
#F90GIO_LIB_DIR=$(F90GIO_DIR)/lib
#F90GIO_INCLUDE_DIR=$(F90GIO_DIR)/include

# config for Netcdf
export NC_INCLUDE="/Users/cda/Documents/work/libs/netcdf-fortran-4.4.4-intel/include"
export NC_LIBS="-L/Users/cda/Documents/work/libs/netcdf-fortran-4.4.4-intel/lib -lnetcdff -L/Users/cda/Documents/work/libs/zlib-1.2.11-intel/lib -L/Users/cda/Documents/work/libs/hdf5-1.8.19-intel/lib -L/Users/cda/Documents/work/libs/netcdf-4.6.1-intel/lib  -lz -lhdf5 -lhdf5_hl -lhdf5_fortran -lhdf5hl_fortran -lnetcdf"

# config for HDF4 
export H4_INCLUDE="/Users/cda/Documents/work/libs/hdf4-4.2.13-intel/include"
export H4_LIBS="-L/Users/cda/Documents/work/libs/zlib-1.2.11-intel/lib -L/Users/cda/Documents/work/libs/jpeg-9c-intel/lib -L/Users/cda/Documents/work/libs/hdf4-4.2.13-intel/lib -lmfhdf -ldf -ljpeg -lz"


# config for HDF5
export H5_INCLUDE="/Users/cda/Documents/work/libs/hdf5-1.8.19-intel/include"
export H5_LIBS="-L/Users/cda/Documents/work/libs/hdf5-1.8.19-intel/lib /Users/cda/Documents/work/libs/hdf5-1.8.19-intel/lib/libhdf5hl_fortran.a /Users/cda/Documents/work/libs/hdf5-1.8.19-intel/lib/libhdf5_hl.a /Users/cda/Documents/work/libs/hdf5-1.8.19-intel/lib/libhdf5_fortran.a /Users/cda/Documents/work/libs/hdf5-1.8.19-intel/lib/libhdf5.a -L/Users/cda/Documents/work/libs/zlib-1.2.11-intel/lib -lz -ldl -lm"
