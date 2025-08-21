##/bin/bash -xe

#FC="mpif90"
#FC_CFLAGS="-fp-model precise -O2"
#FC_LFLAGS=""

#ARCHIVER=ar
#ARCHIEVER_FLAGS= -crvs

# config for F90GIO installation
#F90GIO_DIR=/Users/cda/Documents/work/lab/F90GIO
#F90GIO_LIB_DIR=$(F90GIO_DIR)/lib
#F90GIO_INCLUDE_DIR=$(F90GIO_DIR)/include

# config for NetCDF-Fortran library
export NC_INCLUDE="/opt/homebrew/Cellar/netcdf-fortran/4.6.2/include"
export NC_LIBS="-L/opt/homebrew/Cellar/netcdf-fortran/4.6.2/lib -lnetcdff"

# config for NetCDF-C library
export NC_C_INCLUDE="/opt/homebrew/Cellar/netcdf/4.9.3/include"
export NC_C_LIBS="-L/opt/homebrew/Cellar/netcdf/4.9.3/lib -lnetcdf"

# config for HDF4 
export H4_INCLUDE=""
export H4_LIBS=`h4fc -show TESTSRC | awk -F"TESTSRC " '{print $2}'`


# config for HDF5
#export H5_INCLUDE="/opt/homebrew/Cellar/hdf5/1.14.3_1/include"
export H5_INCLUDE="/opt/homebrew/include"
#export H5_LIBS="-L/opt/homebrew/Cellar/hdf5/1.14.3_1/lib -lhdf5_fortran -lhdf5"
export H5_LIBS="-L/opt/homebrew/lib -lhdf5_fortran -lhdf5"



echo "====================================="
echo "      F90GIO configurations:"
echo 
echo "NC_INCLUDE=$NC_INCLUDE"
echo "NC_LIBS=$NC_LIBS"

echo "NC_C_INCLUDE=$NC_C_INCLUDE"
echo "NC_C_LIBS=$NC_C_LIBS"

echo "H4_INCLUDE=$H4_INCLUDE"
echo "H4_LIBS=$H4_LIBS"

echo "H5_INCLUDE=$H5_INCLUDE"
echo "H5_LIBS=$H5_LIBS"
