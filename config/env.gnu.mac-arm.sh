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

# config for Netcdf
export NC_INCLUDE="/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include"
export NC_LIBS="-L/opt/homebrew/Cellar/netcdf-fortran/4.6.1/lib -lnetcdff -lnetcdf -lnetcdf"

# config for HDF4 
export H4_INCLUDE=""
export H4_LIBS=`h4fc -show TESTSRC | awk -F"TESTSRC " '{print $2}'`


# config for HDF5
export H5_INCLUDE="/opt/homebrew/Cellar/hdf5/1.14.3_1/include"
#"/opt/homebrew/Cellar/hdf5/1.14.3_1/include"
export H5_LIBS="-L/opt/homebrew/Cellar/hdf5/1.14.3_1/lib -lhdf5_fortran -lhdf5"
#/opt/homebrew/Cellar/hdf5/1.14.3_1/lib/libhdf5_hl_fortran.a /opt/homebrew/Cellar/hdf5/1.14.3_1/lib/libhdf5_hl.a /opt/homebrew/Cellar/hdf5/1.14.3_1/lib/libhdf5_fortran.a /opt/homebrew/Cellar/hdf5/1.14.3_1/lib/libhdf5.a "

#-L/opt/homebrew/Cellar/hdf5/1.14.3_1/lib /opt/homebrew/Cellar/hdf5/1.14.3_1/lib/libhdf5_hl_fortran.a /opt/homebrew/Cellar/hdf5/1.14.3_1/lib/libhdf5_hl.a /opt/homebrew/Cellar/hdf5/1.14.3_1/lib/libhdf5_fortran.a /opt/homebrew/Cellar/hdf5/1.14.3_1/lib/libhdf5.a /opt/homebrew/lib/libsz.a -lz -ldl -lm"


echo "====================================="
echo "      F90GIO configurations:"
echo 
echo "NC_INCLUDE=$NC_INCLUDE"
echo "NC_LIBS=$NC_LIBS"

echo "H4_INCLUDE=$H4_INCLUDE"
echo "H4_LIBS=$H4_LIBS"

echo "H5_INCLUDE=$H5_INCLUDE"
echo "H5_LIBS=$H5_LIBS"
