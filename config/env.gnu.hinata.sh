##/bin/bash -xe

FC="mpif90"
#FC_CFLAGS="-fp-model precise -O2"
#FC_LFLAGS=""

#ARCHIVER=ar
#ARCHIEVER_FLAGS= -crvs

# config for F90GIO installation
#F90GIO_DIR=/Users/cda/Documents/work/lab/F90GIO
#F90GIO_LIB_DIR=$(F90GIO_DIR)/lib
#F90GIO_INCLUDE_DIR=$(F90GIO_DIR)/include

# config for Netcdf
export NC_INCLUDE="/usr/lib/hpc/gnu7/mpich/netcdf-fortran/4.4.4/include"
export NC_LIBS="-L/usr/lib/hpc/gnu7/mpich/netcdf-fortran/4.4.4/lib -lnetcdff -L/usr/lib/hpc/gnu7/mpich/netcdf/4.6.1/lib64 -lnetcdf -L/usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64 -lhdf5 -lnetcdf -lnetcdf"

# config for HDF4 
#export H4_INCLUDE="/Users/cda/Documents/work/libs/hdf4-4.2.13-intel/include"
#export H4_LIBS="-L/Users/cda/Documents/work/libs/zlib-1.2.11-intel/lib -L/Users/cda/Documents/work/libs/jpeg-9c-intel/lib -L/Users/cda/Documents/work/libs/hdf4-4.2.13-intel/lib -lmfhdf -ldf -ljpeg -lz"


# config for HDF5
export H5_INCLUDE="/usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/include"
export H5_LIBS="-L/usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64 /usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64/libhdf5hl_fortran.a /usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64/libhdf5_hl.a /usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64/libhdf5_fortran.a /usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64/libhdf5.a -lpthread -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64"

echo "====================================="
echo "      F90GIO configurations:"
echo
echo "NC_INCLUDE=$NC_INCLUDE"
echo "NC_LIBS=$NC_LIBS"

echo "H4_INCLUDE=$H4_INCLUDE"
echo "H4_LIBS=$H4_LIBS"

echo "H5_INCLUDE=$H5_INCLUDE"
echo "H5_LIBS=$H5_LIBS"
