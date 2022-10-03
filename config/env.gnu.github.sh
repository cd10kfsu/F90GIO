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
export NC_INCLUDE=`nf-config --fflags|cut -d " " -f1 | cut -d "I" -f 2`
export NC_LIBS=`nf-config --flibs`

# config for HDF4 
export H4_INCLUDE=""
export H4_LIBS=`h4fc -show TESTSRC | awk -F"TESTSRC " '{print $2}'`


# config for HDF5
export H5_INCLUDE=`h5fc -show TESTSRC | awk -F"TESTSRC" '{print $1}' | awk -F"-I" '{print $2}'|cut -d " " -f 1`
#"/opt/homebrew/Cellar/hdf5/1.13.0/include"
export H5_LIBS=`h5fc -show TESTSRC | awk -F"TESTSRC " '{print $2}'`
#"-L/opt/homebrew/Cellar/hdf5/1.13.0/lib /opt/homebrew/Cellar/hdf5/1.13.0/lib/libhdf5hl_fortran.a /opt/homebrew/Cellar/hdf5/1.13.0/lib/libhdf5_hl.a /opt/homebrew/Cellar/hdf5/1.13.0/lib/libhdf5_fortran.a /opt/homebrew/Cellar/hdf5/1.13.0/lib/libhdf5.a -L/opt/homebrew/opt/szip/lib -lsz -lz -ldl -lm"


echo "====================================="
echo "      F90GIO configurations:"
echo 
echo "NC_INCLUDE=$NC_INCLUDE"
echo "NC_LIBS=$NC_LIBS"

echo "H4_INCLUDE=$H4_INCLUDE"
echo "H4_LIBS=$H4_LIBS"

echo "H5_INCLUDE=$H5_INCLUDE"
echo "H5_LIBS=$H5_LIBS"
