##/bin/bash -xe

FC="gfortran"
#FC_CFLAGS="-fp-model precise -O2"
#FC_LFLAGS=""

#ARCHIVER=ar
#ARCHIEVER_FLAGS= -crvs

# config for F90GIO installation
#F90GIO_DIR=/Users/cda/Documents/work/lab/F90GIO
#F90GIO_LIB_DIR=$(F90GIO_DIR)/lib
#F90GIO_INCLUDE_DIR=$(F90GIO_DIR)/include

# config for Netcdf
export NC_INCLUDE="/usr/include"
export NC_LIBS="-L/usr/lib/x86_64-linux-gnu -lnetcdff -Wl,-Bsymbolic-functions -flto=auto -ffat-lto-objects -flto=auto -Wl,-z,relro -Wl,-z,now -lnetcdf -lnetcdf -lm"

# config for HDF4 
export H4_INCLUDE=""
#-ffile-prefix-map=/build/libhdf4-CKQJlc/libhdf4-4.2.15=. -fstack-protector-strong -fallow-argument-mismatch -O -Wl,-Bsymbolic-functions -Wl,-z,relro -Wl,-z,now -ltirpc"
#"/home/cda/Documents/work/model/v6.2.14/ESMA-Baselibs/hdf4/mfhdf/libsrc/.deps"
#"/home/cda/Documents/work/model/v6.2.14/x86_64-pc-linux-gnu/gfortran/Linux/include/hdf"
#export H4_LIBS=`h4fc -show TESTSRC | awk -F"TESTSRC" '{print $2}'| awk '{$1=$1;print}'`
export H4_LIBS=`h4fc -show TESTSRC | awk -F"TESTSRC " '{print $2}'`
#"-L/usr/lib -lmfhdf -ldf -ljpeg -lz"
#"-L/home/cda/Documents/work/model/v6.2.14/x86_64-pc-linux-gnu/gfortran/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lz"
#export H4_LIBS="-L/usr/lib -lmfhdf -ldf -ljpeg -lz"


# config for HDF5
export H5_INCLUDE="/usr/include/hdf5/serial"
export H5_LIBS="-L/usr/lib/x86_64-linux-gnu/hdf5/serial /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5hl_fortran.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5_hl.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5_fortran.a /usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5.a -lcrypto -lcurl -lpthread -lsz -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib/x86_64-linux-gnu/hdf5/serial"
#export H5_INCLUDE="/usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/include"
#export H5_LIBS="-L/usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64 /usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64/libhdf5hl_fortran.a /usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64/libhdf5_hl.a /usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64/libhdf5_fortran.a /usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64/libhdf5.a -lpthread -lz -ldl -lm -Wl,-rpath -Wl,/usr/lib/hpc/gnu7/mpich/hdf5/1.10.1/lib64"


echo "====================================="
echo "      F90GIO configurations:"
echo
echo "NC_INCLUDE=$NC_INCLUDE"
echo "NC_LIBS=$NC_LIBS"

echo "H4_INCLUDE=$H4_INCLUDE"
echo "H4_LIBS=$H4_LIBS"

echo "H5_INCLUDE=$H5_INCLUDE"
echo "H5_LIBS=$H5_LIBS"
