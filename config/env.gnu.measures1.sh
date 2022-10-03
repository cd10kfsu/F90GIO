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
export NC_INCLUDE="/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/include/netcdf"
export NC_LIBS="-L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -lnetcdff  -lnetcdf -lm -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -lnetcdf -ljpeg -lmfhdf -ldf -ljpeg -lhdf5_hl -lhdf5 -lm -lz -lcurl -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -lsz -ljpeg -ldl -lm"

# config for HDF4 
export H4_INCLUDE=""
export H4_LIBS=`h4fc -show TESTSRC | awk -F"TESTSRC " '{print $2}'`


# config for HDF5
export H5_INCLUDE="/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/include/hdf5"
#"/opt/homebrew/Cellar/hdf5/1.13.0/include"
export H5_LIBS="-L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5hl_fortran.a /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5_hl.a /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5_fortran.a /data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib/libhdf5.a -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -L/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -lm -lsz -lz -ldl -lm -Wl,-rpath -Wl,/data2/cda/model/x86_64-pc-linux-gnu/gfortran/Linux/lib -I/data2/cda/pkg/openmpi-4.1.4-gcc12.1.0/include -pthread -I/data2/cda/pkg/openmpi-4.1.4-gcc12.1.0/lib -L/data2/cda/pkg/openmpi-4.1.4-gcc12.1.0/lib -Wl,-rpath -Wl,/data2/cda/pkg/openmpi-4.1.4-gcc12.1.0/lib -Wl,--enable-new-dtags -lmpi_usempif08 -lmpi_usempi_ignore_tkr -lmpi_mpifh -lmpi"
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
