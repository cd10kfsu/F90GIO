##/bin/bash -xe


# Configurations for gcc-14 on Derecho
# NETCDF (with netcdf-c), HDF4, HDF5 F90IO Interface will all be built 

#-------------------------------------------------------------------------------
# Step 1: set HPC envs & modules

MODULES=(
    #"ncarenv/25.10"
    "gcc/14.3.0"
    #"openmpi/5.0.9"
    "hdf/4.2.15"
    "hdf5/1.14.6"
    "netcdf/4.9.3"
    "cmake/3.31.8"
)

module purge
export LD_LIBRARY_PATH=""

for m in ${MODULES[@]}; do
    echo "module load $m"
    module load $m
done
module list

export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/glade/u/apps/derecho/25.10/spack/opt/spack/libjpeg-turbo/3.0.4/gcc/12.5.0/qnw6/lib64:/glade/u/apps/derecho/25.10/spack/opt/spack/libszip/2.1.1/gcc/12.5.0/dwal/lib:/glade/u/apps/derecho/25.10/spack/opt/spack/netcdf/4.9.3/packages/netcdf-c/4.9.3/gcc/14.3.0/gnur/lib64:/glade/u/apps/derecho/25.10/spack/opt/spack/netcdf/4.9.3/packages/netcdf-fortran/4.6.2/gcc/14.3.0/qgi4/lib"

ulimit -s unlimited

#-------------------------------------------------------------------------------
# Step 2: set F90GIO settings

# config for NetCDF Fortran library
export NC_INCLUDE=`nf-config --includedir`
export NC_LIBS=`pkg-config --libs netcdf-fortran`  # "nf-config --flibs" includes -lnetcdf, which we don't want

# config for NetCDF-C library
export NC_C_INCLUDE=`nc-config --includedir`
export NC_C_LIBS=`nc-config --libs`

# config for HDF4
export H4_INCLUDE=""
export H4_LIBS="-L/glade/u/apps/derecho/25.10/spack/opt/spack/libtirpc/1.3.7/gcc/12.5.0/st3g/lib -L/glade/u/apps/derecho/25.10/spack/opt/spack/zlib-ng/2.2.4/gcc/12.5.0/rq2r/lib -L/glade/u/apps/derecho/25.10/spack/opt/spack/libjpeg-turbo/3.0.4/gcc/12.5.0/qnw6/lib64 -L/glade/u/apps/derecho/25.10/spack/opt/spack/libszip/2.1.1/gcc/12.5.0/dwal/lib -L/glade/u/apps/derecho/25.10/spack/opt/spack/hdf/4.2.15/gcc/14.3.0/ac2o/lib -lmfhdf -ldf -lsz -ljpeg -lz -ltirpc"   # CDA: jpeg libs under lib64, instead of lib as shown by h4fc

# config for HDF5
export H5_INCLUDE=`pkg-config --variable=includedir hdf5_fortran`
export H5_LIBS=`pkg-config --libs hdf5_fortran`

# config for F90GIO options
export CC="gcc"             # C COMPILER
export FC="gfortran"        # Fortran COMPILER

export BUILD_NC="ON"        # ON (default) if build netcdf-fortran F90GIO lib; otherwise OFF
export USE_NC_C="ON"        # ON (default) if use netcdf-c as well for reading c-string attributes ; otherwise OFF
export BUILD_H4="ON"       # ON if build hdf4 lib; otherwise OFF (default)
export BUILD_H5="ON"        # ON (default) if build hdf5 lib; otherwise OFF
export H5_VERSION_1_8="OFF" # ON if hdf5 lib has a version <=1.8; otherwise OFF (default)
export BUILD_FAST_IO="ON"   # ON (default) if use fast netcdf/hdf5 F90GIO lib; otherwise OFF


#
# no need to change lines below
#

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

echo "CC=$CC"
echo "FC=$FC"

echo "BUILD_NC=$BUILD_NC"
echo "USE_NC_C=$USE_NC_C" 
echo "BUILD_H4=$BUILD_H4"
echo "BUILD_H5=$BUILD_H5"
echo "H5_VERSION_1_8=$H5_VERSION_1_8" 
echo "BUILD_FAST_IO=$BUILD_FAST_IO" 

#-------------------------------------------------------------------------------
# Step 3: cmake for building F90GIO
echo ""
echo "cmake building command:"
echo ". $env && mkdir -p build && cd build && cmake .. -DBUILD_NC="$BUILD_NC" -DBUILD_H4="$BUILD_H4" -DBUILD_H5="$BUILD_H5" -DH5_VERSION_1_8="$H5_VERSION_1_8" -DBUILD_FAST_IO="$BUILD_FAST_IO" -DUSE_NC_C="$USE_NC_C" -DCMAKE_Fortran_COMPILER="$FC" -DCMAKE_C_COMPILER="$CC"  && make && make test && make install"
