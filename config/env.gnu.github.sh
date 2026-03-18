#!/bin/bash -e

# Configurations for gcc on github action Ubuntu 24.04.3 LTS (Noble Numbat)
# NETCDF (with netcdf-c), HDF4, HDF5 F90IO Interface will all be built

#-------------------------------------------------------------------------------
# (skip) Step 1: set HPC envs & modules

#-------------------------------------------------------------------------------
# Step 2: set F90GIO settings

# config for NetCDF Fortran library
export NC_INCLUDE=`nf-config --includedir`
export NC_LIBS=`nf-config --flibs`

# config for NetCDF-C library
export NC_C_INCLUDE=`nc-config --includedir`
export NC_C_LIBS=`nc-config --libs`

# config for HDF4 
export H4_INCLUDE=""
export H4_LIBS=`h4fc -show TESTSRC | awk -F"TESTSRC " '{print $2}'`

# config for HDF5
export H5_INCLUDE=`h5fc -show TESTSRC | awk -F"TESTSRC" '{print $1}' | awk -F"-I" '{print $2}'|cut -d " " -f 1`
export H5_LIBS=`h5fc -show TESTSRC | awk -F"TESTSRC " '{print $2}'`

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
